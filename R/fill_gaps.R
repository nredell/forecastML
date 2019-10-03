#' Prepare a dataset for modeling by filling in temporal gaps in data collection
#'
#' In order to create a modeling dataset with feature lags that are temporally correct, the entry
#' function in \code{forecastML}, \code{\link{create_lagged_df}}, needs evenly-spaced time-series with no
#' gaps in data collection. \code{fill_gaps()} can help here.
#' This function takes a \code{data.frame} with (a) dates, (b) the outcome being forecasted, and, optionally,
#' (c) dynamic features that change through time, (d) group columns for multiple time-series modeling,
#' and (e) static or non-dynamic features for multiple time-series modeling and returns a \code{data.frame}
#' with rows evenly spaced in time. Specifically, this function adds rows to the input dataset
#' while filling in (a) dates, (b) grouping information, and (c) static features. The (a) outcome and (b)
#' dynamic features will be \code{NA} for any missing time periods; these \code{NA} values can be left
#' as-is, user-imputed, or removed from modeling in the user-supplied modeling wrapper function for \code{\link{train_model}}.
#'
#' @param data A data.frame with, minimally, dates and the outcome being forecasted.
#' @param date_col The column index--an integer--of the date index. This column should have class 'Date'.
#' @param frequency Date frequency. A string taking the same input as \code{base::seq.Date(..., by = "frequency")} e.g., '1 month', '7 days', '10 years' etc.
#' The highest frequency supported at present is '1 day'.
#' @param groups A character vector or column names that identify the groups/hierarchies when multiple time-series are present. These columns are used as model predictors but are not lagged.
#' Note that combining feature lags with grouped time-series will result in \code{NA} values throughout the data.
#' @param static_features For grouped time series only (optional). A character vector of column names that identify features that do not change through time.
#' These columns are expected to be used as model features but are not lagged (e.g., a ZIP code column). The most recent values for each
#' static feature for each group are used to fill in the resulting missing data in static features when new rows are
#' added to the dataset to fill gaps in data collection.
#' @return An object of class 'data.frame': The returned data.frame has the same number of columns and column order but
#' with additional rows to account for gaps in data collection. For grouped data, any new rows added to the returned data.frame will appear
#' between the minimum--or oldest--date for that group and the maximum--or most recent--date across all groups. If the user-supplied
#' forecasting algorithm(s) cannot handle missing outcome values or missing dynamic features, these should either be
#' imputed prior to \code{create_lagged_df()} or filtered out in the user-supplied modeling function for \code{\link{train_model}}.
#'
#' @section Methods and related functions:
#'
#' The output of \code{fill_gaps()} is passed into
#'
#' \itemize{
#'   \item \code{\link{create_lagged_df}}
#' }
#'
#' @example /R/examples/example_fill_gaps.R
#'
#' @export
fill_gaps <- function(data, date_col = 1, frequency, groups = NULL,
                      static_features = NULL) {

  data <- as.data.frame(data)

  if (!methods::is(data, "data.frame")) {
    stop("The 'data' argument should be an object of class 'data.frame'.")
  }

  if (length(date_col) != 1) {
    stop("The 'data_col' argument should be an integer giving the column location of the date index.")
  }

  if (!methods::is(data[, date_col], "Date")) {
    stop("The date column identified by the 'data_col' argument should be an object of class 'Date'.")
  }

  if (any(is.na(data[, date_col, drop = TRUE]))) {
    stop("The date column identified by the 'data_col' argument has missing or 'NA' dates; remove them prior to running this function.")
  }

  if (missing(frequency)) {
    stop("The 'frequency' argument is required to set the expected frequency of data collection e.g., '1 day', '3 months', '10 years' etc.;
         see base::seq.Date() for valid date frequencies.")
  }

  if (is.null(groups) && !is.null(static_features)) {
    stop("Static features--those that do not change through time--should only be modeled with grouped time-series.")
  }

  # Used to re-order the returned dataset to match the input dataset.
  col_names <- names(data)

  date_name <- names(data)[date_col]

  if (is.null(groups)) {

    data <- data %>%
      dplyr::arrange(eval(parse(text = date_name)))

  } else {

    data <- data %>%
      dplyr::arrange(eval(parse(text = groups)), eval(parse(text = date_name)))
  }

  # Create a merge template giving the date bounds for non-grouped or grouped data.
  data_template <- data %>%
    dplyr::group_by_at(dplyr::vars(groups)) %>%
    dplyr::summarize("date_min" = min(eval(parse(text = date_name)), na.rm = TRUE)) %>%
    dplyr::ungroup()

  data_template$date_max <- max(data[, date_name, drop = TRUE], na.rm = TRUE)

  if (!is.null(static_features)) {

    # Create a merge template to fill in the static features when left_join()-ing. The assumption
    # here is that the last row or most recent observation has a non-missing static feature. This behavior
    # guards against static features that are mostly static or have not changed recently but that may
    # have changed in the distant past. The user will be made aware of this in the help docs.
    data_static <- data %>%
      dplyr::group_by_at(dplyr::vars(groups)) %>%
      dplyr::mutate("date_max" = max(eval(parse(text = date_name)), na.rm = TRUE)) %>%
      dplyr::filter(eval(parse(text = date_name)) == .data$date_max) %>%
      dplyr::select_at(dplyr::vars(groups, static_features))
  }

    data_template_list <- vector("list", nrow(data_template))
    data_template_list <- lapply(seq_along(data_template_list), function(i) {

    # Make a contiguous vector of dates.
    date_seq <- data.frame(seq(data_template[i, "date_min", drop = TRUE],
                               data_template[i, "date_max", drop = TRUE],
                               by = frequency))

    if (!is.null(groups)) {

      # Make a dataset with the grouping columns that's the same length as the date sequence.
      data_groups <- data_template[i, , drop = FALSE]
      data_groups <- dplyr::select(data_groups, groups)
      data_groups <- data_groups[rep(1, nrow(date_seq)), , drop = FALSE]

      data_template_list <- dplyr::bind_cols(date_seq, data_groups)
      names(data_template_list) <- c(date_name, groups)

    } else {

      data_template_list <- date_seq
      names(data_template_list) <- date_name
    }

    data_template_list
  })

  data_template <- dplyr::bind_rows(data_template_list)

  if (!is.null(static_features)) {

    # If there are static features, merge these to the date and groups template
    # with the gaps now removed.
    data_template <- dplyr::left_join(data_template, data_static, by = groups)

    # Now that the dates, groups, and any static features are date-complete with
    # no gaps, we can remove static features from the input dataset followed by
    # a left join with the data template.
    data <- dplyr::select(data, -static_features)
  }

  data_out <- dplyr::left_join(data_template, data, by = c(date_name, groups))

  if (is.null(groups)) {

    data_out <- data_out %>%
      dplyr::arrange(eval(parse(text = date_name)))

  } else {

    data_out <- data_out %>%
      dplyr::arrange(eval(parse(text = groups)), eval(parse(text = date_name)))
  }

  # Re-order the complete dataset to have the same column order as the input dataset.
  data_out <- dplyr::select(data_out, col_names)

  data_out <- as.data.frame(data_out)

  return(data_out)
}
