
#' Create time-contiguous validation datasets for model evaluation
#'
#' Flexibly ceate blocks of time-contiguous validation datasets to assess the likely forecast accuracy
#' of trained models in at various times in the past. These validation datasets are similar to
#' the outer loop of a nested cross-validation model training setup.
#'
#' @param lagged_df An object of class 'lagged_df' or'grouped_lagged_df' from \code{\link{create_lagged_df}}.
#' @param window_length An integer that defines the length of the contiguous validation dataset in dataset rows/dates.
#' If dates were given in \code{create_lagged_df}, the validation window is 'window_length' * 'date frequency' in calendar time.
#' Setting \code{window_length = 0} trains the model on the entire dataset--used for re-training after examining
#' the cross-validation results.
#' @param window_start An optional index or date identifying the row/date to start creating contiguous validation datasets.
#' @param window_stop An optional index or date identifying the row to stop creating contiguous validation datasets.
#' @param skip An integer giving a fixed number of dataset rows/time to skip between validation datasets. If dates were given
#' in \code{create_lagged_df}, the time between validation windows is \code{skip} * 'date frequency'.
#' @param include_partial_window Keep validation datasets that are shorter than \code{window_length}.
#' @return A'windows' S3 object: A list of data.frame(s), one per horizon, giving the indices for the validation datasets.
#'
#' @section Methods and related functions:
#'
#' The output of of \code{create_windows} is passed into
#'
#' \itemize{
#'   \item \code{\link{train_model}}
#' }
#'
#' and has the following generic S3 methods
#'
#' \itemize{
#'   \item \code{\link{plot}}
#' }
#' @example /R/examples/example_create_windows.R
#' @importFrom rlang !!
#' @importFrom rlang !!!
#' @export
create_windows <- function(lagged_df, window_length = 12,
                           window_start = NULL, window_stop = NULL, skip = 0,
                           include_partial_window = TRUE) {

  data <- lagged_df

  if(!methods::is(data, "lagged_df")) {
    stop("This function takes an object of class 'lagged_df' as input. Run create_lagged_df() first.")
  }

  if(missing(window_length)) {
    stop("Define a 'window_length' >= 0 for the validation dataset(s).")
  }

  window_length <- as.numeric(window_length)

  outcome_cols <- attributes(data)$outcome_cols
  outcome_names <- attributes(data)$outcome_names
  row_names <- attributes(data)$row_indices
  date_indices <- attributes(data)$date_indices
  frequency <- attributes(data)$frequency
  data_start <- attributes(data)$data_start
  data_stop <- attributes(data)$data_stop

  window_start <- if (is.null(window_start)) {data_start} else {window_start}
  window_stop <- if (is.null(window_stop)) {data_stop} else {window_stop}

  if (!is.null(date_indices) && !methods::is(window_start, "Date")) {
    stop("Dates were provided with the input dataset created with `create_lagged_df()`;
         Enter a window start date as a length-1 vector of class `Date`.")
  }

  if (!is.null(date_indices) && !methods::is(window_stop, "Date")) {
    stop("Dates were provided with the input dataset created with `create_lagged_df()`;
         Enter a window stop date as a length-1 vector of class `Date`.")
  }

  if(!window_stop >= data_stop) {
    stop(paste0("The end of all validation windows needs to occur on or before row/date ", data_stop, " which is the end of the dataset."))
  }

  # Creating windows with a non-date index.
  if (is.null(date_indices)) {

    # If the window_length is 0 there are no nested cross-validation windows needed.
    if (window_length == 0) {

      window_matrices <- data.frame("start" = window_start, "stop" = window_stop, "window_length" = window_length)

    } else {

      # Create a vector of indices that give the last index/row for a full validation window.
      max_train_indices <- window_stop - window_length + 1

      window_matrices <- purrr::map2(max_train_indices, window_length, function(max_train_index, window_len) {

        start_index <- 1:max_train_index
        stop_index <- 1:max_train_index + window_len - 1

        window_matrix <- cbind("start" = start_index, "stop" = stop_index, "window_length" = window_len)
        window_matrix <- window_matrix[seq(window_start, nrow(window_matrix), window_len + skip), , drop = FALSE]

        # The partial window is an additional row that represents the final, partial validation window.
        if (isTRUE(include_partial_window)) {
          window_matrix_partial <- window_matrix[nrow(window_matrix), , drop = FALSE]
          window_matrix_partial[, "start"] <- window_matrix_partial[, "stop"] + 1 + skip
          window_matrix_partial[, "stop"] <- window_stop
          # Cleaning up windows that exceed the number of rows in the dataframe due to the 'skip' parameter.
          if (window_matrix_partial[, "start"] <= window_matrix_partial[, "stop"]) {
            window_matrix <- rbind(window_matrix, window_matrix_partial)
            rownames(window_matrix) <- NULL
          }
        }
        window_matrix
      })

      window_matrices <- as.data.frame(window_matrices[[1]])

    }  # End index-based cross-validation windows.

  } else {  # Creating cross-validation windows with dates.

    # If the window_length is 0 there are no nested cross-validation windows needed.
    if (window_length == 0) {

      window_matrices <- data.frame("start" = window_start, "stop" = window_stop, "window_length" = window_length)

    } else {

      all_dates <- seq(window_start, window_stop, frequency)

      start_dates <- all_dates[seq(1, length(all_dates), by = window_length + skip)]

      stop_dates <- all_dates[which(all_dates %in% start_dates) + window_length - 1]

      window_matrices <- data.frame("start" = start_dates, "stop" = stop_dates, "window_length" = window_length)

      if (isTRUE(include_partial_window)) {
        window_matrices$stop[is.na(window_matrices$stop)] <- max(date_indices, na.rm = TRUE)
      } else {
        window_matrices <- window_matrices[complete.cases(window_matrices), ]
      }
    }
    }  # End date-based windows.

  attributes(window_matrices) <- unlist(list(attributes(window_matrices),  # Keep the data.frame's attributes.
                                             list("skip" = skip,
                                                  "outcome_cols" = outcome_cols,
                                                  "outcome_names" = outcome_names)), recursive = FALSE)

  class(window_matrices) <- c("windows", class(window_matrices))

  return(window_matrices)
}
#------------------------------------------------------------------------------

#' Plot validation datasets
#'
#' Plot validation datasets across time.
#'
#' @param x An object of class 'windows' from \code{create_windows}.
#' @param lagged_df An object of class 'lagged_df' from \code{create_lagged_df}.
#' @param show_labels Boolean. Show validation dataset IDs on the plot.
#' @param group_filter A string for filtering plot results for grouped time-series (e.g., "group_col_1 == 'A'").
#' The string is passed internally to \code{dplyr::filter}.
#' @param ... Arguments passed to \code{base::plot}
#' @return A plot of the outer-loop nested cross-validation windows.
#' @example /R/examples/example_plot_windows.R
#' @export
plot.windows <- function(x, lagged_df, show_labels = TRUE, group_filter = NULL, ...) {

  data <- lagged_df

  if (!methods::is(windows, "windows")) {
    stop("The 'windows' argument takes an object of class 'windows' as input. Run window_skip() first.")
  }

  if (!methods::is(data, "lagged_df")) {
    stop("The 'data' argument takes an object of class 'lagged_df' as input. Run create_lagged_df() first.")
  }

  outcome_cols <- attributes(data)$outcome_cols
  outcome_names <- attributes(data)$outcome_names
  row_names <- as.numeric(row.names(data[[1]]))  # To-do: for index-based lagged_dfs, adjust for first row differences across variable lookbacks.
  n_outcomes <- length(outcome_cols)
  date_indices <- attributes(data)$date_indices
  groups <- attributes(data)$groups
  skip <- attributes(windows)$skip

  # If there are multiple horizons in the lagged_df, select the first dataset and columns for plotting.
  data_plot <- dplyr::select(data[[1]], outcome_names, groups)

  if (is.null(date_indices)) {  # index-based x-axis in plot.

    data_plot$index <- row_names

  } else {  # date-based x-axis in plot.

    if (is.null(groups)) {

      data_plot$index <- date_indices[row_names]  # Removes the dates from the beginning of a dataset; right now dates are for all dates given and indices are not.

    } else {

      data_plot$index <- date_indices
    }
  }

  if (!is.null(group_filter)) {

    data_plot <- dplyr::filter(data_plot, eval(parse(text = group_filter)))
  }
  #----------------------------------------------------------------------------

  # Create different line segments in ggplot with `color = ggplot_color_group`.
  data_plot$ggplot_color_group <- apply(data_plot[, groups, drop = FALSE], 1, function(x) {paste(x, collapse = "-")})

  data_windows <- windows
  data_windows$window <- 1:nrow(data_windows)

  # Find the x and y coordinates for the plot labels.
  if (isTRUE(show_labels) || missing(show_labels)) {

    data_plot_group <- data_windows %>%
      dplyr::group_by(window_length, window) %>%
      dplyr::summarise("index" = start + ((stop - start) / 2))  # Window midpoint for plot label.
    data_plot_group$label_height <- ifelse(min(data_plot[, 1:n_outcomes], na.rm = TRUE) < 0,
                                           (max(data_plot[, 1:n_outcomes], na.rm = TRUE) - abs(min(data_plot[, 1:n_outcomes], na.rm = TRUE))) / 2,
                                           (max(data_plot[, 1:n_outcomes], na.rm = TRUE) + abs(min(data_plot[, 1:n_outcomes], na.rm = TRUE))) / 2)
    data_plot_group <- data_plot_group[!is.na(data_plot_group$window), ]
  }
  #----------------------------------------------------------------------------
  # Fill in date gaps with NAs so ggplot doesn't connect line segments where there were no entries recorded.
  if (!is.null(groups)) {

    data_plot_template <- expand.grid("index" = seq(min(date_indices, na.rm = TRUE), max(date_indices, na.rm = TRUE), by = attributes(data)$frequency),
                                      "ggplot_color_group" = unique(data_plot$ggplot_color_group),
                                      stringsAsFactors = FALSE)

    data_plot <- dplyr::left_join(data_plot_template, data_plot, by = c("index", "ggplot_color_group"))

    data_plot$ggplot_color_group <- ordered(data_plot$ggplot_color_group)

    # Create a dataset of points for those instances where there the outcomes are NA before and after a given instance.
    # Points are needed because ggplot will not plot a 1-instance geom_line().
    data_plot_point <- data_plot %>%
      dplyr::group_by(ggplot_color_group) %>%
      dplyr::mutate("lag" = dplyr::lag(eval(parse(text = outcome_names)), 1),
                    "lead" = dplyr::lead(eval(parse(text = outcome_names)), 1)) %>%
      dplyr::filter(is.na(lag) & is.na(lead))

    data_plot_point$ggplot_color_group <- factor(data_plot_point$ggplot_color_group, ordered = TRUE, levels(data_plot$ggplot_color_group))
  } else {
    data_plot$ggplot_color_group <- ordered(data_plot$ggplot_color_group)
  }
  #----------------------------------------------------------------------------

  p <- ggplot()
  p <- p + geom_rect(data = windows, aes(xmin = start, xmax = stop,
                                         ymin = -Inf, ymax = Inf), fill = "grey85", show.legend = FALSE)
  p <- p + geom_line(data = data_plot, aes(x = index, y = eval(parse(text = outcome_names)), color = ggplot_color_group),
                     size = 1.05)

  if (!is.null(groups)) {
    if (nrow(data_plot_point) >= 1) {
      p <- p + geom_point(data = data_plot_point, aes(x = index, y = eval(parse(text = outcome_names)), color = ggplot_color_group),
                          show.legend = FALSE)
    }
  }

  if (isTRUE(show_labels) || missing(show_labels)) {
    p <- p + geom_label(data = data_plot_group, aes(x = index, y = label_height, label = window),
                        color = "black", size = 4)
  }

  p <- p + theme_bw()

  if (is.null(groups)) {
    p <- p + theme(legend.position = "none")
  }

  p <- p + xlab("Dataset index / row") + ylab("Outcome") + labs(color = "Groups") + ggtitle("Validation Windows")
  return(p)
}
