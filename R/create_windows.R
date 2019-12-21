#' Create time-contiguous validation datasets for model evaluation
#'
#' Flexibly ceate blocks of time-contiguous validation datasets to assess forecast accuracy
#' of trained models at various times in the past. These validation datasets are similar to
#' the outer loop of a nested cross-validation model training setup.
#'
#' @param lagged_df An object of class 'lagged_df' or 'grouped_lagged_df' from \code{\link{create_lagged_df}}.
#' @param window_length An integer that defines the length of the contiguous validation dataset in dataset rows/dates.
#' If dates were given in \code{create_lagged_df()}, the validation window is 'window_length' * 'date frequency' in calendar time.
#' Setting \code{window_length = 0} trains the model on (a) the entire dataset or (b) between a single \code{window_start} and
#' \code{window_stop} value. Specifying multiple \code{window_start} and \code{window_stop} values with vectors of
#' length > 1 overrides \code{window_length}.
#' @param window_start Optional. A row index or date identifying the row/date to start creating contiguous validation datasets. A
#' vector of start rows/dates can be supplied for greater control. The length and order of \code{window_start} should match \code{window_stop}.
#' If \code{length(window_start) > 1}, \code{window_length}, \code{skip}, and \code{include_partial_window} are ignored.
#' @param window_stop Optional. An index or date identifying the row/date to stop creating contiguous validation datasets. A
#' vector of start rows/dates can be supplied for greater control. The length and order of \code{window_stop} should match \code{window_start}.
#' If \code{length(window_stop) > 1}, \code{window_length}, \code{skip}, and \code{include_partial_window} are ignored.
#' @param skip An integer giving a fixed number of dataset rows/dates to skip between validation datasets. If dates were given
#' in \code{create_lagged_df()}, the time between validation windows is \code{skip} * 'date frequency'.
#' @param include_partial_window Boolean. If \code{TRUE}, keep validation datasets that are shorter than \code{window_length}.
#' @return An S3 object of class 'windows': A data.frame giving the indices for the validation datasets.
#'
#' @section Methods and related functions:
#'
#' The output of \code{create_windows()} is passed into
#'
#' \itemize{
#'   \item \code{\link{train_model}}
#' }
#'
#' and has the following generic S3 methods
#'
#' \itemize{
#'   \item \code{\link[=plot.windows]{plot}}
#' }
#' @example /R/examples/example_create_windows.R
#' @importFrom rlang !!
#' @importFrom rlang !!!
#' @export
create_windows <- function(lagged_df, window_length = 12L,
                           window_start = NULL, window_stop = NULL, skip = 0,
                           include_partial_window = TRUE) {

  if (!methods::is(lagged_df, "lagged_df")) {
    stop("This function takes an object of class 'lagged_df' as input. Run create_lagged_df() first.")
  }

  data <- lagged_df
  rm(lagged_df)

  if (length(window_length) != 1 || !methods::is(window_length, c("numeric"))) {
    stop("The 'window_length' argument needs to be a positive integer of length 1.")
  }

  outcome_col <- attributes(data)$outcome_col
  outcome_names <- attributes(data)$outcome_names
  date_indices <- attributes(data)$date_indices
  frequency <- attributes(data)$frequency
  data_start <- attributes(data)$data_start
  data_stop <- attributes(data)$data_stop

  window_start <- if (is.null(window_start)) {data_start} else {window_start}
  window_stop <- if (is.null(window_stop)) {data_stop} else {window_stop}

  if (!is.null(date_indices) && !methods::is(window_start, "Date") && !methods::is(window_start, "POSIXt")) {
    stop("Dates were provided with the input dataset created with 'create_lagged_df()';
         Enter a vector of window start dates of class 'Date' or 'POSIXt'.")
  }

  if (!is.null(date_indices) && !methods::is(window_stop, "Date") && !methods::is(window_stop, "POSIXt")) {
    stop("Dates were provided with the input dataset created with 'create_lagged_df()';
         Enter a vector of window stop dates of class 'Date' or 'POSIXt'.")
  }

  if (length(window_start) == 1 && length(window_stop) == 1) {  # A single start and stop date.

    if (!window_start >= data_start) {
      stop(paste0("The start of all validation windows needs to occur on or after row/date ", data_start, " which is the beginning of the dataset."))
    }

    if (!window_stop <= data_stop) {
      stop(paste0("The end of all validation windows needs to occur on or before row/date ", data_stop, " which is the end of the dataset."))
    }

    if (is.null(date_indices) && window_length > (as.numeric(window_stop - window_start) + 1)) {
      stop(paste0("The window length is wider than 'window_stop - window_start'. Set 'window_length = 0' to get 1 validation window for this period."))
    }

  } else {  # A vector of multiple start and stop dates.

    if (length(window_start) != length(window_stop)) {
      stop(paste0("length(window_start) != length(window_stop); each validation window needs a start and stop date."))
    }

    if (!all(window_stop >= window_start)) {
      stop(paste0("'window_stop' needs to be greater than 'window_start' for all validation windows"))
    }
  }

  # Creating windows with a non-date index.
  if (is.null(date_indices)) {

    if (length(window_start) == 1 && length(window_stop) == 1) {  # A single start and stop date.

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
      }

      } else { # Multiple start and stop indices.

        window_matrices <- data.frame("start" = window_start, "stop" = window_stop, "window_length" = "custom")

      }  # End index-based cross-validation windows.

    } else {  # Creating cross-validation windows with dates.

    if (length(window_start) == 1 && length(window_stop) == 1) {  # A single start and stop date.

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
      }  # End date-based windows with a single start and stop date.

    } else {  # Multiple start and stop dates.

      window_matrices <- data.frame("start" = window_start, "stop" = window_stop, "window_length" = "custom")
    }  # End date-based windows with a multiple start and stop dates.
  }  # End date-based windows.

  attributes(window_matrices) <- unlist(list(attributes(window_matrices),  # Keep the data.frame's attributes.
                                             list("skip" = skip,
                                                  "outcome_col" = outcome_col,
                                                  "outcome_names" = outcome_names)), recursive = FALSE)

  class(window_matrices) <- c("windows", class(window_matrices))

  return(window_matrices)
}
#------------------------------------------------------------------------------

#' Plot validation datasets
#'
#' Plot validation datasets across time.
#'
#' @param x An object of class 'windows' from \code{create_windows()}.
#' @param lagged_df An object of class 'lagged_df' from \code{create_lagged_df()}.
#' @param show_labels Boolean. If \code{TRUE}, show validation dataset IDs on the plot.
#' @param group_filter Optional. A string for filtering plot results for grouped time series (e.g., \code{"group_col_1 == 'A'"}).
#' This string is passed to \code{dplyr::filter()} internally.
#' @param ... Not used.
#' @return A plot of the outer-loop nested cross-validation windows of class 'ggplot'.
#' @example /R/examples/example_plot_windows.R
#' @export
plot.windows <- function(x, lagged_df, show_labels = TRUE, group_filter = NULL, ...) { # nocov start

  if (!methods::is(x, "windows")) {
    stop("The 'x' argument takes an object of class 'windows' as input. Run create_windows() first.")
  }

  if (!methods::is(lagged_df, "lagged_df")) {
    stop("The 'lagged_df' argument takes an object of class 'lagged_df' as input. Run create_lagged_df() first.")
  }

  windows <- x
  rm(x)

  data <- lagged_df
  rm(lagged_df)

  outcome_col <- attributes(data)$outcome_col
  outcome_names <- attributes(data)$outcome_names
  outcome_levels <- attributes(data)$outcome_levels
  row_indices <- attributes(data)$row_indices
  date_indices <- attributes(data)$date_indices
  groups <- attributes(data)$groups

  # If there are multiple horizons in the lagged_df, select the first dataset and columns for plotting.
  data_plot <- dplyr::select(data[[1]], outcome_names, groups)

  if (is.null(date_indices)) {  # Index-based x-axis in plot.

    data_plot$index <- row_indices

    } else {  # Date-based x-axis in plot.

      data_plot$index <- date_indices
    }

  if (!is.null(group_filter)) {

    data_plot <- dplyr::filter(data_plot, eval(parse(text = group_filter)))
  }
  #----------------------------------------------------------------------------

  # Create different line segments in ggplot with `color = ggplot_color_group`.
  data_plot$ggplot_color_group <- apply(data_plot[, groups, drop = FALSE], 1, function(x) {paste(x, collapse = "-")})

  windows$window <- 1:nrow(windows)

  # Find the x and y coordinates for the plot labels. The labels for factor outcomes are calculated
  # in the gglot2 code.
  if (isTRUE(show_labels) || missing(show_labels)) {

    data_plot_group <- windows %>%
      dplyr::group_by(.data$window_length, .data$window) %>%
      dplyr::summarise("index" = .data$start + ((.data$stop - .data$start) / 2))  # Window midpoint for plot label.

    if (is.null(outcome_levels)) {  # Numeric outcome.

      data_plot_group$label_height <- ifelse(min(data_plot[, 1], na.rm = TRUE) < 0,
                                             (max(data_plot[, 1], na.rm = TRUE) - base::abs(min(data_plot[, 1], na.rm = TRUE))) / 2,
                                             (max(data_plot[, 1], na.rm = TRUE) + base::abs(min(data_plot[, 1], na.rm = TRUE))) / 2)
      }

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
      dplyr::group_by(.data$ggplot_color_group) %>%
      dplyr::mutate("lag" = dplyr::lag(eval(parse(text = outcome_names)), 1),
                    "lead" = dplyr::lead(eval(parse(text = outcome_names)), 1)) %>%
      dplyr::filter(is.na(.data$lag) & is.na(.data$lead))

    data_plot_point$ggplot_color_group <- factor(data_plot_point$ggplot_color_group, ordered = TRUE, levels(data_plot$ggplot_color_group))

  } else {

    data_plot$ggplot_color_group <- ordered(data_plot$ggplot_color_group)
  }
  #----------------------------------------------------------------------------

  p <- ggplot()
  p <- p + geom_rect(data = windows, aes(xmin = .data$start, xmax = .data$stop,
                                         ymin = -Inf, ymax = Inf), fill = "grey85", show.legend = FALSE)

  if (is.null(outcome_levels)) {  # Numeric outcome.

    p <- p + geom_line(data = data_plot, aes(x = .data$index, y = eval(parse(text = outcome_names)),
                                             color = .data$ggplot_color_group), size = 1.05)

  } else {  # Factor outcome.

    p <- p + geom_tile(data = data_plot, aes(x = .data$index, y = ordered(.data$ggplot_color_group),
                                             fill = ordered(eval(parse(text = outcome_names)))))
  }

  if (!is.null(groups) && is.null(outcome_levels)) {  # Numeric outcome with groups.
    if (nrow(data_plot_point) >= 1) {
      p <- p + geom_point(data = data_plot_point, aes(x = .data$index, y = eval(parse(text = outcome_names)),
                                                      color = .data$ggplot_color_group), show.legend = FALSE)
    }
  }

  if (isTRUE(show_labels) || missing(show_labels)) {

    if (is.null(outcome_levels)) {  # Numeric outcome.

      p <- p + geom_label(data = data_plot_group, aes(x = .data$index, y = .data$label_height,
                                                      label = .data$window), color = "black", size = 4)

    } else {  # Factor outcome.

      data_plot_group$label_height <- ordered(levels(ordered(data_plot$ggplot_color_group))[1])

      p <- p + geom_label(data = data_plot_group, aes(x = .data$index, y = .data$label_height,
                                                      label = .data$window), color = "black", size = 4)
    }
  }

  p <- p + theme_bw()

  if (is.null(groups) && is.null(outcome_levels)) {  # Numeric outcome without groups.

    p <- p + theme(legend.position = "none")
  }

  if (is.null(outcome_levels)) {  # Numeric outcome

    p <- p + xlab("Dataset index") + ylab("Outcome") + labs(color = "Groups") + ggtitle("Validation Windows")

    } else {  # Factor outcome.

    if (length(levels(data_plot$ggplot_color_group)) == 1) {

      p <- p + xlab("Dataset index") + ylab("Outcome") + labs(fill = "Outcome") + ggtitle("Validation Windows")

      } else {

      p <- p + xlab("Dataset index") + ylab("Groups") + labs(fill = "Outcome") + ggtitle("Validation Windows")
    }
  }
  return(suppressWarnings(p))
} # nocov end
