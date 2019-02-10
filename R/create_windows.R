
#' Create validation datasets for the outer loop of nested cross-validation
#'
#' Create blocks of contiguous validation windows to assess the likely forecast accuracy
#' of various models.
#'
#' @param lagged_df An object of class 'lagged_df' from create_lagged_df().
#' @param window_length An integer that defines the length of the contiguous validation dataset in dataset rows.
#' Setting 'window_length = 0' trains the model on the entire dataset--useful for re-training after examining
#' the nested cross-validation results.
#' @param window_start An optional index identifying the row to start creating contiguous validation datasets.
#' @param window_stop An optional index identifying the row to stop creating contiguous validation datasets.
#' @param skip An integer giving a fixed number of dataset rows to skip between validation datasets.
#' @param include_partial_window Keep validation datasets that are shorter than window_length.
#' @return A'windows' object: A list of matrices, one per horizon, giving the indices for the validation datasets.
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

  window_length <- as.numeric(window_length[1])

  outcome_cols <- attributes(data)$outcome_cols
  outcome_names <- attributes(data)$outcome_names
  row_names <- attributes(data)$row_indices
  date_indices <- attributes(data)$date_indices
  data_start <- attributes(data)$data_start
  data_stop <- attributes(data)$data_stop

  window_start <- if (is.null(window_start)) {data_start} else {window_start}
  window_stop <- if (is.null(window_stop)) {data_stop} else {window_stop}

  if(!window_stop >= data_stop) {
    stop(paste0("The end of all validation windows needs to occur on or before row index ", data_stop, " which is the end of the dataset."))
    }

  if(!window_stop - window_start + 1 >= max(window_length)) {
    stop(paste0("The length of at least one user-defined validation window, ", window_stop - window_start + 1, ", must be >= the longest 'window_length' of ", max(window_length), "."))
  }

  # If the window_length is 0 there are no nested cross-validation windows needed.
  if (window_length == 0) {
    window_matrices <- list(as.matrix(cbind("start" = data_start, "stop" = data_stop,
                                            "window_length" = window_length), nrow = 1))
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
  }

  attributes(window_matrices) <- list("skip" = skip,
                                      "outcome_cols" = outcome_cols,
                                      "outcome_names" = outcome_names)

  class(window_matrices) <- c("windows", class(window_matrices))

  return(window_matrices)
}
#------------------------------------------------------------------------------

#' Plot validation datasets
#'
#' Plot validation datasets across time.
#'
#' @param windows An object of class 'windows' from create_windows().
#' @param data An object of class 'lagged_df' from create_lagged_df().
#' @param show_labels Show validation dataset IDs on the plot.
#' @return A plot of the outer-loop nested cross-validation windows.
#' @example /R/examples/example_plot_windows.R
#' @export
plot.windows <- function(windows, data, show_labels = TRUE) {

  if(!methods::is(windows, "windows")) {
    stop("The 'windows' argument takes an object of class 'windows' as input. Run window_skip() first.")
  }

  if(!methods::is(data, "lagged_df")) {
    stop("The 'data' argument takes an object of class 'lagged_df' as input. Run create_lagged_df() first.")
  }

  outcome_cols <- attributes(data)$outcome_cols
  outcome_names <- attributes(data)$outcome_names
  row_names <- as.numeric(row.names(data[[1]]))
  n_outcomes <- length(outcome_cols)

  # Create a dataset for a line plot overlay of the time-series on the validation window plot.
  data_line <- data[[1]][, 1:n_outcomes , drop = FALSE]
  data_line$index <- row_names
  data_line <- data_line[rep(1:nrow(data_line), each = length(windows)), ]

  skip <- attributes(windows)$skip

  data_plot_window <- lapply(windows, function(x){as.data.frame(x)})
  data_plot_window <- dplyr::bind_rows(data_plot_window)
  data_plot_window$window_length_partial <- with(data_plot_window, stop - start + 1)
  data_plot_window$window <- 1:nrow(data_plot_window)

  data_plot_line <- data.frame("index" = unlist(purrr::map2(data_plot_window$start, data_plot_window$stop, function(x, y) {
    seq(x, y, 1)
  })))
  data_plot_line$window_length <- rep(data_plot_window$window_length, data_plot_window$window_length_partial)
  data_plot_line$window <- rep(data_plot_window$window, data_plot_window$window_length_partial)
  data_plot_window$window_length_partial <- NULL

  data_line$window_length <- rep(unique(data_plot_line$window_length), nrow(data_line) / length(windows))

  data_plot_line <- dplyr::full_join(data_line, data_plot_line, by = c("index", "window_length"))

  data_plot_line <- dplyr::arrange(data_plot_line, window_length, index)

  data_plot_group <- data_plot_line %>%
    dplyr::group_by(window_length, window) %>%
    dplyr::summarise("index" = mean(index))
  data_plot_group$label_height <- ifelse(min(data_line[, 1:n_outcomes], na.rm = TRUE) < 0,
                                         (max(data_line[, 1:n_outcomes], na.rm = TRUE) - abs(min(data_line[, 1:n_outcomes], na.rm = TRUE))) / 2,
                                         (max(data_line[, 1:n_outcomes], na.rm = TRUE) + abs(min(data_line[, 1:n_outcomes], na.rm = TRUE))) / 2)
  data_plot_group <- data_plot_group[!is.na(data_plot_group$window), ]

  data_plot_window$stop <- with(data_plot_window, ifelse(start == stop, stop + 1, stop))  # To plot a shaded rectangle of length 1.

  p <- ggplot()
  p <- p + geom_rect(data = data_plot_window, aes(xmin = start, xmax = stop,
                                                  ymin = -Inf, ymax = Inf), fill = "grey85", show.legend = FALSE)
  p <- p + geom_line(data = data_plot_line, aes(x = index, y = eval(parse(text = outcome_names))), size = 1)

  if (isTRUE(show_labels) || missing(show_labels)) {
    p <- p + geom_label(data = data_plot_group, aes(x = index, y = label_height, label = window),
                        color = "black", size = 4)
  }

  p <- p + theme_bw()
  p <- p + xlab("Dataset index / row") + ylab("Outcome") +
    ggtitle("Validation Windows")

  return(p)
}
