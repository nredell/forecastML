
#' Compute forecast error
#'
#' Compute forecast error metrics on the validation datasets or a test dataset
#'
#' @param data_results An object of class 'training_results' or 'forecast_results'.
#' @param data_test If 'data_results' is an object of class 'forecast_results', a data.frame used to
#' assess the accuracy of a 'forecast_results' object. 'data_test' should have the outcome/target columns and any grouping columns.
#' @param test_indices Required if 'data_test' is given. A vector or 1-column data.frame of numeric row indices or dates (class'Date') with length nrow(data_test).
#' @param metrics Common forecast error metrics. See the Error Metrics section below for details.
#' @param models Filter results by user-defined model name from train_model() (optional).
#' @param horizons Filter results by horizon (optional).
#' @param windows Filter results by validation window number (optional).
#' @param group_filter A string for filtering plot results for grouped time-series (e.g., "group_col_1 == 'A'").
#'
#' @section Error Metrics:
#' mae = Means absolute error \cr
#' mape = Mean absolute percentage error \cr
#' mdape = Median absolute percentage error \cr
#' smape = Symmetrical mean absolute percentage error
#'
#' @return A 'validation_error' or 'forecast_error' object: A list of data.frames
#' of error metrics for the validation datasets or forecast dataset depending
#' on the data_test argument. \cr
#' A list containing: \cr
#' Error metrics by horizon + validation window \cr
#' Error metrics by horizon, collapsed across validation windows \cr
#' Global error metrics collapsed across horizons and validation windows
#' @example /R/examples/example_return_error.R
#' @export
return_error <- function(data_results, data_test = NULL, test_indices = NULL,
                         metrics = c("mae", "mape", "mdape", "smape"),
                         models = NULL, horizons = NULL, windows = NULL, group_filter = NULL) {

  data <- data_results

  if (!(methods::is(data, "training_results") || methods::is(data, "forecast_results"))) {
    stop("The 'data' argument takes an object of class 'training_results' or 'forecast_results' as input. Run predict() on a 'forecast_model' object first.")
  }

  if (methods::is(data, "forecast_results") && is.null(data_test)) {
    stop("Computing forecast error metrics requires a data.frame input to the 'data_test' argument.")
  }

  if (methods::is(data, "training_results") && !is.null(data_test)) {
    stop("The 'data_test' argument should be NULL when assessing validation error.")
  }

  outcome_cols <- attributes(data)$outcome_cols
  outcome_names <- attributes(data)$outcome_names
  groups <- attributes(data)$groups

  if (!is.null(data$date_indices)) {
    data$valid_indices <- data$date_indices
  }

  if (methods::is(data, "forecast_results")) {

    data_test$forecast_period <- test_indices

    data_test <- dplyr::select(data_test, forecast_period, outcome_names, !!groups)

    data <- dplyr::inner_join(data, data_test, by = c("forecast_period", groups))
  }

  supported_error_metrics <- c("mae", "mape", "mdape", "smape")

  data$residual <- data[, outcome_names] - data[, paste0(outcome_names, "_pred")]

  models <- if (is.null(models)) {unique(data$model)} else {models}
  horizons <- if (is.null(horizons)) {unique(data$horizon)} else {horizons}
  windows <- if (is.null(windows)) {unique(data$window_number)} else {windows}

  data <- data[data$model %in% models & data$horizon %in% horizons &
               data$window_number %in% windows, ]

  if (!is.null(group_filter)) {

    data <- dplyr::filter(data, eval(parse(text = group_filter)))
  }

  # The error metric calculations are currently a lot of copy and paste that needs to be cleaned up.
  # To-do: reduce verboseness and create error metric functions or import from another package.

  if(is.null(data_test)) {  # Error metrics for training data.

    data_1 <- data %>%
      dplyr::group_by_at(dplyr::vars(model, horizon, window_number, !!groups)) %>%
      dplyr::summarise("window_start" = min(valid_indices, na.rm = TRUE),
                       "window_stop" = max(valid_indices, na.rm = TRUE),
                       "window_midpoint" = mean(valid_indices, na.rm = TRUE),
                       "mae" = mean(abs(residual), na.rm = TRUE),
                       "mape" = mean(abs(residual) / abs((eval(parse(text = outcome_names)))), na.rm = TRUE) * 100,
                       "mdape" = median(abs(residual) / abs((eval(parse(text = outcome_names)))), na.rm = TRUE) * 100,
                       "smape" = mean(2 * abs(residual) /
                                        (abs((eval(parse(text = outcome_names)))) + abs(eval(parse(text = paste0(outcome_names, "_pred"))))),
                                      na.rm = TRUE) * 100)
    data_1$mape <- with(data_1, ifelse(is.infinite(mape), NA, mape))
    data_1$mape <- with(data_1, ifelse(is.infinite(mdape), NA, mdape))

    # Compute error metric by horizon and window length across all validation windows.
    data_2 <- data %>%
      dplyr::group_by_at(dplyr::vars(model, horizon, !!groups)) %>%
      dplyr::summarise("window_start" = min(valid_indices, na.rm = TRUE),
                       "window_stop" = max(valid_indices, na.rm = TRUE),
                       "mae" = mean(abs(residual), na.rm = TRUE),
                       "mape" = mean(abs(residual) / abs((eval(parse(text = outcome_names)))), na.rm = TRUE) * 100,
                       "mdape" = median(abs(residual) / abs((eval(parse(text = outcome_names)))), na.rm = TRUE) * 100,
                       "smape" = mean(2 * abs(residual) /
                                         (abs((eval(parse(text = outcome_names)))) + abs(eval(parse(text = paste0(outcome_names, "_pred"))))),
                                          na.rm = TRUE) * 100)
    data_2$mape <- with(data_2, ifelse(is.infinite(mape), NA, mape))
    data_2$mdape <- with(data_2, ifelse(is.infinite(mdape), NA, mdape))

    # Compute error metric by model.
    data_3 <- data %>%
      dplyr::group_by_at(dplyr::vars(model, !!groups)) %>%
      dplyr::summarise("window_start" = min(valid_indices, na.rm = TRUE),
                       "window_stop" = max(valid_indices, na.rm = TRUE),
                       "mae" = mean(abs(residual), na.rm = TRUE),
                       "mape" = mean(abs(residual) / abs((eval(parse(text = outcome_names)))), na.rm = TRUE) * 100,
                       "mdape" = median(abs(residual) / abs((eval(parse(text = outcome_names)))), na.rm = TRUE) * 100,
                       "smape" = mean(2 * abs(residual) /
                                         (abs((eval(parse(text = outcome_names)))) + abs(eval(parse(text = paste0(outcome_names, "_pred"))))),
                                          na.rm = TRUE) * 100)
    data_3$mape <- with(data_3, ifelse(is.infinite(mape), NA, mape))
    data_3$mdape <- with(data_3, ifelse(is.infinite(mdape), NA, mdape))

    } else {  # Error metrics for the forecast_results class which has no validation windows and slightly different grouping columns.

      data_1 <- data.frame()

      # Compute error metric by horizon and window length across all validation windows.
      data_2 <- data %>%
        dplyr::group_by(model, model_forecast_horizon, horizon) %>%
        dplyr::group_by_at(dplyr::vars(model, !!groups, model_forecast_horizon, horizon)) %>%
        dplyr::summarise("mae" = mean(abs(residual), na.rm = TRUE),
                         "mape" = mean(abs(residual) / abs((eval(parse(text = outcome_names)))), na.rm = TRUE) * 100,
                         "mdape" = median(abs(residual) / abs((eval(parse(text = outcome_names)))), na.rm = TRUE) * 100,
                         "smape" = mean(2 * abs(residual) /
                                           (abs((eval(parse(text = outcome_names)))) + abs(eval(parse(text = paste0(outcome_names, "_pred"))))),
                                            na.rm = TRUE) * 100)
      data_2$mape <- with(data_2, ifelse(is.infinite(mape), NA, mape))
      data_2$mdape <- with(data_2, ifelse(is.infinite(mdape), NA, mdape))

      # Compute error metric by model.
      data_3 <- data %>%
        dplyr::group_by_at(dplyr::vars(model, !!groups, model_forecast_horizon)) %>%
        dplyr::summarise("mae" = mean(abs(residual), na.rm = TRUE),
                         "mape" = mean(abs(residual) / abs((eval(parse(text = outcome_names)))), na.rm = TRUE) * 100,
                         "mdape" = median(abs(residual) / abs((eval(parse(text = outcome_names)))), na.rm = TRUE) * 100,
                         "smape" = mean(2 * abs(residual) /
                                           (abs((eval(parse(text = outcome_names)))) + abs(eval(parse(text = paste0(outcome_names, "_pred"))))),
                                            na.rm = TRUE) * 100)
      data_3$mape <- with(data_3, ifelse(is.infinite(mape), NA, mape))
      data_3$mdape <- with(data_3, ifelse(is.infinite(mdape), NA, mdape))
    }

  metrics <- supported_error_metrics[supported_error_metrics %in% metrics]
  error_metrics_missing <- supported_error_metrics[!supported_error_metrics %in% metrics]

  if (length(error_metrics_missing) > 0 ) {
    data_1 <- dplyr::select(data_1, -error_metrics_missing)
    data_2 <- dplyr::select(data_2, -error_metrics_missing)
    data_3 <- dplyr::select(data_3, -error_metrics_missing)
  }

  data_out <- list("error_by_window" = data_1, "error_by_horizon" = data_2, "error_global" = data_3)
  data_out[] <- lapply(data_out, as.data.frame)  # Remove the tibble class.

  attr(data_out, "error_metrics") <- metrics

  if (is.null(data_test)) {
    class(data_out) <- c("validation_error", class(data_out))
  } else {
    class(data_out) <- c("forecast_error", class(data_out))
  }

  return(data_out)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#' Plot validation dataset forecast error
#'
#' Plot forecast error at various levels of aggregation across validation datasets.
#' @param data_error An object of class 'validation_error' from return_error().
#' @param data_results An object of class 'training_results' from predict.forecast_model().
#' @param type Select plot type; 'time' is the default plot.
#' @param models Filter results by user-defined model name from train_model() (optional).
#' @param horizons Filter results by horizon (optional).
#' @param windows Filter results by validation window number (optional).
#' @param group_filter A string for filtering plot results for grouped time-series (e.g., "group_col_1 == 'A'").
#' @return Forecast error plots of class 'ggplot'.
#' @export
plot.validation_error <- function(data_error, data_results, type = c("time", "horizon", "global"),
                                  models = NULL, horizons = NULL, windows = NULL, group_filter = NULL) {

  if(!methods::is(data_error, "validation_error")) {
    stop("The 'data_error' argument takes an object of class 'validation_error' as input. Run return_error() first.")
  }

  type <- type[1]

  outcome_cols <- attributes(data_results)$outcome_cols
  outcome_names <- attributes(data_results)$outcome_names
  groups <- attributes(data_results)$groups

  error_metrics <- attributes(data_error)$error_metrics

  data_plot <- data_error[[1]]

  models <- if (is.null(models)) {unique(data_results$model)} else {models}
  horizons <- if (is.null(horizons)) {unique(data_results$horizon)} else {horizons}
  windows <- if (is.null(windows)) {unique(data_results$window_number)} else {windows}

  # Filter the dataset based on user input.
  data_plot <- data_plot[data_plot$horizon %in% horizons &
                         data_plot$model %in% models & data_plot$window_number %in% windows, ]

  data_results <- data_results[data_results$horizon %in% horizons &
                               data_results$model %in% models & data_results$window_number %in% windows, ]

  if (!is.null(group_filter)) {
    data_plot <- dplyr::filter(data_plot, eval(parse(text = group_filter)))
    data_results <- dplyr::filter(data_results, eval(parse(text = group_filter)))
  }

  # Melt the data for plotting.
  data_plot <- tidyr::gather(data_plot, "error_metric", "value",
                             -!!names(data_plot)[!names(data_plot) %in% error_metrics])

  if (type == "time") {

    data_plot$group <- apply(data_plot[, c("model", "horizon", groups), drop = FALSE], 1, paste, collapse = " + ")
    data_plot$group <- ordered(data_plot$group)

    p <- ggplot()
    if (length(unique(data_plot$window_midpoint)) != 1) {
      p <- p + geom_line(data = data_plot, aes(x = window_midpoint, y = value, color = factor(model), group = group), size = 1.05)
    }
    p <- p + geom_point(data = data_plot, aes(x = window_midpoint, y = value, color = factor(model), group = group), show.legend = FALSE)
    p <- p + scale_color_viridis_d()
    p <- p + facet_grid(error_metric ~ horizon, scales = "free")
    p <- p + theme_bw()
    p <- p + xlab("Dataset row / index") + ylab("Forecast error metric") + labs(color = "Model") +
      ggtitle("Forecast Error by Validation Window - Faceted by horizon and metric")
    return(p)
  }

  if (type == "horizon") {

    data_plot$group <- apply(data_plot[, c("model", "window_number", groups), drop = FALSE], 1, paste, collapse = " + ")
    data_plot$group <- ordered(data_plot$group)

    data_plot_summary <- data_error[[2]]

    models <- if (is.null(models)) {unique(data_plot_summary$model)} else {models}
    horizons <- if (is.null(horizons)) {unique(data_plot_summary$horizon)} else {horizons}

    data_plot_summary <- data_plot_summary[data_plot_summary$horizon %in% horizons &
                                           data_plot_summary$model %in% models, ]

    if (!is.null(group_filter)) {
      data_plot_summary <- dplyr::filter(data_plot_summary, eval(parse(text = group_filter)))
    }

    # Melt the data for plotting.
    data_plot_summary <- tidyr::gather(data_plot_summary, "error_metric", "value",
                                       -!!names(data_plot_summary)[!names(data_plot_summary) %in% error_metrics])

    data_plot_summary$group <- apply(data_plot_summary[, c("model", groups), drop = FALSE], 1, paste, collapse = " + ")
    data_plot_summary$group <- ordered(data_plot_summary$group)

    p <- ggplot()
    p <- p + geom_point(data = data_plot, aes(x = ordered(horizon), y = value, color = factor(model), group = group), size = 1.05, alpha = .20, show.legend = FALSE)
    if (length(unique(data_plot$window_midpoint)) != 1) {
      if (length(unique(data_plot$horizon)) != 1) {
        p <- p + geom_line(data = data_plot, aes(x = ordered(horizon), y = value, color = factor(model), group = group), size = 1.05, alpha = .20, show.legend = FALSE)
      }
      p <- p + geom_point(data = data_plot_summary, aes(x = ordered(horizon), y = value, color = factor(model)), size = 1.1)
    }
    p <- p + geom_label(data = data_plot_summary, aes(x = ordered(horizon), y = value, color = factor(model), label = round(value, 1)), position = position_dodge(.5), show.legend = FALSE)
    p <- p + scale_color_viridis_d()
    p <- p + facet_grid(error_metric ~ ., scales = "free")
    p <- p + theme_bw()
    p <- p + xlab("Forecast horizon") + ylab("Forecast error metric") + labs(color = "Model", alpha = NULL) +
      ggtitle("Forecast Error by Forecast Horizon - Faceted by metric",
              subtitle = "Each line is a validation window")
    return(p)
  }

  if (type == "global") {

    data_plot_summary <- data_error[[3]]

    data_plot_summary <- data_plot_summary[data_plot_summary$model %in% models, ]

    if (!is.null(group_filter)) {
      data_plot_summary <- dplyr::filter(data_plot_summary, eval(parse(text = group_filter)))
    }

    data_plot_summary$group <- apply(data_plot_summary[, c("model", groups), drop = FALSE], 1, paste, collapse = " + ")
    data_plot_summary$group <- ordered(data_plot_summary$group)


    data_plot_summary <- tidyr::gather(data_plot_summary, "error_metric", "value",
                                       -!!names(data_plot_summary)[!names(data_plot_summary) %in% error_metrics])

    p <- ggplot(data_plot_summary)
    p <- p + geom_bar(data = data_plot_summary, aes(x = model, y = value, fill = factor(model), group = group), stat = "identity", position = position_dodge(width = 1))
    p <- p + geom_label(data = data_plot_summary, aes(x = model, y = value, label = round(value, 1), group = group), show.legend = FALSE, position = position_dodge(width = 1))
    p <- p + scale_fill_viridis_d()
    p <- p + facet_grid(error_metric ~ ., scales = "free")
    p <- p + theme_bw()
    p <- p + xlab("Model") + ylab("Forecast error metric") + labs(fill = "Model", alpha = NULL) +
      ggtitle("Forecast Error - Faceted by metric")
    return(p)
  }
}
