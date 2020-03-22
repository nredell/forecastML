#' Compute forecast error
#'
#' Compute forecast error metrics on the validation datasets or a new test dataset.
#'
#' @param data_results An object of class 'training_results' or 'forecast_results' from running (a)
#' \code{\link[=predict.forecast_model]{predict}} on a trained model or (b) \code{combine_forecasts()}.
#' @param data_test Required for forecast results only. If \code{data_results} is an object of class 'forecast_results', a data.frame used to
#' assess the accuracy of a 'forecast_results' object. \code{data_test} should have the outcome/target columns
#' and any grouping columns.
#' @param test_indices Required if \code{data_test} is given. A vector or 1-column data.frame of numeric
#' row indices or dates (class 'Date' or 'POSIXt') with length \code{nrow(data_test)}.
#' @param metrics A character vector of common forecast error metrics. The default behavior is to return all metrics.
#' @param models Optional. A character vector of user-defined model names supplied to \code{train_model()} to filter results.
#' @param horizons Optional. A numeric vector to filter results by horizon.
#' @param windows Optional. A numeric vector to filter results by validation window number.
#' @param group_filter Optional. A string for filtering plot results for grouped time-series
#' (e.g., \code{"group_col_1 == 'A'"}). \code{group_filter} is passed to \code{dplyr::filter()} internally.
#'
#' @return An S3 object of class 'validation_error' or 'forecast_error': A list of data.frames
#' of error metrics for the validation datasets or forecast dataset depending
#' on the \code{data_test} argument. An input to \code{data_results} from \code{combine_forecasts()} will return
#' a single data.frame with results for each model passed in \code{combine_forecasts(...)}. \cr
#'
#' A list containing: \cr
#'
#' \itemize{
#'   \item Error metrics by horizon + validation window
#'   \item Error metrics by horizon, collapsed across validation windows
#'   \item Global error metrics collapsed across horizons and validation windows
#'}
#' @section Error Metrics:
#'
#' \itemize{
#'   \item \code{mae}: Mean absolute error (works with factor outcomes)
#'   \item \code{mape}: Mean absolute percentage error
#'   \item \code{mdape}: Median absolute percentage error
#'   \item \code{smape}: Symmetrical mean absolute percentage error
#'}
#' @section Methods and related functions:
#'
#' The output of \code{return_error()} has the following generic S3 methods
#'
#' \itemize{
#'   \item \code{\link[=plot.validation_error]{plot}} from \code{return_error()}
#' }
#' @example /R/examples/example_return_error.R
#' @export
return_error <- function(data_results, data_test = NULL, test_indices = NULL,
                         metrics = c("mae", "mape", "mdape", "smape"),
                         models = NULL, horizons = NULL, windows = NULL, group_filter = NULL) {

  if (!(methods::is(data_results, "training_results") || methods::is(data_results, "forecast_results"))) {
    stop("The 'data_results' argument takes an object of class 'training_results' or 'forecast_results' as input. Run predict() on a 'forecast_model' object first.")
  }

  if (methods::is(data_results, "training_results") && !is.null(data_test)) {
    stop("The 'data_test' argument should be NULL when assessing validation error.")
  }

  if (methods::is(data_results, "forecast_results") && is.null(data_test)) {
    stop("Computing forecast error metrics requires a data.frame input to the 'data_test' argument.")
  }

  if (xor(is.null(data_test), is.null(test_indices))) {
    stop("If using a test dataset to assess forecast error, both 'data_test' and 'test_indices' need to be specified.")
  }

  # The order of these available metrics should match the error_functions vector below. Only 'mae'
  # is available for factor outcomes at present; an error will be thrown below if this is not the case.
  error_metrics <- c("mae", "mape", "mdape", "smape")

  # Filter the user input error metrics to only those that are available.
  metrics <- metrics[metrics %in% error_metrics]

  if (length(metrics) == 0) {
    stop("None of the error 'metrics' match any of 'mae', 'mape', 'mdape', or 'smape'.")
  }

  # The return() from combine_forecasts(), 'forecastML', is also an object of class 'forecast_results' but does not need
  # filtering, so these input types will be handled slightly differently.
  is_forecastML <- methods::is(data_results, "forecastML")

  data <- data_results
  rm(data_results)

  method <- attributes(data)$method
  outcome_name <- attributes(data)$outcome_name
  outcome_levels <- attributes(data)$outcome_levels
  groups <- attributes(data)$groups
  #----------------------------------------------------------------------------
  # For factor outcomes, is the prediction a factor level or probability?
  if (!is.null(outcome_levels)) {

    factor_level <- if (any(names(data) %in% paste0(outcome_name, "_pred"))) {TRUE} else {FALSE}
    factor_prob <- !factor_level

    if (!all(metrics %in% c("mae"))) {
      stop("Only the 'mae' metric is available for factor outcomes. Set 'metrics = 'mae'' and re-run.")
    }

    # This will eventually change.
    if (factor_prob) {
      stop("Error metrics with predicted class probabilities are not currently supported.")
    }
  }
  #----------------------------------------------------------------------------
  # If dates were given, use dates.
  if (!is.null(data$date_indices)) {
    data$valid_indices <- data$date_indices
  }

  # Merge user-supplied test data to the forecasts from predict.forecast_model()
  if (methods::is(data, "forecast_results")) {

    data_test$forecast_period <- test_indices

    data_test <- dplyr::select(data_test, .data$forecast_period, !!outcome_name, !!groups)

    data$model_forecast_horizon <- data$horizon  # Added to use the same dplyr code as direct forecasting.

    data <- dplyr::inner_join(data, data_test, by = c("forecast_period", groups))

    if (nrow(data) == 0) {
      stop("The test dataset in 'data_test' does not overlap with the forecast period in 'data_results'.")
    }
  }

  if (methods::is(data, "training_results")) {
    # Change the forecast horizon name to "horizon", which, although it makes sense from a naming perspective,
    # will reduce the amount of code below.
    if (method == "direct") {
      data$horizon <- data$model_forecast_horizon
      data$model_forecast_horizon <- NULL
    }
  }
  #----------------------------------------------------------------------------
  # Residual calculations
  if (is.null(outcome_levels)) {  # Numeric outcome.

    data$residual <- data[, outcome_name] - data[, paste0(outcome_name, "_pred")]

  } else {  # Factor outcome.

    if (factor_level) {

      # Binary accuracy/residual. A residual of 1 is an incorrect classification.
      data$residual <- ifelse(as.character(data[, outcome_name, drop = TRUE]) != as.character(data[, paste0(outcome_name, "_pred"), drop = TRUE]), 1, 0)

    } else {  # Class probabilities were predicted.

      # data_residual <- 1 - data[, names(data) %in% outcome_levels]
      # names(data_residual) <- paste0(names(data_residual), "_residual")
      # data <- dplyr::bind_cols(data, data_residual)
      # rm(data_residual)
    }
  }
  #----------------------------------------------------------------------------
  # Filter results based on user input.
  if (!is_forecastML) {

    models <- if (is.null(models)) {unique(data$model)} else {models}
    horizons <- if (is.null(horizons)) {unique(data$horizon)} else {horizons}
    windows <- if (is.null(windows)) {unique(data$window_number)} else {windows}

    data <- data[data$model %in% models & data$horizon %in% horizons & data$window_number %in% windows, ]

    if (!is.null(group_filter)) {
      data <- dplyr::filter(data, eval(parse(text = group_filter)))
    }
  }
  #----------------------------------------------------------------------------
  # Select error functions. The forecastML internal error functions are in zzz.R.
  # The functions are called with named x, y, and z args in dplyr::summarize_at().
  error_functions <- c(forecastML_mae, forecastML_mape, forecastML_mdape, forecastML_smape)
  # The user-selected 'metrics' are used to select the appropriate error functions.
  select_error_funs <- sapply(metrics, function(x) {which(x == error_metrics)})
  error_functions <- error_functions[select_error_funs]
  names(error_functions) <- error_metrics[select_error_funs]
  #----------------------------------------------------------------------------
  # For all error function args: x = 'residual', y = 'actual', and z = 'prediction'. Unused
  # function args for a given metric are passed in ... and ignored. See zzz.R.
  if (is.null(data_test)) {  # Error metrics for training data.

    # Compute error metrics at the validation window level.
    data_1 <- data %>%
      dplyr::group_by_at(dplyr::vars(.data$model, .data$horizon, .data$window_number)) %>%
      dplyr::mutate("window_start" = min(.data$valid_indices, na.rm = TRUE),
                    "window_stop" = max(.data$valid_indices, na.rm = TRUE),
                    "window_midpoint" = base::mean(.data$valid_indices, na.rm = TRUE)) %>%
      dplyr::group_by_at(dplyr::vars(.data$model, .data$horizon,
                                     .data$window_number, !!groups, .data$window_start,
                                     .data$window_stop, .data$window_midpoint)) %>%
      dplyr::summarize_at(dplyr::vars(1),  # 1 is a col position that gets the fun to run; args x, y, and z defined below.
                          .funs = error_functions,
                          x = rlang::quo(.data$residual),
                          y = rlang::sym(outcome_name),
                          z = rlang::sym(paste0(outcome_name, "_pred"))
                          )

    # Compute error metric by horizon and window length across all validation windows.
    data_2 <- data %>%
      dplyr::group_by_at(dplyr::vars(.data$model, .data$horizon)) %>%
      dplyr::mutate("window_start" = min(.data$valid_indices, na.rm = TRUE),
                    "window_stop" = max(.data$valid_indices, na.rm = TRUE)) %>%
      dplyr::group_by_at(dplyr::vars(.data$model, .data$horizon, !!groups, .data$window_start, .data$window_stop)) %>%
      dplyr::summarize_at(dplyr::vars(1),  # 1 is a col position that gets the fun to run; args x, y, and z defined below.
                          .funs = error_functions,
                          x = rlang::quo(.data$residual),
                          y = rlang::sym(outcome_name),
                          z = rlang::sym(paste0(outcome_name, "_pred")))

    # Compute error metric by model.
    data_3 <- data %>%
      dplyr::group_by_at(dplyr::vars(.data$model)) %>%
      dplyr::mutate("window_start" = min(.data$valid_indices, na.rm = TRUE),
                    "window_stop" = max(.data$valid_indices, na.rm = TRUE)) %>%
      dplyr::group_by_at(dplyr::vars(.data$model, !!groups, .data$window_start, .data$window_stop)) %>%
      dplyr::summarize_at(dplyr::vars(1),  # 1 is a col position that gets the fun to run; args x, y, and z defined below.
                          .funs = error_functions,
                          x = rlang::quo(.data$residual),
                          y = rlang::sym(outcome_name),
                          z = rlang::sym(paste0(outcome_name, "_pred")))
    #--------------------------------------------------------------------------
    } else if (!is_forecastML) {  # Error metrics for the forecast_results class which has no validation windows and slightly different grouping columns.

      data_1 <- data.frame()

      # Compute error metric by horizon and window length across all validation windows.
      data_2 <- data %>%
        dplyr::group_by_at(dplyr::vars(.data$model, !!groups, .data$model_forecast_horizon, .data$horizon)) %>%
        dplyr::summarize_at(dplyr::vars(1),  # 1 is a col position that gets the fun to run; args x, y, and z defined below.
                            .funs = error_functions,
                            x = rlang::quo(.data$residual),
                            y = rlang::sym(outcome_name),
                            z = rlang::sym(paste0(outcome_name, "_pred")))

      # Compute error metric by model.
      data_3 <- data %>%
        dplyr::group_by_at(dplyr::vars(.data$model, !!groups, .data$model_forecast_horizon)) %>%
        dplyr::summarize_at(dplyr::vars(1),  # 1 is a col position that gets the fun to run; args x, y, and z defined below.
                            .funs = error_functions,
                            x = rlang::quo(.data$residual),
                            y = rlang::sym(outcome_name),
                            z = rlang::sym(paste0(outcome_name, "_pred")))

    } else {  # Final forecasts from combine_forecasts().

      data_combined <- data %>%
        dplyr::group_by_at(dplyr::vars(.data$model, !!groups)) %>%
        dplyr::summarize_at(dplyr::vars(1),  # 1 is a col position that gets the fun to run; args x, y, and z defined below.
                            .funs = error_functions,
                            x = rlang::quo(.data$residual),
                            y = rlang::sym(outcome_name),
                            z = rlang::sym(paste0(outcome_name, "_pred")))

    }  # End error metrics for forecast results.
  #----------------------------------------------------------------------------

  if (!is_forecastML) {

    data_out <- list("error_by_window" = data_1, "error_by_horizon" = data_2, "error_global" = data_3)
    data_out[] <- lapply(data_out, as.data.frame)  # Remove the tibble class.
  }

  if (is.null(data_test)) {  # Validation error.

      class(data_out) <- c("validation_error", class(data_out))

  } else {  # Forecast error.

    if (!is_forecastML) {

      class(data_out) <- c("forecast_error", class(data_out))

    } else {
      data_out <- as.data.frame(data_combined)
      class(data_out) <- c("forecast_error", class(data_out))
    }
  }

  attr(data_out, "error_metrics") <- metrics

  return(data_out)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#' Plot validation dataset forecast error
#'
#' Plot forecast error at various levels of aggregation across validation datasets.
#' @param x An object of class 'validation_error' from \code{return_error()}.
#' @param data_results An object of class 'training_results' from \code{predict.forecast_model()}.
#' @param type Select plot type; \code{type = "time"} is the default plot.
#' @param metric Select error metric to plot (e.g., "mae"); \code{attributes(x)$error_metrics[1]} is the default metric.
#' @param models Optional. A vector of user-defined model names from \code{train_model()} to filter results.
#' @param horizons Optional. A numeric vector to filter results by horizon.
#' @param windows Optional. A numeric vector to filter results by validation window number.
#' @param group_filter A string for filtering plot results for grouped time-series (e.g., \code{"group_col_1 == 'A'"}).
#' @param facet Optional. A formula with any combination of \code{horizon}, \code{model}, or \code{group} (for grouped time series).
#' passed to \code{ggplot2::facet_grid()} internally (e.g., \code{horizon ~ model}, \code{horizon + model ~ .}, \code{~ horizon + group}).
#' Can be \code{NULL}. The default faceting is set internally depending on the plot \code{type}.
#' @param ... Not used.
#' @return Forecast error plots of class 'ggplot'.
#' @export
plot.validation_error <- function(x, data_results, type = c("time", "horizon", "global"), metric = NULL,
                                  facet = NULL, models = NULL, horizons = NULL, windows = NULL, group_filter = NULL, ...) { # nocov start

  #----------------------------------------------------------------------------
  data_error <- x

  if (!methods::is(data_results, "training_results")) {
    stop("The 'data_results' argument takes an object of class 'training_results' as input. Run predict.forecast_model() first.")
  }

  type <- type[1]

  method <- attributes(data_results)$method
  outcome_name <- attributes(data_results)$outcome_name
  groups <- attributes(data_results)$groups

  error_metrics <- metric

  if (is.null(error_metrics)) {
    error_metrics <- attributes(data_error)$error_metrics[1]
  }

  if (!error_metrics %in% attributes(data_error)$error_metrics) {
    stop("The error metric in 'metric' is not in the validation error dataset 'x'; re-run return_error() with this metric.")
  }
  #----------------------------------------------------------------------------
  # Set default plot facets for each plot type.
  if (is.null(facet)) {

    if (type == "time") {

      facet <- horizon ~ model

    } else if (type == "horizon") {

      facet <- horizon ~ model

    } else if (type == "global") {

      facet <- horizon ~ model
    }
  }

  facets <- forecastML_facet_plot(facet, groups)  # Function in zzz.R.
  facet <- facets[[1]]
  facet_names <- facets[[2]]

  if (type == "time") {

    data_plot <- data_error$error_by_window

  } else if (type == "horizon") {

    data_plot <- data_error$error_by_horizon
    data_plot$window_number <- TRUE  # Not in data, added for filtering.

  } else if (type == "global") {

    data_plot <- data_error$error_global
    data_plot$window_number <- TRUE  # Not in data, added for filtering.
    data_plot$horizon <- TRUE  # Not in data, added for filtering.
  }
  #----------------------------------------------------------------------------
  # Filter the datasets based on user input.
  models <- if (is.null(models)) {unique(data_results$model)} else {models}
  horizons <- if (is.null(horizons)) {
    if (method == "direct") {
      unique(data_results$model_forecast_horizon)
    } else if (method == "multi_output") {
      unique(data_results$horizon)
    }} else {
      horizons
      }
  windows <- if (is.null(windows)) {unique(data_results$window_number)} else {windows}

  data_plot <- data_plot[data_plot$horizon %in% horizons & data_plot$model %in% models & data_plot$window_number %in% windows, ]

  if (method == "direct") {
    data_results$horizon <- data_results$model_forecast_horizon
    data_results$model_forecast_horizon <- NULL
  }

  data_results <- data_results[data_results$horizon %in% horizons & data_results$model %in% models & data_results$window_number %in% windows, ]
  #----------------------------------------------------------------------------
  # User filtering to display select results in grouped time-series.
  if (!is.null(group_filter)) {
    data_plot <- dplyr::filter(data_plot, eval(parse(text = group_filter)))
    data_results <- dplyr::filter(data_results, eval(parse(text = group_filter)))
  }
  #----------------------------------------------------------------------------
  # Melt the data for plotting.
  data_plot <- tidyr::gather(data_plot, "error_metric", "value", -!!names(data_plot)[!names(data_plot) %in% error_metrics])
  #----------------------------------------------------------------------------
  # ggplot colors and facets are complimentary: all facets, same color; all colors, no facet.
  ggplot_color <- c(c("model", "horizon", groups)[!c("model", "horizon", groups) %in% facet_names])
  #----------------------------------------------------------------------------

  if (is.null(groups)) {
    data_plot <- dplyr::arrange(data_plot, .data$model, .data$horizon, .data$window_number)
  } else {
    data_plot <- dplyr::arrange(data_plot, .data$model, .data$horizon, .data$window_number, !!!rlang::syms(groups))
  }

  if (type == "global") {
    data_plot$horizon <- "All"
    data_plot$horizon <- ordered(data_plot$horizon)
  }

  data_plot$horizon <- factor(data_plot$horizon, levels = unique(data_plot$horizon), ordered = TRUE)

  data_plot[, groups] <- lapply(seq_along(data_plot[, groups, drop = FALSE]), function(i) {
    factor(data_plot[, groups[i]], levels = unique(data_plot[, groups[i]]), ordered = TRUE)
  })

  data_plot$ggplot_color <- apply(data_plot[,  ggplot_color, drop = FALSE], 1, function(x) {paste(x, collapse = "-")})

  data_plot$ggplot_color <- factor(data_plot$ggplot_color, levels = unique(data_plot$ggplot_color), ordered = TRUE)

  # Give error a name in the legend if plot is faceted by model and horizon (and group if groups are given).
  if (length(ggplot_color) == 0) {
    data_plot$ggplot_color <- "Error"
    data_plot$ggplot_color <- ordered(data_plot$ggplot_color)
  }

  temp_1 <- unlist(Map(function(x) {toupper(substr(x[1], 1, 1))}, ggplot_color))
  temp_2 <- unlist(Map(function(x) {substr(x, 2, nchar(x))}, ggplot_color))
  x_axis_title <- paste(temp_1, temp_2, sep = "")
  x_axis_title <- paste(x_axis_title, collapse = " + ")

  #----------------------------------------------------------------------------
  # Create plots.
  if (type == "time") {

    p <- ggplot()

    if (length(unique(data_plot$window_midpoint)) != 1) {

      p <- p + geom_line(data = data_plot, aes(x = .data$window_midpoint,
                                               y = .data$value,
                                               color = .data$ggplot_color,
                                               group = .data$ggplot_color
                                               ), size = 1.05)
    }

    p <- p + geom_point(data = data_plot, aes(x = .data$window_midpoint,
                                              y = .data$value,
                                              color = .data$ggplot_color,
                                              group = .data$ggplot_color
                                              ))
    p <- p + scale_color_viridis_d()
    p <- p + facet_grid(facet, scales = "free")
    p <- p + theme_bw() + theme(panel.spacing = unit(0, "lines"))
    p <- p + xlab("Dataset index") + ylab(paste0("Forecast error metric (", error_metrics, ")")) + labs(color = x_axis_title) +
      ggtitle("Forecast Error by Validation Window")

    return(suppressWarnings(p))
  }

  if (type == "horizon") {

    p <- ggplot()
    p <- p + geom_col(data = data_plot,
                        aes(x = .data$ggplot_color,
                            y = .data$value,
                            fill = .data$ggplot_color,
                            group = .data$ggplot_color))
    p <- p + facet_grid(facet, scales = "free_y")
    p <- p + scale_color_viridis_d()
    p <- p + theme_bw() + theme(
      panel.spacing = unit(0, "lines"),
      axis.text.x = element_blank()
    )
    p <- p + xlab(x_axis_title) + ylab(paste0("Forecast error metric (", error_metrics, ")")) + labs(fill = x_axis_title, alpha = NULL) +
      ggtitle("Forecast Error by Forecast Horizon")
    p

    return(suppressWarnings(p))
  }

  if (type == "global") {

    p <- ggplot()

    p <- p + geom_bar(data = data_plot,
                      aes(x = .data$ggplot_color,
                          y = .data$value,
                          fill = .data$ggplot_color),
                      stat = "identity", position = position_dodge(width = 1))
    p <- p + scale_fill_viridis_d()
    p <- p + facet_grid(facet, scales = "free")
    p <- p + theme_bw() + theme(
      panel.spacing = unit(0, "lines"),
      axis.text.x = element_blank()
    )
    p <- p + xlab(x_axis_title) + ylab(paste0("Forecast error metric (", error_metrics, ")")) + labs(fill = x_axis_title, alpha = NULL) +
      ggtitle("Forecast Error Across Validation Windows and Horizons")

    return(suppressWarnings(p))
  }
} # nocov end
