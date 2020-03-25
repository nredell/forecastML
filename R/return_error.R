#' Compute forecast error
#'
#' Compute forecast error metrics on the validation datasets or a new test dataset.
#'
#' @param data_results An object of class 'training_results' or 'forecast_results' from running (a)
#' \code{\link[=predict.forecast_model]{predict}} on a trained model or (b) \code{combine_forecasts()}.
#' @param data_test Required for forecast results only. If \code{data_results} is an object of class 'forecast_results', a data.frame used to
#' assess the accuracy of a 'forecast_results' object. \code{data_test} should have the outcome/target columns
#' and any grouping columns.
#' @param test_indices Required if \code{data_test} is given or 'rmsse' %in% \code{metrics}. A vector or 1-column data.frame of numeric
#' row indices or dates (class 'Date' or 'POSIXt') with length \code{nrow(data_test)}.
#' @param aggregate Default \code{median}. A function--without parentheses--that aggregates historical prediction or forecast error across time series.
#' All error metrics are first calculated at the level of the individual time series. \code{aggregate} is then used to combine error metrics across
#' validation windows and horizons. Aggregations are returned at the group level if \code{data_results} contains groups.
#' @param metrics A character vector of common forecast error metrics. The default behavior is to return all metrics.
#' @param models Optional. A character vector of user-defined model names supplied to \code{train_model()} to filter results.
#' @param horizons Optional. A numeric vector to filter results by horizon.
#' @param windows Optional. A numeric vector to filter results by validation window number.
#' @param group_filter Optional. A string for filtering plot results for grouped time series
#' (e.g., \code{"group_col_1 == 'A'"}). \code{group_filter} is passed to \code{dplyr::filter()} internally.
#'
#' @return An S3 object of class 'validation_error', 'forecast_error', or 'forecastML_error': A list of data.frames
#' of error metrics for the validation or forecast dataset depending on the class of \code{data_results}: 'training_results',
#' 'forecast_results', or 'forecastML' from \code{combine_forecasts()}.
#'
#' A list containing: \cr
#'
#' \itemize{
#'   \item Error metrics by model, horizon, and validation window
#'   \item Error metrics by model and horizon, collapsed across validation windows
#'   \item Global error metrics by model collapsed across horizons and validation windows
#'}
#' @section Error Metrics:
#'
#' \itemize{
#'   \item \code{mae}: Mean absolute error (works with factor outcomes)
#'   \item \code{mape}: Mean absolute percentage error
#'   \item \code{mdape}: Median absolute percentage error
#'   \item \code{smape}: Symmetrical mean absolute percentage error
#'   \item \code{rmse}: Root mean squared error
#'   \item \code{rmsse}: Root mean squared scaled error from the M5 competition
#'}
#' @section Methods and related functions:
#'
#' The output of \code{return_error()} has the following generic S3 methods
#'
#' \itemize{
#'   \item \code{\link[=plot.validation_error]{plot}} from \code{return_error()}
#'   \item \code{\link[=plot.forecast_error]{plot}} from \code{return_error()}
#' }
#' @example /R/examples/example_return_error.R
#' @export
return_error <- function(data_results, data_test = NULL, test_indices = NULL, aggregate = stats::median,
                         metrics = c("mae", "mape", "mdape", "smape", "rmse", 'rmsse'),
                         models = NULL, horizons = NULL, windows = NULL, group_filter = NULL) {

  if (!(methods::is(data_results, "training_results") || methods::is(data_results, "forecast_results"))) {
    stop("The 'data_results' argument takes an object of class 'training_results' or 'forecast_results' as input. Run predict() on a 'forecast_model' object first.")
  }

  if (methods::is(data_results, "training_results") && "rmsse" %in% metrics && any(is.null(data_test), is.null(test_indices))) {
    warning("'rmsse' was not calculated. The 'rmsse' metric needs a dataset of actuals passed in 'data_test' and 'test_indices'.")
  }

  if (methods::is(data_results, "forecast_results") && is.null(data_test)) {
    stop("Computing forecast error metrics requires a data.frame input to the 'data_test' argument.")
  }

  if (xor(is.null(data_test), is.null(test_indices))) {
    stop("If using a test dataset to assess forecast error, both 'data_test' and 'test_indices' need to be specified.")
  }

  # The order of these available metrics should match the error_functions vector later in the script. Only 'mae'
  # is available for factor outcomes at present; an error will be thrown below if this is not the case.
  if (is.null(data_test)) {  # The M5 rmsse requires a dataset of actuals.

    error_metrics <- c("mae", "mape", "mdape", "smape", "rmse")

  } else {

    error_metrics <- c("mae", "mape", "mdape", "smape", "rmse", 'rmsse')
  }

  # Filter the user input error metrics to only those that are available.
  metrics <- metrics[metrics %in% error_metrics]

  if (length(metrics) == 0) {
    stop("None of the error 'metrics' match any of 'mae', 'mape', 'mdape', 'smape', 'rmse' or 'rmsse'.")
  }

  # The return() from combine_forecasts(), 'forecastML', is also an object of class 'forecast_results' but does not need
  # filtering, so these input types will be handled slightly differently.
  is_forecastML <- methods::is(data_results, "forecastML")

  data <- data_results

  method <- attributes(data)$method
  outcome_name <- attributes(data)$outcome_name
  outcome_levels <- attributes(data)$outcome_levels
  groups <- attributes(data)$groups

  if (is_forecastML) {type <- attributes(data)$type}  # 'horizon' or 'error'; used for group_by()
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
  #----------------------------------------------------------------------------
  # Merge user-supplied test data to the forecasts from predict.forecast_model()
  if (methods::is(data, "forecast_results")) {

    data_test$forecast_period <- test_indices

    data_test <- dplyr::select(data_test, .data$forecast_period, !!outcome_name, !!groups)

    data <- dplyr::inner_join(data, data_test, by = c("forecast_period", groups))

    if (nrow(data) == 0) {
      stop("The test dataset in 'data_test' does not overlap with the forecast period in 'data_results'.")
    }
  }
  #----------------------------------------------------------------------------
  # Special rmsse error metric from the M5 forecasting competition. This metric needs
  # access to the actuals from the training data which is unique for the supported error metrics.
  if (any(metrics %in% "rmsse")) {

    data_test$index <- test_indices

    data_test <- data_test %>% dplyr::select(.data$index, !!outcome_name, !!groups)

    # Repeat prediction data for multiple windows.
    n_windows <- length(unique(data_results$window_number))
    if (n_windows == 0) {n_windows <- 1}
    n_rows <- nrow(data_test)

    data_test <- data_test[rep(1:n_rows, n_windows), , drop = FALSE]

    data_test$window_number <- rep(1:n_windows, each = n_rows)

    if (methods::is(data_results, "training_results")) {

      data_merge <- data %>% dplyr::select(.data$valid_indices, .data$window_number, !!groups)
      names(data_merge)[names(data_merge) == "valid_indices"] <- "index"
      data_merge$instance_in_valid_dataset <- TRUE
    }

    if (methods::is(data_results, "training_results")) {

      if (unique(data$window_length) != 0) {  # Validation rows need to be removed from training rows.

        data_test <- dplyr::left_join(data_test, data_merge, by = c("index", "window_number", groups))
        data_test <- dplyr::distinct(data_test, .data$index, .data$window_number, !!!rlang::syms(groups), .keep_all = TRUE)
        data_test[which(data_test$instance_in_valid_dataset), outcome_name] <- NA

      } else {  # All training rows are also validation rows--essentially model fit error.

        data_test$window_number <- NULL
        data_test <- dplyr::left_join(data_test, data_merge, by = c("index", groups))
        data_test <- dplyr::distinct(data_test, .data$index, !!!rlang::syms(groups), .keep_all = TRUE)
      }
    }

    data_test <- data_test %>%
      dplyr::arrange(!!!rlang::syms(groups), .data$window_number, .data$index) %>%
      dplyr::group_by_at(dplyr::vars(.data$window_number, !!groups)) %>%
      dplyr::mutate("outcome_lag_1" = dplyr::lag(!!rlang::sym(outcome_name), 1),
                    "outcome_minus_outcome_lag_1" = !!rlang::sym(outcome_name) - .data$outcome_lag_1,
                    "sse_denom" = sum(.data$outcome_minus_outcome_lag_1^2, na.rm = TRUE),
                    "n" = sum(!is.na(.data$outcome_minus_outcome_lag_1))) %>%  # Used to compute average to convert sse_denom to mse_denom.
      dplyr::filter(n > 0) %>%  # Avoid a division by zero error for sparse data with NAs or only 1 data point.
      dplyr::summarize("mse_denom" = .data$sse_denom[1] / (n[1]))  # No need to subtract 1 per the formula, 'n' already accounts for missing data from lags.

    if (!is_forecastML) {

      data <- dplyr::left_join(data, data_test, by = c("window_number", groups))

    } else {

      data$window_number <- 1

      data <- dplyr::left_join(data, data_test, by = c("window_number", groups))
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
    horizons <- if (is.null(horizons)) {unique(data$model_forecast_horizon)} else {horizons}
    windows <- if (is.null(windows)) {unique(data$window_number)} else {windows}

    data <- data[data$model %in% models & data$model_forecast_horizon %in% horizons & data$window_number %in% windows, ]

    if (!is.null(group_filter)) {
      data <- dplyr::filter(data, eval(parse(text = group_filter)))
    }
  }
  #----------------------------------------------------------------------------
  # Select error functions. The forecastML internal error functions are in zzz.R.
  # The functions are called with named x, y, and z args in dplyr::summarize_at().
  if (is.null(data_test)) {  # The M5 rmsse requires a dataset of actuals.

    error_functions <- c(forecastML_mae, forecastML_mape, forecastML_mdape, forecastML_smape,
                         forecastML_rmse)
  } else {

    error_functions <- c(forecastML_mae, forecastML_mape, forecastML_mdape, forecastML_smape,
                         forecastML_rmse, forecastML_rmsse)
  }

  # The user-selected 'metrics' are used to select the appropriate error functions.
  select_error_funs <- sapply(metrics, function(x) {which(x == error_metrics)})
  error_functions <- error_functions[select_error_funs]
  names(error_functions) <- error_metrics[select_error_funs]
  #----------------------------------------------------------------------------
  # The error combinations at higher levels of aggregation--'data_2' and 'data_3'--are based on
  # error metrics calculated at the lowest level--data_1--and combined using the user-supplied
  # 'aggregate' method.

  # For all error function args: x = 'residual', y = 'actual', and z = 'prediction'. Unused
  # function args for a given metric are passed in ... and ignored. See zzz.R.
  if (methods::is(data_results, "training_results")) {  # Error metrics for training data.

    #--------------------------------------------------------------------------
    # Compute error metrics at the validation window level.
    if (!all(metrics %in% "rmsse")) {

      data_1 <- data %>%
        dplyr::group_by_at(dplyr::vars(.data$model, .data$model_forecast_horizon, .data$window_number)) %>%
        dplyr::mutate("window_start" = min(.data$valid_indices, na.rm = TRUE),
                      "window_stop" = max(.data$valid_indices, na.rm = TRUE),
                      "window_midpoint" = base::mean(.data$valid_indices, na.rm = TRUE)) %>%
        dplyr::group_by_at(dplyr::vars(.data$model, .data$model_forecast_horizon,
                                       .data$window_number, !!groups, .data$window_start,
                                       .data$window_stop, .data$window_midpoint)) %>%
        dplyr::summarize_at(dplyr::vars(1),  # 1 is a col position that gets the fun to run; args x, y, and z defined below.
                            .funs = error_functions,
                            x = rlang::quo(.data$residual),
                            y = rlang::sym(outcome_name),
                            z = rlang::sym(paste0(outcome_name, "_pred")))
    }

    if (any(metrics %in% "rmsse")) {

      data_1_rmsse <- data %>%
        dplyr::group_by_at(dplyr::vars(.data$model, .data$model_forecast_horizon, .data$window_number)) %>%
        dplyr::mutate("window_start" = min(.data$valid_indices, na.rm = TRUE),
                      "window_stop" = max(.data$valid_indices, na.rm = TRUE),
                      "window_midpoint" = base::mean(.data$valid_indices, na.rm = TRUE)) %>%
        dplyr::group_by_at(dplyr::vars(.data$model, .data$model_forecast_horizon,
                                       .data$window_number, !!groups, .data$window_start,
                                       .data$window_stop, .data$window_midpoint)) %>%
        dplyr::summarize("h" = sum(!is.na(.data$residual)),
                         "sse_num" = sum(.data$residual^2, na.rm = TRUE),
                         "mse_denom" = .data$mse_denom[1])

      data_1_rmsse$rmsse <- with(data_1_rmsse, sqrt((1 / h) * (sse_num / mse_denom)))

      data_1_rmsse$h <- NULL
      data_1_rmsse$sse_num <- NULL
      data_1_rmsse$mse_denom <- NULL

      if (all(metrics %in% "rmsse")) {

        data_1 <- data_1_rmsse

      } else {

        data_1$rmsse <- data_1_rmsse$rmsse
      }
    }
    #--------------------------------------------------------------------------
    # Compute error metric by horizon and window length across all validation windows.
    data_2 <- data_1 %>%
      dplyr::group_by_at(dplyr::vars(.data$model, .data$model_forecast_horizon)) %>%
      dplyr::mutate("window_start" = min(.data$window_start, na.rm = TRUE),
                    "window_stop" = max(.data$window_stop, na.rm = TRUE)) %>%
      dplyr::group_by_at(dplyr::vars(.data$model, .data$model_forecast_horizon, !!groups, .data$window_start, .data$window_stop)) %>%
      dplyr::summarize_at(dplyr::vars(!!!rlang::syms(metrics)), aggregate, na.rm = TRUE)
    #--------------------------------------------------------------------------
    # Compute error metric by model.
    data_3 <- data_1 %>%
      dplyr::group_by_at(dplyr::vars(.data$model)) %>%
      dplyr::mutate("window_start" = min(.data$window_start, na.rm = TRUE),
                    "window_stop" = max(.data$window_start, na.rm = TRUE)) %>%
      dplyr::group_by_at(dplyr::vars(.data$model, !!groups, .data$window_start, .data$window_stop)) %>%
      dplyr::summarize_at(dplyr::vars(!!!rlang::syms(metrics)), aggregate, na.rm = TRUE)
    #--------------------------------------------------------------------------

    } else if (!is_forecastML) {  # Error metrics for the forecast_results class which has no validation windows and slightly different grouping columns.

      #------------------------------------------------------------------------
      # Compute error metrics at the validation window level.
      if (!all(metrics %in% "rmsse")) {

        data_1 <- data %>%
          dplyr::group_by_at(dplyr::vars(.data$model, .data$model_forecast_horizon, .data$horizon, .data$window_number, !!groups)) %>%
          dplyr::summarize_at(dplyr::vars(1),  # 1 is a col position that gets the fun to run; args x, y, and z defined below.
                              .funs = error_functions,
                              x = rlang::quo(.data$residual),
                              y = rlang::sym(outcome_name),
                              z = rlang::sym(paste0(outcome_name, "_pred")))
      }

      if (any(metrics %in% "rmsse")) {

        data_1_rmsse <- data %>%
          dplyr::group_by_at(dplyr::vars(.data$model, .data$model_forecast_horizon, .data$horizon, .data$window_number, !!groups)) %>%
          dplyr::summarize("h" = sum(!is.na(.data$residual)),
                           "sse_num" = sum(.data$residual^2, na.rm = TRUE),
                           "mse_denom" = .data$mse_denom[1])

        data_1_rmsse$rmsse <- with(data_1_rmsse, sqrt((1 / h) * (sse_num / mse_denom)))

        data_1_rmsse$h <- NULL
        data_1_rmsse$sse_num <- NULL
        data_1_rmsse$mse_denom <- NULL

        if (all(metrics %in% "rmsse")) {

          data_1 <- data_1_rmsse

        } else {

          data_1$rmsse <- data_1_rmsse$rmsse
        }
      }
      #------------------------------------------------------------------------
      # Compute error metric by horizon and window across all validation windows.
      data_2 <- data_1 %>%
        dplyr::ungroup() %>%
        dplyr::group_by_at(dplyr::vars(.data$model, .data$model_forecast_horizon, .data$horizon, !!groups)) %>%
        dplyr::summarize_at(dplyr::vars(!!!rlang::syms(metrics)), aggregate, na.rm = TRUE)
      #------------------------------------------------------------------------
      # Compute error metric by model.
      data_3 <- data_1 %>%
        dplyr::ungroup() %>%
        dplyr::group_by_at(dplyr::vars(.data$model, .data$model_forecast_horizon, !!groups)) %>%
        dplyr::summarize_at(dplyr::vars(!!!rlang::syms(metrics)), aggregate, na.rm = TRUE)
    #--------------------------------------------------------------------------

    } else {  # Final forecasts from combine_forecasts().

      #------------------------------------------------------------------------
      # There are no validation windows to compute error metrics for.
      data_1 <- data.frame()
      #------------------------------------------------------------------------
      # Compute error metric by model and horizon across all validation windows.
      if (!all(metrics %in% "rmsse")) {

        if (type == "horizon") {

          data_2 <- data %>%
            dplyr::group_by_at(dplyr::vars(.data$model, .data$horizon, !!groups)) %>%
            dplyr::summarize_at(dplyr::vars(1),  # 1 is a col position that gets the fun to run; args x, y, and z defined below.
                                .funs = error_functions,
                                x = rlang::quo(.data$residual),
                                y = rlang::sym(outcome_name),
                                z = rlang::sym(paste0(outcome_name, "_pred")))
        }

        if (any(metrics %in% "rmsse")) {

          data_2_rmsse <- data %>%
            dplyr::group_by_at(dplyr::vars(.data$model, .data$horizon, !!groups)) %>%
            dplyr::summarize("h" = sum(!is.na(.data$residual)),
                             "sse_num" = sum(.data$residual^2, na.rm = TRUE),
                             "mse_denom" = .data$mse_denom[1])

          data_2_rmsse$rmsse <- with(data_2_rmsse, sqrt((1 / h) * (sse_num / mse_denom)))

          data_2_rmsse$h <- NULL
          data_2_rmsse$sse_num <- NULL
          data_2_rmsse$mse_denom <- NULL

          if (all(metrics %in% "rmsse")) {

            data_2 <- data_2_rmsse

          } else {

            data_2$rmsse <- data_2_rmsse$rmsse
          }
        }
      }
      #------------------------------------------------------------------------
      # Compute error metric by model.
      if (type == "horizon") {

        if (type == "horizon") {

          data_3 <- data %>%
            dplyr::group_by_at(dplyr::vars(.data$model, !!groups)) %>%
            dplyr::summarize_at(dplyr::vars(1),  # 1 is a col position that gets the fun to run; args x, y, and z defined below.
                                .funs = error_functions,
                                x = rlang::quo(.data$residual),
                                y = rlang::sym(outcome_name),
                                z = rlang::sym(paste0(outcome_name, "_pred")))
        }

        if (any(metrics %in% "rmsse")) {

          data_3_rmsse <- data %>%
            dplyr::group_by_at(dplyr::vars(.data$model, !!groups)) %>%
            dplyr::summarize("h" = sum(!is.na(.data$residual)),
                             "sse_num" = sum(.data$residual^2, na.rm = TRUE),
                             "mse_denom" = .data$mse_denom[1])

          data_3_rmsse$rmsse <- with(data_3_rmsse, sqrt((1 / h) * (sse_num / mse_denom)))

          data_3_rmsse$h <- NULL
          data_3_rmsse$sse_num <- NULL
          data_3_rmsse$mse_denom <- NULL

          if (all(metrics %in% "rmsse")) {

            data_3 <- data_3_rmsse

          } else {

            data_3$rmsse <- data_3_rmsse$rmsse
          }
        }
      }
      #------------------------------------------------------------------------
    }  # End error metrics for results from combine_forecasts().
  #----------------------------------------------------------------------------

  data_out <- list("error_by_window" = data_1, "error_by_horizon" = data_2, "error_global" = data_3)
  data_out[] <- lapply(data_out, as.data.frame)  # Remove the tibble class.

  if (methods::is(data_results, "training_results")) {  # Validation error.

      class(data_out) <- c("validation_error", class(data_out))

  } else {  # Forecast error

    if (!is_forecastML) {
      class(data_out) <- c("forecast_error", class(data_out))
    } else {
      class(data_out) <- c("forecastML_error", class(data_out))
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

  # Changing 'model_forecast_horizon' to 'horizon' for standardizing plot code with multi-output models.
  if (method == "direct") {

    data_results$horizon <- data_results$model_forecast_horizon
    data_results$model_forecast_horizon <- NULL
    names(data_plot)[names(data_plot) == "model_forecast_horizon"] <- "horizon"
  }

  if (is.null(horizons)) {

    horizons <- unique(data_results$horizon)
  }

  windows <- if (is.null(windows)) {unique(data_results$window_number)} else {windows}

  data_plot <- data_plot[data_plot$horizon %in% horizons & data_plot$model %in% models & data_plot$window_number %in% windows, ]

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
}  # nocov end
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#' Plot forecast error
#'
#' Plot forecast error at various levels of aggregation.
#' @param x An object of class 'validation_error' from \code{return_error()}.
#' @param data_results An object of class 'forecast_results' from \code{predict.forecast_model()}.
#' @param type Select plot type; \code{type = "window"} is the default plot.
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
plot.forecast_error <- function(x, data_results, type = c("window", "horizon", "global"), metric = NULL,
                                facet = NULL, models = NULL, horizons = NULL, windows = NULL, group_filter = NULL, ...) { # nocov start

  #----------------------------------------------------------------------------
  data_error <- x

  if (!methods::is(data_results, "forecast_results")) {
    stop("The 'data_results' argument takes an object of class 'forecast_results' as input. Run predict.forecast_model() first.")
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
    stop("The error metric in 'metric' is not in the forecast error dataset 'x'; re-run return_error() with this metric.")
  }
  #----------------------------------------------------------------------------
  # Set default plot facets for each plot type.
  if (is.null(facet)) {

    if (type == "window") {

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

  if (type == "window") {

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

  if (is.null(horizons)) {
    horizons <- unique(data_results$model_forecast_horizon)
  }

  windows <- if (is.null(windows)) {unique(data_results$window_number)} else {windows}

  data_plot <- data_plot[data_plot$model_forecast_horizon %in% horizons & data_plot$model %in% models & data_plot$window_number %in% windows, ]

  data_results <- data_results[data_results$model_forecast_horizon %in% horizons & data_results$model %in% models & data_results$window_number %in% windows, ]
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

  data_plot$model_forecast_horizon <- factor(data_plot$model_forecast_horizon, levels = unique(data_plot$model_forecast_horizon), ordered = TRUE)

  data_plot[, groups] <- lapply(seq_along(data_plot[, groups, drop = FALSE]), function(i) {
    factor(data_plot[, groups[i]], levels = unique(data_plot[, groups[i]]), ordered = TRUE)
  })

  data_plot$ggplot_color <- apply(data_plot[,  ggplot_color, drop = FALSE], 1, function(x) {paste(x, collapse = "-")})

  # Give error a name in the legend if plot is faceted by model and horizon (and group if groups are given).
  if (length(ggplot_color) == 0) {
    data_plot$ggplot_color <- "Error"
  }

  # Used to avoid lines spanning any gaps between validation windows.
  if (all(data_plot$window_number == 1)) {  # One window; no need to add the window number to the legend.

    data_plot$ggplot_group <- apply(data_plot[,  ggplot_color, drop = FALSE], 1, function(x) {paste(x, collapse = "-")})

  } else {

    data_plot$ggplot_group <- apply(data_plot[,  c("window_number", ggplot_color), drop = FALSE], 1, function(x) {paste(x, collapse = "-")})
  }

  # Coerce to viridis color scale with an ordered factor. With the data.frame sorted, unique() pulls the levels in their order of appearance.
  data_plot$ggplot_color <- factor(data_plot$ggplot_color, levels = unique(data_plot$ggplot_color), ordered = TRUE)

  data_plot$ggplot_group <- factor(data_plot$ggplot_group, levels = unique(data_plot$ggplot_group), ordered = TRUE)

  temp_1 <- unlist(Map(function(x) {toupper(substr(x[1], 1, 1))}, ggplot_color))
  temp_2 <- unlist(Map(function(x) {substr(x, 2, nchar(x))}, ggplot_color))
  x_axis_title <- paste(temp_1, temp_2, sep = "")
  x_axis_title <- paste(x_axis_title, collapse = " + ")

  #----------------------------------------------------------------------------
  # Create plots.
  if (type %in% c("window", "horizon")) {

    p <- ggplot()

    if (1 %in% horizons || nrow(data_plot) == 1 || (method == "multi_output" && any(c(facet_names %in% "horizon")))) {  # Use geom_point instead of geom_line to plot a 1-step-ahead forecast.

      if (method == "direct") {

        data_plot_temp <- data_plot[data_plot$model_forecast_horizon == 1, ]
        data_plot_temp$index <- data_plot_temp$horizon
        data_plot_temp$horizon <- NULL
        names(data_plot_temp)[names(data_plot_temp) == "model_forecast_horizon"] <- "horizon"  # Rename for faceting.

      } else if (method == "multi_output") {

        data_plot_temp <- data_plot
      }

      p <- p + geom_point(data = data_plot_temp,
                          aes(x = .data$index, y = .data$value,
                              color = .data$ggplot_color, group = .data$ggplot_group))
    }

    if ((method == "direct" && !all(horizons == 1)) || (method == "multi_output" && nrow(data_plot) > 1 && !any(c(facet_names %in% "horizon")))) {  # Plot forecasts for model forecast horizons > 1.

      if (method == "direct") {

        data_plot_temp <- data_plot[data_plot$model_forecast_horizon != 1, ]
        data_plot_temp$index <- data_plot_temp$horizon
        data_plot_temp$horizon <- NULL
        names(data_plot_temp)[names(data_plot_temp) == "model_forecast_horizon"] <- "horizon"  # Rename for faceting.

      } else if (method == "multi_output") {

        data_plot_temp <- data_plot
      }

      p <- p + geom_line(data = data_plot_temp,
                         aes(x = .data$index, y = .data$value,
                             color = .data$ggplot_color, group = .data$ggplot_group))
    }

    p <- p + scale_color_viridis_d()
    p <- p + theme_bw() + theme(panel.spacing = unit(0, "lines"))
    p <- p + facet_grid(facet, scales = "fixed")
    p <- p + xlab(x_axis_title) + ylab(paste0("Forecast error metric (", error_metrics, ")")) + labs(color = x_axis_title)
    if (type %in% c("window")) {
      p <- p + ggtitle("Forecast Error by Validation Window and Model Forecast Horizon")
    } else {
      p <- p + ggtitle("Forecast Error by Model Forecast Horizon")
    }

    return(suppressWarnings(p))
  }

  if (type == "global") {

    data_plot$horizon <- data_plot$model_forecast_horizon

    p <- ggplot()

    p <- p + geom_bar(data = data_plot,
                      aes(x = .data$ggplot_color,
                          y = .data$value,
                          fill = .data$ggplot_color,
                          group = .data$ggplot_group),
                      stat = "identity", position = position_dodge(width = 1))
    p <- p + scale_fill_viridis_d()
    p <- p + facet_grid(facet, scales = "free")
    p <- p + theme_bw() + theme(
      panel.spacing = unit(0, "lines"),
      axis.text.x = element_blank()
    )
    p <- p + xlab(x_axis_title) + ylab(paste0("Forecast error metric (", error_metrics, ")")) + labs(fill = x_axis_title, alpha = NULL) +
      ggtitle("Forecast Error Across Validation Windows and Forecast Horizons")

    return(suppressWarnings(p))
  }
} # nocov end

