#' Combine multiple horizon-specific forecast models to produce one forecast
#'
#' The horizon-specific models can either be combined to (a) produce final forecasts for only those
#' horizons at which they were trained (i.e., shorter-horizon models override longer-horizon models
#' when producing final short-horizon h-step-ahead forecasts) or (b) produce final forecasts using
#' any combination of horizon-specific models that minimized error over the validation/training dataset or
#' a holdout test dataset.
#'
#' @param ... One or more objects of class 'forecast_results' from running \code{predict.forecast_model()} on
#' an input forward-looking forecast dataset. These are the forecasts from the horizon-specific
#' direct forecasting models trained over the entire training dataset by setting \code{create_windows(..., window_length = 0)}.
#' If \code{type = 'horizon'}, 1 final h-step-ahead forecast is returned for each model passed in \code{...}.
#' @param type A character vector of length 1 that identifies the forecast combination method.
#' @param data_error Not implemented at present.
#' @return An S3 object of class 'forecastML' with final h-step-ahead forecasts.
#'
#'    \strong{Columns in returned 'forecast_results' data.frame:}
#'     \itemize{
#'       \item \code{model}: User-supplied model name in \code{train_model()}.
#'       \item \code{model_forecast_horizon}: The direct-forecasting time horizon that the model was trained on.
#'       \item \code{horizon}: Forecast horizons, 1:h, measured in dataset rows.
#'       \item \code{forecast_period}: The forecast period in row indices or dates. The forecast period starts at either \code{attributes(create_lagged_df())$data_stop + 1} for row indices or \code{attributes(create_lagged_df())$data_stop + 1 * frequency} for date indices.
#'       \item \code{"groups"}: If given, the user-supplied groups in \code{create_lagged_df()}.
#'       \item \code{"outcome_name"_pred}: The final forecasts.
#'       the user-supplied prediction function.
#'    }
#' @export
combine_forecasts <- function(..., type = c("horizon", "error"), data_error = list(NULL)) {

  data_forecast <- list(...)

  if (!all(unlist(lapply(data_forecast, function(x) {methods::is(x, "forecast_results")})))) {
    stop("One or more of the forecast datasets given in '...' is not an object of class 'forecast_results'.
         Run predict.forecast_model() on a forward-looking forecast dataset trained over a training dataset
         made with create_windows(window_length = 0).")
  }

  outcome_levels <- attributes(data_forecast[[1]])$outcome_levels

  #----------------------------------------------------------------------------
  # For factor outcomes, is the prediction a factor level or probability.
  if (!is.null(outcome_levels)) {
    factor_level <- if (any(names(data) %in% paste0(outcome_names, "_pred"))) {TRUE} else {FALSE}
    factor_prob <- !factor_level

    if (factor_prob) {
      stop("Forecast combinations are not currently available for class probability outcomes.")
    }
  }
  #----------------------------------------------------------------------------

  data_forecast <- dplyr::bind_rows(data_forecast)
  data_forecast <- dplyr::as_tibble(data_forecast)

  if (unique(data_forecast$window_length) != 0) {
    stop("Some models were trained using multiple validation windows. Retrain any final forecast models
         using create_windows(window_length = 0) before combining forecast models across horizons.")
  }

  type <- type[1]

  if (!type %in% c("horizon")) {  # List all available types here.
    stop("Select a forecast combination 'type' that is one of 'horizon', 'error' is not yet supported.")
  }

  if (type == "horizon") {

    model_forecast_horizons <- sort(unique(data_forecast$model_forecast_horizon))
    horizons <- sort(unique(data_forecast$horizon))
    forecast_period <- sort(unique(data_forecast$forecast_period))

    # Create a list of forecast horizons where each horizon-specific model will produce
    # a forecast. This is a greedy selection method where the final forecast is a combination of horizon-specific
    # models--the combination being that longer-term models only contribute forecasts that are not
    # already being contributed by shorter-term models.
    horizon_filter <- lapply(seq_along(model_forecast_horizons), function(i) {

      if (i == 1) {

        if (model_forecast_horizons[i] == 1) {

          x <- 1

        } else {

          x <- seq(1, model_forecast_horizons[i])
        }

      } else if (i < max(seq_along(model_forecast_horizons))) {

        x <- seq(model_forecast_horizons[i - 1] + 1, model_forecast_horizons[i], 1)

      } else {

        x <- seq(model_forecast_horizons[i - 1] + 1, model_forecast_horizons[i], 1)
      }
    })
    #--------------------------------------------------------------------------
    # Filter the results so that short-term forecasts from shorter-term horizon-specific models overwrite
    # short-term forecasts from longer-term horizon-specific models.
    data_forecast <- lapply(seq_along(model_forecast_horizons), function(i) {

      data_forecast[data_forecast$model_forecast_horizon == model_forecast_horizons[i] &
                    data_forecast$horizon %in% horizon_filter[[i]], ]
    })

    data_forecast <- dplyr::bind_rows(data_forecast)
    data_forecast <- dplyr::select(data_forecast, -.data$window_length, -.data$window_number)
    data_forecast <- dplyr::arrange(data_forecast, .data$model, .data$horizon)
    data_forecast <- as.data.frame(data_forecast)

    class(data_forecast) <- c("forecastML", class(data_forecast))

    return(data_forecast)
  }
}
