#------------------------------------------------------------------------------
# Test that combine_forecasts() works correctly.
library(forecastML)
library(dplyr)

test_that("combine forecasts works with 1 model and type equals horizon", {

  # Sampled Seatbelts data from the R package datasets.
  data("data_seatbelts", package = "forecastML")

  horizons <- c(1, 12)
  lookback <- 1:15

  data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                                 lookback = lookback, horizon = horizons)

  windows <- create_windows(data_train, window_length = 0)

  model_function <- function(data, my_outcome_col) {
    model <- lm(DriversKilled ~ ., data = data)
    return(model)
  }

  set.seed(224)
  model_results <- train_model(data_train, windows, model_name = "LASSO", model_function)

  data_forecast <- create_lagged_df(data_seatbelts, type = "forecast", outcome_col = 1,
                                    lookback = lookback, horizon = horizons)

  prediction_function <- function(model, data_features) {

    x <- data_features

    data_pred <- data.frame("y_pred" = predict(model, newdata = x))
    return(data_pred)
  }

  data_forecasts <- predict(model_results, prediction_function = list(prediction_function), data = data_forecast)

  data_combined <- combine_forecasts(data_forecasts)

  all(
    max(horizons) == nrow(data_combined),  # Are the correct number of h-step-ahead forecasts produced?
    all(data_combined$horizon <= data_combined$model_forecast_horizon),  # Are all forecasts produced possible given the model horizon?
    # Are the horizon-specific forecasts the same?
    identical(data_forecasts[1, "DriversKilled_pred"], data_combined$DriversKilled_pred[1]),
    identical(data_forecasts[3:(nrow(data_forecasts)), "DriversKilled_pred"], data_combined$DriversKilled_pred[2:(max(horizons))])
    )

  testthat::expect_error(combine_forecasts(model_results))  # Input is not of class 'forecast_results'.
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("combine forecasts works with multiple models and type equals horizon", {

  data("data_seatbelts", package = "forecastML")

  horizons <- c(1, 12)
  lookback <- 1:15

  data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                                 lookback = lookback, horizon = horizons)

  windows <- create_windows(data_train, window_length = 0)

  model_function <- function(data, my_outcome_col) {
    model <- lm(DriversKilled ~ ., data = data)
    return(model)
  }

  set.seed(224)
  model_results_1 <- train_model(data_train, windows, model_name = "LASSO", model_function)
  model_results_2 <- train_model(data_train, windows, model_name = "LASSO 2", model_function)

  data_forecast <- create_lagged_df(data_seatbelts, type = "forecast", outcome_col = 1,
                                    lookback = lookback, horizon = horizons)

  prediction_function <- function(model, data_features) {

    x <- data_features

    data_pred <- data.frame("y_pred" = predict(model, newdata = x))
    return(data_pred)
  }

  data_forecasts_1 <- predict(model_results_1, prediction_function = list(prediction_function),
                              data = data_forecast)

  data_forecasts_2 <- predict(model_results_2, prediction_function = list(prediction_function),
                              data = data_forecast)

  data_combined <- combine_forecasts(data_forecasts_1, data_forecasts_2)

  all(
    (max(horizons) * 2) == nrow(data_combined),  # Are the correct number of h-step-ahead forecasts produced?
    all(data_combined$horizon <= data_combined$model_forecast_horizon),  # Are all forecasts produced possible given the model horizon?
    # Are the horizon-specific forecasts the same?
    identical(data_forecasts_1[1, "DriversKilled_pred"], data_combined$DriversKilled_pred[1]),
    identical(data_forecasts_1[3:(nrow(data_forecasts_1)), "DriversKilled_pred"], data_combined$DriversKilled_pred[2:(max(horizons))])
  )

  testthat::expect_error(combine_forecasts(model_results))  # Input is not of class 'forecast_results'.
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
