#------------------------------------------------------------------------------
# Test that return_error() works correctly.
library(forecastML)
library(dplyr)

test_that("return_error produces correct mae and mape across aggregations", {

  data("data_seatbelts", package = "forecastML")

  data_seatbelts <- data_seatbelts[, 1:2]

  horizons <- 3
  lookback <- 6

  data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                                 lookback = lookback, horizons = horizons)

  windows <- create_windows(data_train, window_length = 0)

  data = data_seatbelts

  model_function <- function(data, my_outcome_col) {

    x <- data[, -(my_outcome_col), drop = FALSE]
    y <- data[, my_outcome_col, drop = FALSE]
    x <- as.matrix(x, ncol = ncol(x))
    y <- as.matrix(y, ncol = ncol(y))

    model <- lm(y ~ x)
    return(model)
  }

  set.seed(224)
  model_results <- train_model(data_train, windows, model_name = "LASSO", model_function,
                               my_outcome_col = 1)

  prediction_function <- function(model, data_features) {

    x <- data_features

    data_pred <- data.frame("y_pred" = predict(model, x))
    return(data_pred)
  }

  # Predict on the validation datasets.
  data_valid <- predict(model_results, prediction_function = list(prediction_function),
                        data = data_train)

  data_valid$DriversKilled <- 10
  data_valid$DriversKilled_pred <- 20

  data_error <- return_error(data_valid)

  data_valid$DriversKilled <- 20
  data_valid$DriversKilled_pred <- 10

  data_error_2 <- return_error(data_valid)

  all(
    all(data_error$error_by_window$mae == 10),
    all(data_error$error_by_horizon$mae == 10),
    all(data_error$error_global$mae == 10),
    all(data_error$error_by_window$mape == 100),
    all(data_error$error_by_horizon$mape == 100),
    all(data_error$error_global$mape == 100),
    all(data_error_2$error_by_window$mape == 50),
    all(data_error_2$error_by_horizon$mape == 50),
    all(data_error_2$error_global$mape == 50)
  )
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("validation error, window_length = 0 and 1 horizon returns the same error metrics across all aggregations", {

  data("data_seatbelts", package = "forecastML")

  data_seatbelts <- data_seatbelts[, 1:2]

  horizons <- 3
  lookback <- 6

  data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                                 lookback = lookback, horizons = horizons)

  windows <- create_windows(data_train, window_length = 0)

  model_function <- function(data, my_outcome_col) {

    x <- data[, -(my_outcome_col), drop = FALSE]
    y <- data[, my_outcome_col, drop = FALSE]
    x <- as.matrix(x, ncol = ncol(x))
    y <- as.matrix(y, ncol = ncol(y))

    model <- lm(y ~ x)
    return(model)
  }

  set.seed(224)
  model_results <- train_model(data_train, windows, model_name = "LASSO", model_function,
                               my_outcome_col = 1)

  prediction_function <- function(model, data_features) {

    x <- data_features

    data_pred <- data.frame("y_pred" = predict(model, x))
    return(data_pred)
  }

  # Predict on the validation datasets.
  data_valid <- predict(model_results, prediction_function = list(prediction_function),
                        data = data_train)

  data_error <- return_error(data_valid)

  error_metrics <- c("mae", "mape", "mdape", "smape")

  data_error <- lapply(data_error, function(x) {
    x[, error_metrics]
  })

  expect_mapequal(data_error[[1]], data_error[[2]])
  expect_mapequal(data_error[[1]], data_error[[3]])
  expect_mapequal(data_error[[2]], data_error[[3]])
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("test/forecast error, 1 horizon returns the 2 data.frames of error metrics", {

  data("data_seatbelts", package = "forecastML")

  data_seatbelts <- data_seatbelts[, 1:2]

  horizons <- 3
  lookback <- 6

  data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                                 lookback = lookback, horizons = horizons)

  windows <- create_windows(data_train, window_length = 0)

  model_function <- function(data, my_outcome_col) {

    model <- lm(DriversKilled ~ ., data = data)
    return(model)
  }

  set.seed(224)
  model_results <- train_model(data_train, windows, model_name = "LASSO", model_function,
                               my_outcome_col = 1)

  prediction_function <- function(model, data_features) {

    x <- data_features

    data_pred <- data.frame("y_pred" = predict(model, newdata = x))
    return(data_pred)
  }

  data_forecast <- create_lagged_df(data_seatbelts, type = "forecast", outcome_col = 1,
                                    lookback = lookback, horizons = horizons)

  # Forecast
  data_forecasts <- predict(model_results, prediction_function = list(prediction_function),
                            data = data_forecast)

  test_indices <- 192:203
  data_test <- data_seatbelts[test_indices - 11, ]

  data_error <- return_error(data_forecasts, data_test = data_test, test_indices = test_indices)

  all(
    nrow(data_error[[1]]) == 0,
    nrow(data_error[[2]]) == horizons,
    nrow(data_error[[3]]) == 1
      )
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("return_error returns a data.frame when the input is 1 model from combine_forecasts(type = 'horizon')", {

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

  set.seed(224)
  data_test <- data.frame("DriversKilled" = runif(12, 110, 160), "index" = 193:(193 + 11))
  test_indices <- data_test$index

  data_error_combined <- return_error(data_combined, data_test, test_indices)

  all(
    methods::is(data_error_combined, "data.frame")
  )
})
