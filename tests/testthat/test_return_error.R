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
