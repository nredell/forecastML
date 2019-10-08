#------------------------------------------------------------------------------
# Test that return_error() works correctly.
library(forecastML)
library(dplyr)
library(glmnet)

test_that("return_error produces correct mae and mape across aggregations", {

  # Sampled Seatbelts data from the R package datasets.
  data("data_seatbelts", package = "forecastML")

  # Example - Training data for 2 horizon-specific models w/ common lags per predictor.
  horizons <- c(12)
  lookback <- 1:15

  data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                                 lookback = lookback, horizon = horizons)

  windows <- create_windows(data_train, window_length = 12)

  model_function <- function(data, my_outcome_col) {

    x <- data[, -(my_outcome_col), drop = FALSE]
    y <- data[, my_outcome_col, drop = FALSE]
    x <- as.matrix(x, ncol = ncol(x))
    y <- as.matrix(y, ncol = ncol(y))

    model <- glmnet::cv.glmnet(x, y, nfolds = 3)
    return(model)
  }

  # my_outcome_col = 1 is passed in ... but could have been defined in model_function().
  model_results <- train_model(data_train, windows, model_name = "LASSO", model_function,
                               my_outcome_col = 1)

  prediction_function <- function(model, data_features) {

    x <- as.matrix(data_features, ncol = ncol(data_features))

    data_pred <- data.frame("y_pred" = predict(model, x, s = "lambda.min"))
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
