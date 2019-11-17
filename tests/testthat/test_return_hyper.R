#------------------------------------------------------------------------------
# Test that return_hyper() works correctly.
library(forecastML)
library(dplyr)
library(glmnet)

test_that("return_hyper returns a data.frame of parameters with 1 row for each model.", {

  skip_on_covr()

  # Sampled Seatbelts data from the R package datasets.
  data("data_seatbelts", package = "forecastML")

  data_seatbelts <- data_seatbelts[, 1:2, drop = FALSE]

  # Example - Training data for 2 horizon-specific models w/ common lags per predictor.
  horizons <- c(1, 12)
  lookback <- c(1, 3, 6, 9, 12, 15)

  data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                                 lookback = lookback, horizon = horizons)

  windows <- create_windows(data_train, window_length = 0)

  model_function <- function(data, my_outcome_col) {

    x <- data[, -(my_outcome_col), drop = FALSE]
    y <- data[, my_outcome_col, drop = FALSE]
    x <- as.matrix(x, ncol = ncol(x))
    y <- as.matrix(y, ncol = ncol(y))

    model <- glmnet::cv.glmnet(x, y, nfolds = 3)
    return(model)
  }

  set.seed(224)
  model_results <- train_model(data_train, windows, model_name = "LASSO", model_function,
                               my_outcome_col = 1)

  model <- model_results

  hyper_function <- function(model) {

    lambda_min <- model$lambda.min
    lambda_1se <- model$lambda.1se

    data_hyper <- data.frame("lambda_min" = lambda_min, "lambda_1se" = lambda_1se)
    return(data_hyper)
  }

  data_hyper <- return_hyper(model_results, hyper_function)

  expect_true(is.data.frame(data_hyper))
  expect_true(all(c("lambda_min", "lambda_1se") %in% names(data_hyper)))
  expect_true(nrow(data_hyper) == nrow(windows) * length(horizons))
})
