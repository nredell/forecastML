#------------------------------------------------------------------------------
# Test that train_model() works correctly.
library(forecastML)
library(dplyr)
library(glmnet)

test_that("train model works with fixed and ... model args", {

  # Sampled Seatbelts data from the R package datasets.
  data("data_seatbelts", package = "forecastML")

  # Example - Training data for 2 horizon-specific models w/ common lags per predictor.
  horizons <- c(1, 12)
  lookback <- 1:15

  data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                                 lookback = lookback, horizon = horizons)

  windows <- create_windows(data_train, window_length = 12)

  model_function <- function(data, my_outcome_col) {

    x <- data[, -(my_outcome_col), drop = FALSE]
    y <- data[, my_outcome_col, drop = FALSE]
    x <- as.matrix(x, ncol = ncol(x))
    y <- as.matrix(y, ncol = ncol(y))

    model <- glmnet::glmnet(x, y)
    return(model)
  }

  set.seed(224)
  model_results_1 <- train_model(data_train, windows, model_name = "LASSO", model_function,
                                 my_outcome_col = 1)

  model_function <- function(data, my_outcome_col = 1, my_unused_argument) {

    x <- data[, -(my_outcome_col), drop = FALSE]
    y <- data[, my_outcome_col, drop = FALSE]
    x <- as.matrix(x, ncol = ncol(x))
    y <- as.matrix(y, ncol = ncol(y))

    model <- glmnet::glmnet(x, y)
    return(model)
  }

  set.seed(224)
  model_results_2 <- train_model(data_train, windows, model_name = "LASSO", model_function)

  testthat::expect_equal(length(model_results_1), length(horizons))
  testthat::expect_equal(length(model_results_2), length(horizons))
  testthat::expect_equal(length(model_results_1[[1]]), nrow(windows))
  testthat::expect_equal(length(model_results_2[[1]]), nrow(windows))
  testthat::expect_true(methods::is(model_results_1$horizon_1$window_1$model, "glmnet"))  # Argument in ....
  testthat::expect_true(methods::is(model_results_2$horizon_1$window_1$model, "glmnet"))  # Hardcoded model args.
})
