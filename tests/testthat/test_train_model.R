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

    model <- glmnet::cv.glmnet(x, y, nfolds = 3)
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

    model <- glmnet::cv.glmnet(x, y, nfolds = 3)
    return(model)
  }

  set.seed(224)
  model_results_2 <- train_model(data_train, windows, model_name = "LASSO", model_function)

  all(
    length(model_results_1) == length(horizons),  # Is the length of the nested list correct?
    length(model_results_2) == length(horizons),  # Is the length of the nested list correct?
    length(model_results_1[[1]]) == nrow(windows),  # 1 model for each validation window.
    length(model_results_2[[1]]) == nrow(windows),  # 1 model for each validation window.
    methods::is(model_results_1$horizon_1$window_1$model, "cv.glmnet"),  # Argument in ....
    methods::is(model_results_2$horizon_1$window_1$model, "cv.glmnet")  # Hardcoded model args.
  )
})
