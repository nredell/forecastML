#------------------------------------------------------------------------------
# Test that return_hyper() works correctly.
library(forecastML)
library(dplyr)

test_that("return_hyper returns a data.frame of parameters with 1 row for each model.", {

  set.seed(224)
  data_seatbelts <- data.frame("y" = rnorm(1:200))

  #data_seatbelts <- data_seatbelts[, 1:2, drop = FALSE]

  # Example - Training data for 2 horizon-specific models w/ common lags per predictor.
  horizons <- c(1, 12)
  lookback <- c(1, 3, 6, 9, 12, 15)

  data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                                 lookback = lookback, horizon = horizons)

  windows <- create_windows(data_train, window_length = 0)

  model_function <- function(data) {

    model <- lm(y ~ ., data = data)

    return(model)
  }

  set.seed(224)
  model_results <- train_model(data_train, windows, model_name = "LM", model_function)

  hyper_function <- function(model) {

    # Hardcoded because we are interested in the function return, no the model parameters.
    data_hyper <- data.frame("test" = 1)

    return(data_hyper)
  }

  data_hyper <- return_hyper(model_results, hyper_function)

  expect_true(is.data.frame(data_hyper))
  expect_true(all(c("test") %in% names(data_hyper)))
  expect_true(nrow(data_hyper) == nrow(windows) * length(horizons))
})
