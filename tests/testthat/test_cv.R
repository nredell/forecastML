#------------------------------------------------------------------------------
# Cross-validation indices are correct for dates and indices from input data to lagged_df to predict.
library(forecastML)
library(dplyr)

test_that("cross-validation indices are correct for dates and indices from input data to lagged_df to predict", {

  data("data_seatbelts", package = "forecastML")

  # Example - Training data for 2 horizon-specific models w/ common lags per predictor.
  horizons <- c(1, 12)
  lookback <- c(1, 3, 6, 12, 15)

  dates <- seq(as.Date("1969-01-01"), as.Date("1984-12-01"), by = "1 month")

  data_no_dates_discrard_rows <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                                                  lookback = lookback, horizon = horizons)

  data_dates_discrard_rows <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                                               lookback = lookback, horizon = horizons,
                                               dates = dates, frequency = "1 month")

  data_no_dates_keep_rows <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                                              lookback = lookback, horizon = horizons,
                                              keep_rows = TRUE)

  data_dates_keep_rows <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                                               lookback = lookback, horizon = horizons,
                                               dates = dates, frequency = "1 month", keep_rows = TRUE)

  windows_1 <- create_windows(data_no_dates_discrard_rows, window_length = 12)
  windows_2 <- create_windows(data_dates_discrard_rows, window_length = 12)
  windows_3 <- create_windows(data_no_dates_keep_rows, window_length = 12)
  windows_4 <- create_windows(data_dates_keep_rows, window_length = 12)


  model_function <- function(data) {

    model <- 224  # The model will simply return a constant to get the code to run.

    return(model)
  }

  model_results_1 <- train_model(data_no_dates_discrard_rows, windows_1, model_name = "constant", model_function)
  model_results_2 <- train_model(data_dates_discrard_rows, windows_2, model_name = "constant", model_function)
  model_results_3 <- train_model(data_no_dates_keep_rows, windows_3, model_name = "constant", model_function)
  model_results_4 <- train_model(data_dates_keep_rows, windows_4, model_name = "constant", model_function)

  predict_fun <- function(model, data) {
    return(data.frame(224))
  }

  pred_1 <- predict(model_results_1, prediction_function = list(predict_fun), data = data_no_dates_discrard_rows)
  pred_2 <- predict(model_results_2, prediction_function = list(predict_fun), data = data_dates_discrard_rows)
  pred_3 <- predict(model_results_3, prediction_function = list(predict_fun), data = data_no_dates_keep_rows)
  pred_4 <- predict(model_results_4, prediction_function = list(predict_fun), data = data_dates_keep_rows)

  # Test that the indices are the same for all combinations of keep_rows = TRUE/FALSE and row/date indices.
  testthat::expect_equal(length(attributes(data_no_dates_discrard_rows)$row_indices), length(attributes(data_dates_discrard_rows)$row_indices))
  testthat::expect_equal(length(attributes(data_dates_discrard_rows)$row_indices), length(attributes(data_dates_discrard_rows)$date_indices))
  testthat::expect_equal(length(attributes(data_no_dates_keep_rows)$row_indices), length(attributes(data_dates_keep_rows)$row_indices))
  testthat::expect_equal(length(attributes(data_dates_keep_rows)$row_indices), length(attributes(data_dates_keep_rows)$date_indices))

  testthat::expect_true(all(dates == attributes(data_dates_keep_rows)$date_indices))

  testthat::expect_true(all(unique(pred_1$valid_indices) == attributes(data_no_dates_discrard_rows)$row_indices))
  testthat::expect_true(all(unique(pred_2$valid_indices) == attributes(data_dates_discrard_rows)$row_indices))
  testthat::expect_true(all(unique(pred_2$date_indices) == attributes(data_dates_discrard_rows)$date_indices))
  testthat::expect_true(all(unique(pred_3$valid_indices) == attributes(data_no_dates_keep_rows)$row_indices))
  testthat::expect_true(all(unique(pred_4$valid_indices) == attributes(data_no_dates_keep_rows)$row_indices))
  testthat::expect_true(all(unique(pred_4$date_indices) == attributes(data_dates_keep_rows)$date_indices))
  testthat::expect_true(all(unique(pred_4$row_indices) == as.numeric(row.names(data))))
  testthat::expect_true(all(unique(pred_4$date_indices) == dates))
})
