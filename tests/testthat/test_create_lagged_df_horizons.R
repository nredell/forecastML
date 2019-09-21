#------------------------------------------------------------------------------
# Test that create_lagged_df() produces correct lags for training and forecasting
# datasets that have dates and are ungrouped.
library(forecastML)
library(dplyr)

test_that("lagged_df, training data, non-grouped with dates is correct", {

  #------------------------------------------------------------------------------
  # Create a simple data.frame with 1 feature.
  dates <- seq(as.Date("2015-01-01"), as.Date("2020-12-01"), by = "1 month")

  data <- data.frame(
    "outcome" = 1:length(dates),
    "feature" = 1:length(dates) * 2
  )

  # create_lagged_df(data_test) should equal the constructed data.
  data_test <- data
  #------------------------------------------------------------------------------
  # data is the ground truth dataset.
  data$outcome_lag_1 <- dplyr::lag(data$outcome, 1)

  data$feature_lag_1 <- dplyr::lag(data$feature, 1)

  data$feature <- NULL

  data <- data[complete.cases(data), ]
  #------------------------------------------------------------------------------

  data_out <- forecastML::create_lagged_df(data = data_test, type = "train",
                                           outcome_cols = 1, horizons = 1,
                                           lookback = 1, dates = dates,
                                           frequency = "1 month")

  data_out <- data.frame(data_out$horizon_1)

  identical(data, data_out)
})

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("lagged_df, forecasting data, non-grouped with dates is correct", {

  #------------------------------------------------------------------------------
  # Create a simple data.frame with 1 feature.
  dates <- seq(as.Date("2015-01-01"), as.Date("2020-12-01"), by = "1 month")

  data <- data.frame(
    "outcome" = 1:length(dates),
    "feature" = 1:length(dates) * 2
  )

  # create_lagged_df(data_test) should equal the constructed data.
  data_test <- data
  #------------------------------------------------------------------------------
  # data is the ground truth dataset.
  data$outcome_lag_1 <- dplyr::lag(data$outcome, 0)

  data$feature_lag_1 <- dplyr::lag(data$feature, 0)

  data$feature <- NULL

  data <- data[nrow(data), , drop = FALSE]

  data$outcome <- NULL

  attr(data, "row.names") <- 1L
  #------------------------------------------------------------------------------

  data_out <- forecastML::create_lagged_df(data = data_test, type = "forecast",
                                           outcome_cols = 1, horizons = 1,
                                           lookback = 1, dates = dates,
                                           frequency = "1 month")

  data_out <- data.frame(data_out$horizon_1)

  data_out[, c("index", "horizon")] <- NULL

  identical(data, data_out)
})

