#------------------------------------------------------------------------------
# Test that create_lagged_df() produces correct lags for training and forecasting
# datasets that have dates and are ungrouped.
library(forecastML)
library(dplyr)

test_that("lagged_df, training data, grouped with dates is correct", {

  #------------------------------------------------------------------------------
  # Create a simple data.frame with 1 feature.
  dates <- seq(as.Date("2015-01-01"), as.Date("2020-12-01"), by = "1 month")

  data <- data.frame(
    "outcome" = 1:length(dates),
    "group" = c(rep("A", 5), rep("B", 60), rep("C", 5), rep("B", 2)),
    "feature" = 1:length(dates) * 2
  )

  # create_lagged_df(data_test) should equal the constructed data.
  data_test <- data
  #------------------------------------------------------------------------------
  # data is the ground truth dataset.
  data <- data %>%
    dplyr::group_by(group) %>%
    dplyr::mutate("outcome_lag_1" = dplyr::lag(outcome, 1),
                  "outcome_lag_2" = dplyr::lag(outcome, 2),
                  "outcome_lag_3" = dplyr::lag(outcome, 3),
                  "feature_lag_1" = dplyr::lag(feature, 1),
                  "feature_lag_2" = dplyr::lag(feature, 2),
                  "feature_lag_3" = dplyr::lag(feature, 3))

  data$feature <- NULL

  data <- as.data.frame(data)
  #------------------------------------------------------------------------------

  data_out <- forecastML::create_lagged_df(data = data_test, type = "train",
                                           outcome_col = 1, horizons = 1,
                                           lookback = 1:3, dates = dates,
                                           frequency = "1 month", groups = "group")

  data_out <- data.frame(data_out$horizon_1)

  identical(data, data_out)
})

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("lagged_df, forecasting data, grouped with dates is correct", {

  lookback <- 1:3

  #------------------------------------------------------------------------------
  # Create a simple data.frame with 1 feature.
  dates <- seq(as.Date("2015-01-01"), as.Date("2020-12-01"), by = "1 month")

  data <- data.frame(
    "outcome" = 1:length(dates),
    "group" = c(rep("A", 5), rep("B", 60), rep("C", 5), rep("B", 2)),
    "feature" = 1:length(dates) * 2
  )

  data$date <- dates

  # create_lagged_df(data_test) should equal the constructed data.
  data_test <- data
  #------------------------------------------------------------------------------
  # data is the ground truth dataset.

  data <- data.frame(
    "group" = c("A", "B", "C"),
    "outcome_lag_1" = c(NA, 72, NA),
    "outcome_lag_2" = c(NA, 71, NA),
    "outcome_lag_3" = c(NA, NA, 70),
    "feature_lag_1" = c(NA, 144, NA),
    "feature_lag_2" = c(NA, 142, NA),
    "feature_lag_3" = c(NA, NA, 140)
  )
  #------------------------------------------------------------------------------

  data_test <- forecastML::fill_gaps(data_test, date_col = 4, frequency = '1 month',
                                     groups = "group")

  dates <- data_test$date
  data_test$date <- NULL

  data_out <- forecastML::create_lagged_df(data = data_test, type = "forecast",
                                           outcome_col = 1, horizons = 1,
                                           lookback = lookback, dates = dates,
                                           frequency = "1 month", groups = "group")

  data_out <- data.frame(data_out$horizon_1)

  data_out[, c("index", "horizon")] <- NULL

  all.equal(data, data_out)
})
