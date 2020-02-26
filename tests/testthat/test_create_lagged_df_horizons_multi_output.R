#------------------------------------------------------------------------------
# Test that create_lagged_df() produces correct lags for training and forecasting
# datasets that have dates and are ungrouped.
library(forecastML)
library(dplyr)

test_that("multi_output, lagged_df, training data, non-grouped with dates is correct", {

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
  outcome_1 <- dplyr::lead(data$outcome, 1)  # Horizon 1 outcome
  outcome_3 <- dplyr::lead(data$outcome, 3)  # Horizon 3 outcome
  outcome_lag_1 <- dplyr::lag(data$outcome, 1 - 1)
  outcome_lag_3 <- dplyr::lag(data$outcome, 3 - 1)

  feature_lag_1 <- dplyr::lag(data$feature, 1 - 1)
  feature_lag_3 <- dplyr::lag(data$feature, 3 - 1)

  data <- data.frame(outcome_1, outcome_3, outcome_lag_1, outcome_lag_3, feature_lag_1, feature_lag_3)
  #------------------------------------------------------------------------------

  data_out <- forecastML::create_lagged_df(data = data_test, type = "train", method = "multi_output",
                                           outcome_col = 1, horizons = c(1, 3),
                                           lookback = c(1, 3), dates = dates,
                                           frequency = "1 month")

  data_out <- data.frame(data_out$horizon_1_3)

  identical(data, data_out)
})

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("multi_output, lagged_df, forecasting data, non-grouped with dates is correct", {

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
  outcome_lag_1 <- data$outcome[nrow(data)]
  feature_lag_1 <- data$feature[nrow(data)]

  data <- data.frame(outcome_lag_1, feature_lag_1)
  #------------------------------------------------------------------------------

  data_out <- forecastML::create_lagged_df(data_test, type = "forecast", method = "multi_output",
                                           outcome_col = 1, horizons = c(1, 3),
                                           lookback = 1, dates = dates,
                                           frequency = "1 month")

  data_out <- data.frame(data_out$horizon_1_3)

  data_out_index <- data_out$index
  data_out_horizon <- data_out$horizon

  data_out[, c("index", "horizon")] <- NULL

  all(
    data_out_index == "2021-01-01, 2021-03-01", # Are the nested forecast date indices correct?
    data_out_horizon == "1, 3", # Are the nested forecast date indices correct?
    identical(data, data_out)
  )
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("multi_output, lagged_df, forecasting data, dynamic features are missing", {

  #------------------------------------------------------------------------------
  # Create a simple data.frame with 1 feature.
  dates <- seq(as.Date("2015-01-01"), as.Date("2020-12-01"), by = "1 month")

  data <- data.frame(
    "outcome" = 1:length(dates),
    "feature" = 1:length(dates) * 2
  )

  data_test <- data
  #------------------------------------------------------------------------------

  data_out_no_groups <- forecastML::create_lagged_df(data = data_test, type = "forecast", method = "multi_output",
                                                     outcome_col = 1, horizons = 3,
                                                     lookback = 3,
                                                     dynamic_features = "feature",
                                                     dates = dates,
                                                     frequency = "1 month")

  data_test$group <- "A"

  data_out_groups <- forecastML::create_lagged_df(data = data_test, type = "forecast", method = "multi_output",
                                                  outcome_col = 1, horizons = 3,
                                                  lookback = 3,
                                                  dynamic_features = "feature",
                                                  groups = "group",
                                                  dates = dates,
                                                  frequency = "1 month")

  data_out_no_groups <- data.frame(data_out_no_groups$horizon_3)
  data_out_groups <- data.frame(data_out_groups$horizon_3)

  all(is.na(data_out_no_groups$feature), is.na(data_out_groups$feature))
})
