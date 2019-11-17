#------------------------------------------------------------------------------
# Test that create_lagged_df() produces correct lags for training and forecasting
# datasets with custom feature lags in lookback_control.
library(forecastML)
library(dplyr)

test_that("lagged_df, non-grouped training data with lookback equaling lookback_control", {

  #------------------------------------------------------------------------------
  # Create a simple data.frame.
  dates <- seq(as.Date("2015-01-01"), as.Date("2020-12-01"), by = "1 month")

  data_test <- data.frame(
    "outcome" = 1:length(dates),
    "feature" = 1:length(dates) * 2,
    "feature_2" = 1:length(dates) * 3
  )
  #------------------------------------------------------------------------------

  lookback <- 3:4

  data_lookback <- forecastML::create_lagged_df(data = data_test, type = "train",
                                                outcome_col = 1, horizons = 3,
                                                lookback = lookback, dates = dates,
                                                frequency = "1 day")

  lookback_control <- rep(list(3:4), 3)

  data_lookback_control <- forecastML::create_lagged_df(data = data_test, type = "train",
                                                outcome_col = 1, horizons = 3,
                                                lookback_control = lookback_control, dates = dates,
                                                frequency = "1 day")

  # Coerce using data.frame to drop lagged_df attributes which will differ.
  expect_identical(data.frame(data_lookback$horizon_3), data.frame(data_lookback_control$horizon_3))
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("lagged_df, non-grouped forecasting data with lookback equaling lookback_control", {

  #------------------------------------------------------------------------------
  # Create a simple data.frame.
  dates <- seq(as.Date("2015-01-01"), as.Date("2020-12-01"), by = "1 month")

  data_test <- data.frame(
    "outcome" = 1:length(dates),
    "feature" = 1:length(dates) * 2,
    "feature_2" = 1:length(dates) * 3
  )
  #------------------------------------------------------------------------------

  lookback <- 3:4

  data_lookback <- forecastML::create_lagged_df(data = data_test, type = "forecast",
                                                outcome_col = 1, horizons = 3,
                                                lookback = lookback, dates = dates,
                                                frequency = "1 day")

  lookback_control <- rep(list(3:4), 3)

  data_lookback_control <- forecastML::create_lagged_df(data = data_test, type = "forecast",
                                                        outcome_col = 1, horizons = 3,
                                                        lookback_control = lookback_control, dates = dates,
                                                        frequency = "1 day")

  # Coerce using data.frame to drop lagged_df attributes which will differ.
  expect_identical(data.frame(data_lookback$horizon_3), data.frame(data_lookback_control$horizon_3))
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("lagged_df, training and forecasting data lookback_control skips groups and static and dynamic features", {

  data("data_buoy", package = "forecastML")

  frequency <- "1 day"
  groups <- c("buoy_id")
  static_features <- c("lat", "lon")
  dynamic_features <- c("day", "year")

  dates <- data_buoy$date
  data_buoy$date <- NULL
  #------------------------------------------------------------------------------

  lookback_control <- rep(list(3:4), ncol(data_buoy))

  data_train <- forecastML::create_lagged_df(data = data_buoy, type = "train",
                                                        outcome_col = 1, horizons = 3,
                                                        lookback_control = lookback_control,
                                                        dynamic_features = dynamic_features,
                                                        groups = groups,
                                                        static_features = static_features,
                                                        dates = dates,
                                                        frequency = frequency)

  data_forecast <- forecastML::create_lagged_df(data = data_buoy, type = "forecast",
                                             outcome_col = 1, horizons = 3,
                                             lookback_control = lookback_control,
                                             dynamic_features = dynamic_features,
                                             groups = groups,
                                             static_features = static_features,
                                             dates = dates,
                                             frequency = frequency)

  # Expect that the names of all non-lagged features are present and non-lagged w/ lookback_control.
  all(
    all(c(dynamic_features, groups, static_features) %in% names(data_train$horizon_3)),
    all(c(dynamic_features, groups, static_features) %in% names(data_forecast$horizon_3))
  )
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("lagged_df, training data lookback_control appropriately drops lagged features", {

  data("data_buoy", package = "forecastML")

  frequency <- "1 day"
  groups <- c("buoy_id")
  static_features <- c("lat", "lon")
  dynamic_features <- c("day", "year")

  dates <- data_buoy$date
  data_buoy$date <- NULL
  #------------------------------------------------------------------------------

  horizons <- 4

  # Features with a lag of 3 should be automatically removed.
  lookback_control <- rep(list(3:4), ncol(data_buoy))

  data_train <- forecastML::create_lagged_df(data = data_buoy, type = "train",
                                             outcome_col = 1, horizons = horizons,
                                             lookback_control = lookback_control,
                                             dynamic_features = dynamic_features,
                                             groups = groups,
                                             static_features = static_features,
                                             dates = dates,
                                             frequency = frequency)

  # Remove the sea_surface_temperature column
  lookback_control[ncol(data_buoy)] <- list(NULL)

  data_train_drop <- forecastML::create_lagged_df(data = data_buoy, type = "train",
                                             outcome_col = 1, horizons = horizons,
                                             lookback_control = lookback_control,
                                             dynamic_features = dynamic_features,
                                             groups = groups,
                                             static_features = static_features,
                                             dates = dates,
                                             frequency = frequency)

  # Expect that the names of implicitly and explicitly removed features are absent.
  all(
    !any(grepl("lag_3", names(data_train$horizon_4))),
    !any(grepl("sea_surface_temperature", names(data_train_drop$horizon_4)))
    )
})
