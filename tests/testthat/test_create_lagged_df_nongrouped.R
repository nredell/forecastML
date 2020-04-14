#------------------------------------------------------------------------------
# Test that create_lagged_df() produces correct lags for training and forecasting
# datasets that have dates and are ungrouped.
library(forecastML)
library(dplyr)

test_that("lagged_df, forecasting data, non-grouped with daily date horizons are correct", {

  #------------------------------------------------------------------------------
  # Create a simple data.frame with 1 feature.
  dates <- seq(as.Date("2019-01-01"), as.Date("2020-12-01"), by = "1 day")

  # create_lagged_df(data_test) should equal the constructed data.
  data_test <- data.frame(
    "outcome" = 1:length(dates),
    "feature" = 1:length(dates) * 2
  )
  #------------------------------------------------------------------------------
  # data is the ground truth dataset.

  data <- data.frame(
    "index" = c("2020-12-02", "2020-12-03", "2020-12-04"),
    "horizon" = 1:3
  )

  data$index <- as.Date(data$index)
  #------------------------------------------------------------------------------

  data_out <- forecastML::create_lagged_df(data = data_test, type = "forecast",
                                           outcome_col = 1, horizons = 3,
                                           lookback = 3:4, dates = dates,
                                           frequency = "1 day")

  data_out <- data.frame(data_out$horizon_3[, c("index", "horizon")])

  all.equal(data, data_out)
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("lagged_df, forecasting data, non-grouped with monthly date horizons are correct", {

  #------------------------------------------------------------------------------
  # Create a simple data.frame with 1 feature.
  dates <- seq(as.Date("2019-01-01"), as.Date("2020-12-01"), by = "1 month")

  # create_lagged_df(data_test) should equal the constructed data.
  data_test <- data.frame(
    "outcome" = 1:length(dates),
    "feature" = 1:length(dates) * 2
  )
  #------------------------------------------------------------------------------
  # data is the ground truth dataset.

  data <- data.frame(
    "index" = c("2021-01-01", "2021-02-01", "2021-03-01"),
    "horizon" = 1:3
  )

  data$index <- as.Date(data$index)
  #------------------------------------------------------------------------------

  data_out <- forecastML::create_lagged_df(data = data_test, type = "forecast",
                                           outcome_col = 1, horizons = 3,
                                           lookback = 3:4, dates = dates,
                                           frequency = "1 month")

  data_out <- data.frame(data_out$horizon_3[, c("index", "horizon")])

  all.equal(data, data_out)
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("lagged_df, forecasting data, non-grouped with incorrect/missing args throws errors", {

  #------------------------------------------------------------------------------
  # Create a simple data.frame with 1 feature.
  dates <- seq(as.Date("2019-01-01"), as.Date("2020-12-01"), by = "1 month")

  # create_lagged_df(data_test) should equal the constructed data.
  data_test <- data.frame(
    "outcome" = 1:length(dates),
    "feature" = 1:length(dates) * 2
  )
  #------------------------------------------------------------------------------

  horizons <- 7
  lookback <- 3

  # Expect that feature lags support direct forecasting to the given horizon
  expect_error(forecastML::create_lagged_df(data = data_test, type = "forecast",
                                            outcome_col = 1, horizons = horizons,
                                            lookback = lookback, dates = dates,
                                            frequency = "1 month"))

  horizons <- 7
  lookback <- 10
  dates_test <- as.character(dates)

  # Expect that a non-date vector of throws an error.
  expect_error(forecastML::create_lagged_df(data = data_test, type = "forecast",
                                            outcome_col = 1, horizons = horizons,
                                            lookback = lookback, dates = dates_test,
                                            frequency = "1 month"))

  # Expect that specifying both lookback and lookback control throws an error.
  expect_error(forecastML::create_lagged_df(data = data_test, type = "forecast",
                                            outcome_col = 1, horizons = horizons,
                                            lookback = lookback, lookback_control = lookback,
                                            dates = dates,
                                            frequency = "1 month"))
})


