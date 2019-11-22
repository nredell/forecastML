#------------------------------------------------------------------------------
# Test that create_windows() produces the correct validation datasets.
library(forecastML)
library(dplyr)
library(lubridate)

test_that("create_windows with dates is correct", {

  # Create a simple data.frame with 1 feature.
  dates <- seq(as.Date("2015-01-01"), as.Date("2020-12-01"), by = "1 month")

  data <- data.frame(
    "outcome" = 1:length(dates),
    "feature" = 1:length(dates) * 2
  )

  # create_lagged_df(data_test) should equal the constructed data.
  data_test <- data

  lookback <- 1:12

  data_test <- forecastML::create_lagged_df(data_test, horizons = 3, lookback = lookback,
                                            dates = dates, frequency = "1 month")

  window_length <- 1

  windows <- forecastML::create_windows(data_test, window_length = window_length)

  identical(windows$start, windows$stop)
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("create_windows with the same start and stop date for 1 unit of time is correct", {

  # Create a simple data.frame with 1 feature.
  dates <- seq(as.Date("2015-01-01"), as.Date("2020-12-01"), by = "1 month")

  data <- data.frame(
    "outcome" = 1:length(dates),
    "feature" = 1:length(dates) * 2
  )

  # create_lagged_df(data_test) should equal the constructed data.
  data_test <- data

  lookback <- 1:12

  data_test <- forecastML::create_lagged_df(data_test, horizons = 3, lookback = lookback,
                                            dates = dates, frequency = "1 month")

  window_length <- 1

  windows <- forecastML::create_windows(data_test, window_length = window_length)

  identical(windows$start, windows$stop)
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("create_windows length and skip with dates is correct", {

  # Create a simple data.frame with 1 feature.
  dates <- seq(as.Date("2015-01-01"), as.Date("2020-12-01"), by = "1 month")

  data <- data.frame(
    "outcome" = 1:length(dates),
    "feature" = 1:length(dates) * 2
  )

  # create_lagged_df(data_test) should equal the constructed data.
  data_test <- data

  lookback <- 1:12

  data_test <- forecastML::create_lagged_df(data_test, horizons = 3, lookback = lookback,
                                            dates = dates, frequency = "1 month")

  window_start_true <- as.Date(c("2017-01-01", '2019-05-01'))
  window_stop_true <- as.Date(c("2018-03-01", '2019-07-01'))

  windows <- forecastML::create_windows(data_test, window_start = window_start_true,
                                        window_stop = window_stop_true)

  all(
    all(windows$start == window_start_true),
    all(windows$stop == window_stop_true)
  )
})
