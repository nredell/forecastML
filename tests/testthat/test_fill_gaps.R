#------------------------------------------------------------------------------
# Test that fill_gaps() produces correct lags for training datasets.
library(forecastML)
library(dplyr)

test_that("fill_gaps with un-grouped data is correct", {

  # Create a simple data.frame with 1 feature.
  dates <- seq(as.Date("2015-01-01"), as.Date("2020-12-01"), by = "1 month")

  data <- data.frame(
    "dates" = dates,
    "outcome" = 1:length(dates),
    "feature" = 1:length(dates) * 2
  )

  # fill_gaps(data_test) should equal the constructed data.
  data_test <- data
  #------------------------------------------------------------------------------
  # data is the ground truth dataset.

  data_test <- data_test[-(c(2, 20:35, nrow(data_test) - 1)), ]
  #------------------------------------------------------------------------------

  data_out <- forecastML::fill_gaps(data_test, date_col = 1, frequency = "1 month")

  all(
    identical(data$dates, data_out$dates),  # Dates should be identical.
    all(any(is.na(data_out$outcome)), any(is.na(data_out$feature)))  # The outcome and non-static features should have NAs.
  )
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("fill_gaps with grouped data is correct", {

  data("data_buoy", package = "forecastML")
  data("data_buoy_gaps", package = "forecastML")

  frequency <- "1 day"
  groups <- c("buoy_id")
  static_features <- c("lat", "lon")

  data_buoy_filled <- forecastML::fill_gaps(data_buoy_gaps, date_col = 1,
                                            frequency = frequency, groups = groups,
                                            static_features = static_features)

  identical(data_buoy, data_buoy_filled)
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("fill_gaps column fills are correct", {

  data("data_buoy_gaps", package = "forecastML")

  frequency <- "1 day"
  groups <- c("buoy_id")
  static_features <- c("lat", "lon")
  dynamic_features <- c("day", "year")

  data_test <- forecastML::fill_gaps(data_buoy_gaps, date_col = 1,
                                             frequency = frequency, groups = groups,
                                             static_features = static_features)

  testthat::expect_equal(sum(is.na(data_test$date)), 0)
  testthat::expect_gt(sum(is.na(data_test[, dynamic_features])), 0)
  testthat::expect_equal(sum(is.na(data_test[, groups])), 0)
  testthat::expect_equal(sum(is.na(data_test[, static_features])), 0)
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("fill_gaps error messages work", {

  data("data_buoy_gaps", package = "forecastML")

  frequency <- "1 day"
  groups <- c("buoy_id")
  static_features <- c("lat", "lon")
  dynamic_features <- c("day", "year")

  data_buoy_gaps$date <- as.character(data_buoy_gaps$date)

  # Expect the date column to be of class 'Date'.
  expect_error(forecastML::fill_gaps(data_buoy_gaps, date_col = 1,
                                     frequency = frequency, groups = groups,
                                     static_features = static_features))

  data_buoy_gaps$date <- as.Date(data_buoy_gaps$date)

  # Expect error if the input data is not a data.frame.
  expect_error(forecastML::fill_gaps(as.matrix(data_buoy_gaps), date_col = 1,
                                     frequency = frequency, groups = groups,
                                     static_features = static_features))

  # Expect error if the date_col argument is out of bounds.
  expect_error(forecastML::fill_gaps(data_buoy_gaps, date_col = 1:2,
                                     frequency = frequency, groups = groups,
                                     static_features = static_features))

  # Expect error if there are missing dates.
  data_temp <- data_buoy_gaps
  data_temp$date[1] <- NA
  expect_error(forecastML::fill_gaps(data_temp, date_col = 1,
                                     frequency = frequency, groups = groups,
                                     static_features = static_features))

  # Expect error if the 'frequency' argument is does not work in base::seq().
  expect_error(forecastML::fill_gaps(data_buoy_gaps, date_col = 1,
                                     frequency = "1 decade", groups = groups,
                                     static_features = static_features))

  # Expect static_features to not work when groups are missing
  expect_error(forecastML::fill_gaps(data_buoy_gaps, date_col = 1,
                                     frequency = frequency,
                                     static_features = static_features))

  # Expect missing date arguments to error out.
  expect_error(forecastML::fill_gaps(data_buoy_gaps))
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
