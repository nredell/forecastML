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

  identical(data$dates, data_out$dates)
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

test_that("fill_gaps with grouped data is correct", {

  data("data_buoy", package = "forecastML")
  data("data_buoy_gaps", package = "forecastML")

  frequency <- "1 day"
  groups <- c("buoy_id")
  static_features <- c("lat", "lon")

  identical(data_buoy, forecastML::fill_gaps(data_buoy_gaps, date_col = 1,
                                             frequency = frequency, groups = groups,
                                             static_features = static_features))
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

  all(
    sum(is.na(data_test$date)) == 0,                # dates should not be missing
    sum(is.na(data_test[, dynamic_features])) > 0,  # dynamic features should be missing
    sum(is.na(data_test[, groups])) == 0,           # grouping features should not be missing
    sum(is.na(data_test[, static_features])) == 0   # static features should not be missing
    )
})

