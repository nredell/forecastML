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

  group <- "group"

  # create_lagged_df(data_test) should equal the constructed data.
  data_test <- data
  #------------------------------------------------------------------------------
  # data is the ground truth dataset.
  data <- data %>%
    dplyr::group_by(group) %>%
    dplyr::mutate("outcome_temp" = dplyr::lead(outcome, 1),
                  "outcome_lag_1" = dplyr::lag(outcome, 1 - 1),
                  "outcome_lag_2" = dplyr::lag(outcome, 2 - 1),
                  "outcome_lag_3" = dplyr::lag(outcome, 3 - 1),
                  "feature_lag_1" = dplyr::lag(feature, 1 - 1),
                  "feature_lag_2" = dplyr::lag(feature, 2 - 1),
                  "feature_lag_3" = dplyr::lag(feature, 3 - 1))

  data$feature <- NULL

  data$outcome <- data$outcome_temp

  data$outcome_temp <- NULL

  names(data)[1] <- "outcome_1"

  data <- as.data.frame(data)
  #------------------------------------------------------------------------------

  data_out <- forecastML::create_lagged_df(data = data_test, type = "train", method = "multi_output",
                                           outcome_col = 1, horizons = 1,
                                           lookback = 1:3, dates = dates,
                                           frequency = "1 month", groups = "group")

  data_out <- data.frame(data_out$horizon_1)

  # We passed an unsorted data.frame into create_lagged_df() and if we sort after the fact,
  # the results should match the output of create_lagged_df().
  data$dates <- dates
  data <- dplyr::arrange(data, !!rlang::sym(group), !!dates)
  data$dates <- NULL

  identical(data, data_out)
})

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# test_that("lagged_df, forecasting data, grouped with dates is correct", {
# #
# #   lookback <- 1:3
# #
# #   #------------------------------------------------------------------------------
# #   # Create a simple data.frame with 1 feature.
# #   dates <- seq(as.Date("2015-01-01"), as.Date("2020-12-01"), by = "1 month")
# #
# #   data <- data.frame(
# #     "outcome" = 1:length(dates),
# #     "group" = c(rep("A", 5), rep("B", 60), rep("C", 5), rep("B", 2)),
# #     "feature" = 1:length(dates) * 2
# #   )
# #
# #   data$date <- dates
# #
# #   # create_lagged_df(data_test) should equal the constructed data.
# #   data_test <- data
# #   #------------------------------------------------------------------------------
# #   # data is the ground truth dataset.
# #
# #   data <- data.frame(
# #     "group" = c("A", "B", "C"),
# #     "outcome_lag_1" = c(NA, 72, NA),
# #     "outcome_lag_2" = c(NA, 71, NA),
# #     "outcome_lag_3" = c(NA, NA, 70),
# #     "feature_lag_1" = c(NA, 144, NA),
# #     "feature_lag_2" = c(NA, 142, NA),
# #     "feature_lag_3" = c(NA, NA, 140)
# #   )
# #   #------------------------------------------------------------------------------
# #
# #   data_test <- forecastML::fill_gaps(data_test, date_col = 4, frequency = '1 month',
# #                                      groups = "group")
# #
# #   dates <- data_test$date
# #   data_test$date <- NULL
# #
# #   data_out <- forecastML::create_lagged_df(data = data_test, type = "forecast", method = "multi_output",
# #                                            outcome_col = 1, horizons = 1,
# #                                            lookback = lookback, dates = dates,
# #                                            frequency = "1 month", groups = "group")
# #
# #   data_out <- data.frame(data_out$horizon_1)
# #
# #   data_out[, c("index", "horizon")] <- NULL
# #
# #   all.equal(data, data_out)
#   TRUE
# })
