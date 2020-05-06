#------------------------------------------------------------------------------
# Test that create_skeleton() preserves lagged_df objects
library(forecastML)
library(dplyr)

test_that("create_skeleton correctly preserves lagged_df objects", {

  data("data_seatbelts", package = "forecastML")

  data_lagged <- create_lagged_df(data_seatbelts, "train", "direct", horizons = 1:2,
                                  lookback = 1:3)

  data_skeleton <- create_skeleton(data_lagged)

  data_lagged_attr <- attributes(data_lagged)
  data_skeleton_attr <- attributes(data_skeleton)[!names(attributes(data_skeleton)) %in% "skeleton"]

  testthat::expect_identical(data_lagged_attr, data_skeleton_attr)
})
#------------------------------------------------------------------------------
