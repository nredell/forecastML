# Sampled Seatbelts data from the R package datasets.
data("data_seatbelts", package = "forecastML")
#------------------------------------------------------------------------------
# Example 1 - Training data for 3 horizon-specific models w/ common lags per predictor.
horizons <- c(1, 6, 12)
lookback <- 1:15

data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                               lookback = lookback, horizon = horizons)
plot(data_train)
#------------------------------------------------------------------------------
# Example 2 - Training data for one 3-month horizon model w/ unique lags per predictor.
horizons <- 3
lookback <- list(c(3, 6, 9, 12), c(4:12), c(6:15), c(8))

data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
                               lookback_control = lookback, horizon = horizons)
plot(data_train)
