# Sampled Seatbelts data from the R package datasets.
data("data_seatbelts", package = "forecastML")
#------------------------------------------------------------------------------
# Example 1 - Training data for 2 horizon-specific models w/ common lags per predictor.
horizons <- c(1, 12)
lookback <- 1:15

data <- data_seatbelts

data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_cols = 1,
                               horizons = horizons, lookback = lookback)
head(data_train[[length(horizons)]])

# Example 1 - Forecasting dataset
# The last 'nrow(data_seatbelts) - horizon' rows are automatically used from data_seatbelts.
data_forecast <- create_lagged_df(data_seatbelts, type = "forecast", outcome_cols = 1,
                                  horizons = horizons, lookback = lookback)
head(data_forecast[[length(horizons)]])

#------------------------------------------------------------------------------
# Example 2 - Training data for one 3-month horizon model w/ unique lags per predictor.
horizons <- 3
lookback <- list(c(3, 6, 9, 12), c(4:12), c(6:15), c(8))

data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_cols = 1,
                               horizons = horizons, lookback_control = lookback)
head(data_train[[length(horizons)]])
