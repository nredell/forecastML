\donttest{
# Sampled Seatbelts data from the R package datasets.
data("data_seatbelts", package = "forecastML")

# Example - Training data for 12 horizon-specific models w/ common lags per predictor.
horizons <- 1:12
lookback <- 1:15

data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_cols = 1,
                               lookback = lookback, horizon = horizons)

create_windows(data_train, window_length = 12)
}
