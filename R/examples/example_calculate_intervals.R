data("data_seatbelts", package = "forecastML")

data_train <- forecastML::create_lagged_df(data_seatbelts, type = "train", method = "direct",
                                           outcome_col = 1, lookback = 1:15, horizons = c(1, 6, 12))

windows <- forecastML::create_windows(data_train, window_length = 0)

model_fn <- function(data) {
  model <- lm(DriversKilled ~ ., data)
}

model_results <- forecastML::train_model(data_train, windows, model_name = "OLS", model_function = model_fn)

predict_fn <- function(model, data_features) {
  data_pred <- as.data.frame(predict(model, data_features))
}

data_fit <- predict(model_results, prediction_function = list(predict_fn), data = data_train)

residuals <- residuals(data_fit)

data_forecast <- forecastML::create_lagged_df(data_seatbelts, type = "forecast", method = "direct",
                                              outcome_col = 1, lookback = 1:15, horizons = c(1, 6, 12))

data_forecasts <- predict(model_results, prediction_function = list(predict_fn), data = data_forecast)

data_forecasts <- forecastML::combine_forecasts(data_forecasts)

set.seed(1)
data_forecasts <- forecastML::calculate_intervals(data_forecasts, residuals)

plot(data_forecasts)
