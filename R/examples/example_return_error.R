\donttest{
# Sampled Seatbelts data from the R package datasets.
data("data_seatbelts", package = "forecastML")

# Example - Training data for 12 horizon-specific models w/ common lags per predictor.
horizons <- 1:12
lookback <- 1:15

data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_cols = 1,
                               lookback = lookback, horizon = horizons)

windows <- create_windows(data_train, window_length = 12)

# User-define model - LASSO
# The model takes in a data.frame with a target and predictors with exactly the same format as
# in create_lagged_df(). 'outcome_cols' is the column index of the target. The
# model returns a model object suitable for a predict-type function.
library(glmnet)
model_function <- function(data, outcome_cols = 1) {

  x <- data[, -(outcome_cols), drop = FALSE]
  y <- data[, outcome_cols, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))

  model <- glmnet::cv.glmnet(x, y)
  return(model)
}

model_results <- train_model(data_train, windows,
                             model_function, model_name = "LASSO")

# Sampled Seatbelts data from the R package datasets.
data("data_seatbelts", package = "forecastML")

# Example - Training data for 12 horizon-specific models w/ common lags per predictor.
horizons <- 1:12
lookback <- 1:15

data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_cols = 1,
                               lookback = lookback, horizon = horizons)

windows <- create_windows(data_train, window_length = 12)

# User-define model - LASSO
# The model takes in a data.frame with a target and predictors with exactly the same format as
# in create_lagged_df(). 'outcome_cols' is the column index of the target. The
# model returns a model object suitable for a predict-type function.
library(glmnet)
model_function <- function(data, outcome_cols = 1) {

  x <- data[, -(outcome_cols), drop = FALSE]
  y <- data[, outcome_cols, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))

  model <- glmnet::cv.glmnet(x, y)
  return(model)
}

model_results <- train_model(data_train, windows,
                             model_function, model_name = "LASSO")

# User-defined prediction function - LASSO
# The predict() wrapper takes two positional arguments. First,
# the returned model from the user-defined modeling function (model_function() above).
# Second, a data.frame of predictors--lagged predictors will be created automatically
# using create_lagged_df().
prediction_function <- function(model, data_features) {

  x <- as.matrix(data_features, ncol = ncol(data_features))

  data_pred <- data.frame("y_pred" = predict(model, x, s = "lambda.min"))
  return(data_pred)
}

# Predict on the validation datasets.
data_valid <- predict(model_results, prediction_function = list(prediction_function))

# Forecast error metrics for validation datasets.
data_error <- return_error(data_valid)
}
