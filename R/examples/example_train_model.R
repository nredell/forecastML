\donttest{
# Sampled Seatbelts data from the R package datasets.
data("data_seatbelts", package = "forecastML")

# Example - Training data for 2 horizon-specific models w/ common lags per predictor.
horizons <- c(1, 12)
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

# View the results for the model (a) trained on the first horizon
# and (b) to be assessed on the first outer-loop validation window.
model_results[[1]][[1]]$model
}
