## ---- eval = FALSE, echo = FALSE, out.extra = 'style = "position:absolute; top:0; right:0; padding:5px; width: 150px; height: 160px;"'----
#  # htmltools::img(src = knitr::image_uri("forecastML_logo.png"),
#  #                alt = 'logo',
#  #                style = 'position:absolute; top:0; right:0; padding:5px; width: 150px; height: 160px;')
#  #knitr::include_graphics("forecastML_logo.png")

## ---- eval = FALSE, echo = FALSE, out.width="600px", out.height="400px"----
#  knitr::include_graphics("direct_forecasting.gif")

## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 4)

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("forecastML")

## ---- warning = FALSE, message = FALSE-----------------------------------
library(dplyr)
library(ggplot2)
library(forecastML)
library(glmnet)
library(randomForest)
library(DT)

data("data_seatbelts", package = "forecastML")
data <- data_seatbelts

data <- data[, c("DriversKilled", "kms", "PetrolPrice", "law")]
DT::datatable(head(data, 5))

## ------------------------------------------------------------------------
ts_frequency <- 12  # monthly time-series

data_train <- data[1:(nrow(data) - ts_frequency), ]
data_test <- data[(nrow(data) - ts_frequency + 1):nrow(data), ]

p <- ggplot(data, aes(x = 1:nrow(data), y = DriversKilled))
p <- p + geom_line()
p <- p + geom_vline(xintercept = nrow(data_train), color = "red", size = 1.1)
p <- p + theme_bw() + xlab("Index")
p

## ------------------------------------------------------------------------
horizons <- 1:ts_frequency
lookback <- 1:15

data_list <- forecastML::create_lagged_df(data_train, type = "train",
                                          outcome_cols = 1, lookback = lookback,
                                          horizons = horizons)

## ------------------------------------------------------------------------
DT::datatable(head(data_list[[6]], 10), options = list(scrollX = TRUE))

## ------------------------------------------------------------------------
plot(data_list)

## ---- warnings = FALSE, message = FALSE----------------------------------
windows <- forecastML::create_windows(lagged_df = data_list, window_length = 12, skip = 0,
                                      window_start = NULL, window_stop = NULL,
                                      include_partial_window = TRUE)
windows

## ------------------------------------------------------------------------
plot(windows, data_list, show_labels = TRUE)

## ------------------------------------------------------------------------
# Example 1 - LASSO
model_function <- function(data, outcome_cols = 1) {

  x <- data[, -(outcome_cols), drop = FALSE]
  y <- data[, outcome_cols, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))

  model <- glmnet::cv.glmnet(x, y)
  return(model)
}

# Example 2 - Random Forest
model_function_2 <- function(data, outcome_cols = 1) {

  outcome_names <- names(data)[outcome_cols]
  model_formula <- formula(paste0(outcome_names,  "~ ."))

  model <- randomForest::randomForest(formula = model_formula, data = data, ntree = 200)
  return(model)
}

## ------------------------------------------------------------------------
model_results <- forecastML::train_model(lagged_df = data_list, windows, 
                                         model_function, model_name = "LASSO")
model_results_2 <- forecastML::train_model(lagged_df = data_list, windows, 
                                           model_function_2, model_name = "RF")

## ------------------------------------------------------------------------
# Example 1 - LASSO
prediction_function <- function(model, data_features) {

  x <- as.matrix(data_features, ncol = ncol(data_features))

  data_pred <- data.frame("y_pred" = predict(model, x, s = "lambda.min"))
  return(data_pred)
}

# Example 2 - Random Forest
prediction_function_2 <- function(model, data_features) {

  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}

## ------------------------------------------------------------------------
data_results <- predict(model_results, model_results_2,
                        prediction_function = list(prediction_function, prediction_function_2))


## ------------------------------------------------------------------------
DT::datatable(head(data_results, 10), options = list(scrollX = TRUE))

## ------------------------------------------------------------------------
plot(data_results, type = "prediction", horizons = c(1, 6, 12))

## ------------------------------------------------------------------------
plot(data_results, type = "residual", horizons = c(1, 6, 12), windows = 10:14)

## ------------------------------------------------------------------------
plot(data_results, type = "forecast_stability", windows = max(data_results$window_number))
plot(data_results, type = "forecast_stability", valid_indices = attributes(data_list)$row_indices[1:3])

## ------------------------------------------------------------------------
plot(data_results, type = "forecast_variability", valid_indices = 30:80)

## ------------------------------------------------------------------------
data_error <- forecastML::return_error(data_results, metrics = c("mae", "mape", "smape"),
                                       models = NULL)

DT::datatable(data_error$error_global, options = list(scrollX = TRUE))

## ------------------------------------------------------------------------
plot(data_error, data_results, type = "time", horizons = c(1, 6, 12), windows = 10:14)

## ------------------------------------------------------------------------
plot(data_error, data_results, type = "horizon", horizons = c(1, 6, 12))

## ------------------------------------------------------------------------
plot(data_error, data_results, type = "global")

## ------------------------------------------------------------------------
hyper_function <- function(model) {

  lambda_min <- model$lambda.min
  lambda_1se <- model$lambda.1se

  data_hyper <- data.frame("lambda_min" = lambda_min, "lambda_1se" = lambda_1se)
  return(data_hyper)
}

## ------------------------------------------------------------------------
data_hyper <- forecastML::return_hyper(model_results, hyper_function)

plot(data_hyper, data_results, data_error, type = "stability", horizons = c(1, 6, 12))
plot(data_hyper, data_results, data_error, type = "error", c(1, 6, 12))

## ------------------------------------------------------------------------
data_forecast_list <- forecastML::create_lagged_df(data_train, type = "forecast", 
                                                  lookback = lookback,  horizon = horizons)

DT::datatable(head(data_forecast_list$horizon_6), options = list(scrollX = TRUE))

## ------------------------------------------------------------------------
data_forecast <- predict(model_results, model_results_2,
                         prediction_function = list(prediction_function, prediction_function_2), 
                         data_forecast = data_forecast_list)

DT::datatable(head(data_forecast, 10), options = list(scrollX = TRUE))

## ------------------------------------------------------------------------
plot(data_forecast, data_actual = data_train[-(1:150), ],
     actual_indices = as.numeric(row.names(data_train[-(1:150), ])),
     horizons = c(1, 6, 12), facet_plot = c("model", "model_forecast_horizon"))

plot(data_forecast, data_actual = data_test, 
     actual_indices = as.numeric(row.names(data_test)),
     facet_plot = "model", horizons = c(1, 6, 12))

## ------------------------------------------------------------------------
data_error <- forecastML::return_error(data_forecast, data_test = data_test,
                                       test_indices = as.numeric(row.names(data_test)),
                                       metrics = c("mae", "mape", "smape", "mdape"))

DT::datatable(head(data_error$error_by_horizon, 10), options = list(scrollX = TRUE))

## ------------------------------------------------------------------------
data_list <- forecastML::create_lagged_df(data_train, type = "train", 
                                          lookback = lookback, 
                                          horizon = horizons)

## ------------------------------------------------------------------------
windows <- forecastML::create_windows(data_list, window_length = 0)

plot(windows, data_list, show_labels = TRUE)

## ------------------------------------------------------------------------
model_results <- forecastML::train_model(data_list, windows, model_function, model_name = "LASSO")

data_results <- predict(model_results, prediction_function = list(prediction_function))

DT::datatable(head(data_results, 10), options = list(scrollX = TRUE))
plot(data_results, type = "prediction", horizons = c(1, 6, 12))
plot(data_results, type = "residual", horizons = c(1, 6, 12))
plot(data_results, type = "forecast_stability", valid_indices = 109:120)

## ------------------------------------------------------------------------
data_error <- forecastML::return_error(data_results, metrics = c("mae", "mape", "mdape", "smape"),
                                       models = NULL)

DT::datatable(head(data_error$error_global), options = list(scrollX = TRUE))
plot(data_error, data_results, type = "horizon")

## ------------------------------------------------------------------------
data_hyper <- forecastML::return_hyper(model_results, hyper_function)

plot(data_hyper, data_results, data_error, type = "stability", horizons = c(1, 6, 12))
plot(data_hyper, data_results, data_error, type = "error", c(1, 6, 12))

## ------------------------------------------------------------------------
data_forecast_list <- forecastML::create_lagged_df(data_train, type = "forecast", 
                                                  lookback = lookback,  horizon = horizons)

data_forecast <- predict(model_results, prediction_function = list(prediction_function), 
                         data_forecast = data_forecast_list)

plot(data_forecast, data_actual = data[-(1:150), ],
     actual_indices = as.numeric(row.names(data[-(1:150), ])),
     horizons = c(1, 6, 12), 
     facet_plot = c("model", "model_forecast_horizon")) + ggplot2::theme(legend.position = "none")

plot(data_forecast, data_actual = data_test, actual_indices = as.numeric(row.names(data_test)),
     facet_plot = NULL, horizons = c(1, 6, 12))

## ------------------------------------------------------------------------
data_error <- forecastML::return_error(data_forecast, data_test = data_test, 
                                       test_indices = as.numeric(row.names(data_test)),
                                       metrics = c("mae", "mape", "mdape", "smape"))

DT::datatable(data_error$error_by_horizon, options = list(scrollX = TRUE))
DT::datatable(data_error$error_global, options = list(scrollX = TRUE))

