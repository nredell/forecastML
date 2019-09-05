## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 4)

## ---- warning = FALSE, message = FALSE-----------------------------------
library(forecastML)
library(DT)

data("data_seatbelts", package = "forecastML")
data <- data_seatbelts

data <- data[, c("DriversKilled", "kms", "PetrolPrice", "law")]
DT::datatable(head(data, 5))

## ------------------------------------------------------------------------
ts_date_interval <- "1 month"

dates <- seq(as.Date("1969-01-01"), as.Date("1984-12-01"), by = ts_date_interval)

## ------------------------------------------------------------------------
horizons <- c(1, 6, 12)  # forecasting 1, 1:6, and 1:12 months into the future.

# A list of length 3, one slot for each modeled forecast horizon.
lookback_control <- vector("list", length(horizons))
lookback_control <- lapply(lookback_control, function(x) {
  list(
    c(3, 12),  # column 1: DriversKilled
    1:3,       # column 2: kms
    1:12,      # column 3: PetrolPrice
    12         # column 4: law
    )
  })

data_train <- forecastML::create_lagged_df(data, type = "train", outcome_cols = 1, 
                                           lookback_control = lookback_control,
                                           horizons = horizons, dates = dates,
                                           frequency = ts_date_interval)

## ---- results = 'hide'---------------------------------------------------
plot(data_train)

## ------------------------------------------------------------------------
horizons <- c(1, 6, 12)  # forecasting 1, 1:6, and 1:12 months into the future.

# A list of length 3, one slot for each modeled forecast horizon.
lookback_control <- vector("list", length(horizons))
lookback_control <- lapply(lookback_control, function(x) {
  
  lapply(1:4, function(x) {1:12})  # 12 feature lags for each of our 4 modeled features.
  })

# Find the column index of the feature that we're removing.
remove_col <- which(grepl("PetrolPrice", names(data)))

# Remove the feature from the 12-month-out lagged data.frame.
lookback_control[[which(horizons == 12)]][remove_col] <- list(NULL)

data_train <- forecastML::create_lagged_df(data, type = "train", outcome_cols = 1, 
                                           lookback_control = lookback_control,
                                           horizons = horizons, dates = dates,
                                           frequency = ts_date_interval)

## ---- results = 'hide'---------------------------------------------------
plot(data_train)[[remove_col]]  # we're selecting 1 of our 4 feature-level plots.

## ------------------------------------------------------------------------
DT::datatable(head(data_train$horizon_12))

