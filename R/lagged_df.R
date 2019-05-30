#' Create datasets with lagged features
#'
#' Create a list of datasets with lagged predictors to (a) train forecasting models for
#' specified forecast horizons and (b) forecast with lagged predictors.
#'
#' @param data A data.frame with the target to be forecasted and features/predictors.
#' @param type The type of dataset to return--model training or forecast prediction. The default is 'train'.
#' @param outcome_cols The column index--an integer--of the target to be forecasted; only univariate forecasting is supported at present.
#' @param horizons Required. A vector of one or more forecast horizons, h, measured in dataset rows. For each horizon, 1:h
#' forecasts are returned (e.g., 'horizon = 12' trains a model to minimize 12-step-ahead error and returns forecasts
#' for 1:12 steps into the future).
#' @param lookback A vector giving the lags--in dataset rows--for creating the lagged features. All features in the
#' input dataset, 'data', are lagged by the same values which will produce an input dataset with dimensions
#' nrow(data) by (n_features * length(lookback)). Lags that don't support direct forecasting for a given horizon
#' are dropped. Either 'lookback' or 'lookback_control' need to be specified.
#' @param lookback_control A list of vectors, specifying potentially unique lags for each feature. The length
#' of the list should equal ncol(data) and be ordered the same as the columns in 'data').
#' Lags that don't support direct forecasting for a given horizon
#' are dropped. Either 'lookback' or 'lookback_control' need to be specified.
#' @return A 'lagged_df' object: A list of data.frames with new columns for the lagged predictors.
#' @example /R/examples/example_create_lagged_df.R
#'
#' @import ggplot2
#' @importFrom stats as.formula complete.cases cov sd
#' @importFrom magrittr %>%
#'
#' @export
create_lagged_df <- function(data, type = c("train", "forecast"), outcome_cols = 1,
                             horizons = NULL, lookback = NULL, lookback_control = NULL) {

  if (!methods::is(data, c("data.frame"))) {stop("The 'data' argument takes an object of class 'data.frame'.")}


  type <- type[1]

  outcome_cols <- as.numeric(outcome_cols[1])  # Restricting to one outcome at present.
  horizon <- as.numeric(horizons)

  if (!xor(is.null(lookback), is.null(lookback_control))) {stop("Enter an argument for either `lookback`--a feature lag vector--or `lookback_control`--a list of feature lag vectors.")}

  if (!is.null(lookback)) {
    lookback <- as.numeric(lookback)
    if (!all(lookback > 0)) {stop("The 'lookback' argument needs to be a vector of positive numbers.")}
    if (!max(lookback) >= min(horizon)) {stop("The highest lookback needs to be >= the shortest forecast horizon to allow for direct forecasting with lagged predictors.")}
  }

  # Check the lookback_control argument.
  if (length(horizon) == 1 & !is.null(lookback_control)) {
    # Check if there is one list location for each feature in the dataset.
    if(length(lookback_control) != (ncol(data) - length(outcome_cols))) {
      stop("For a single forecast horizon, the length of the 'lookback_control' list should equal to the number of features in
           the dataset. For multiple forecast horizons, 'lookback_control' is a nested list with length(lookback_control) ==
           length(horizons) and, one layer down, the nested list should have a length equal to the number of features in the dataset.
           Features with no lags should have a NULL value in the appropriate list location.")
    }

    # Clean the lookback_control list of impossible lag values for the given forecast horizon.
    # This will be an attribute for this function's return which is used for plotting.
    lookback_control <- lapply(seq_along(lookback_control), function(i) {
      data_out <- lookback_control[[i]][lookback_control[[i]] >= horizon]
    })
  } else if (length(horizon) > 1 & !is.null(lookback_control)) {

    lookback_control <- lapply(seq_along(lookback_control), function(i) {
      lapply(seq_along(lookback_control[[i]]), function(j) {
        data_out <- lookback_control[[i]][[j]][lookback_control[[i]][[j]] >= horizon[i]]
      })
    })
  }

  # This will be used to remove the rows with NAs in our new lagged predictor dataset--rows 1:lookback_max at the begnning of the dataset.
  if (!is.null(lookback)) {
    lookback_max <- max(lookback, na.rm = TRUE)
  } else {
    # TO-do: Greater flexibility could be added in the case of different horizons having different max lags.
    lookback_max <- max(unlist(lookback_control), na.rm = TRUE)
  }

  if (!all(horizon < nrow(data))) {stop("The forecast horizon needs to be less than nrow(data).")}

  row_names <- 1:nrow(data)

  outcome_names <- names(data)[outcome_cols]
  var_names <- names(data)  # All columns are currently used as predictors, even the target column.

  # Outcome columns.
  data_y <- data[, outcome_cols, drop = FALSE]

  # Each item in the list is a dataframe of lagged features that allow direct forecasting
  # to the given horizon.
  if (type == "train") {

    data_out <- lapply(seq_along(horizon), function(i) {

      forecast_horizon <- horizon[i]

      # Only create lagged features that allow direct forecasting to max(i). If a single lookback vector is defined,
      # we'll do this filtering outside of the inner loop below.
      if (!is.null(lookback)) {
        lookback_over_horizon <- lookback[lookback >= forecast_horizon]
      }

      data_x <- lapply(1:ncol(data), function(j) {

        # Only create lagged features that allow direct forecasting to max(i)--unique lags for each feature.
        if (!is.null(lookback_control)) {
          # As a convenience to the user a single-horizon forecast that uses a custom lookback doesn't need to be a nested list.
          if (length(horizon) == 1) {
            lookback_over_horizon <- lookback_control[[j]]
          } else {
            # A nested list of lags is needed for custom lookback_control(s) for each forecast horizon.
            lookback_over_horizon <- lookback_control[[i]][[j]]
          }
        }

        # If there are no feature-level lags suitable for the forecast horizon, return NULL for this feature-level lagged data.frame.
        if (!is.na(lookback_over_horizon) && length(lookback_over_horizon) > 0) {
          data_x <- as.data.frame(sapply(lookback_over_horizon, function(k){dplyr::lag(as.numeric(data[, j]), k)}))
          names(data_x) <- unlist(lapply(var_names[j], function(x){paste0(x, "_lag_", lookback_over_horizon)}))
          data_x
        } else {
          data_x <- NULL
        }
      })
      data_x <- dplyr::bind_cols(data_x)
      data_out <- dplyr::bind_cols(data_y, data_x)
      data_out <- data_out[-(1:lookback_max), ]  # To-do: make this more flexible for multiple horizons.

      attr(data_out, "horizon") <- forecast_horizon
      if (!is.null(lookback)) {
        attr(data_out, "lookback") <- lookback_over_horizon
      } else {
        attr(data_out, "lookback") <- if (length(horizon) == 1) {lookback_control} else {lookback_control[[i]]}
      }
      data_out
    })
  }

  if (type == "forecast") {

    data_out <- lapply(seq_along(horizon), function(i) {

      forecast_horizon <- horizon[i]

      # Only create lagged features that allow direct forecasting to max(i). If a single lookback vector is defined,
      # we'll do this filtering outside of the inner loop below.
      if (!is.null(lookback)) {
        lookback_over_horizon <- lookback[lookback >= forecast_horizon]
      }

      data_x <- lapply(1:ncol(data), function(j) {

        # Only create lagged features that allow direct forecasting to max(i)--unique lags for each feature.
        if (!is.null(lookback_control)) {
          # As a convenience to the user a single-horizon forecast that uses a custom lookback doesn't need to be a nested list.
          if (length(horizon) == 1) {
            lookback_over_horizon <- lookback_control[[j]]
          } else {
            # A nested list of lags is needed for custom lookback_control(s) for each forecast horizon.
            lookback_over_horizon <- lookback_control[[i]][[j]]
          }
        }

        if (!is.na(lookback_over_horizon) && length(lookback_over_horizon) > 0) {
          #k <- 1
          var_list <- vector("list", length(horizon))
          for (k in (forecast_horizon:1)) {
            var_list[[k]] <- as.data.frame(sapply(lookback_over_horizon, function(l){dplyr::lag(as.numeric(data[, j]), l - k)}))
            var_list[[k]] <- var_list[[k]][nrow(var_list[[k]]), , drop = FALSE]
            names(var_list[[k]]) <- unlist(lapply(var_names[j], function(x){paste0(x, "_lag_", lookback_over_horizon)}))
            if (j == 1) {
              var_list[[k]] <- dplyr::bind_cols(data.frame("horizon" = k), var_list[[k]])
            }
          }
          data_x <- dplyr::bind_rows(var_list)
          data_x
        } else {
          data_x <- NULL
        }
      })
      data_x <- dplyr::bind_cols(data_x)

      attr(data_x, "horizon") <- forecast_horizon
      if (!is.null(lookback)) {
        attr(data_x, "lookback") <- lookback_over_horizon
      } else {
        attr(data_out, "lookback") <- if (length(horizon) == 1) {lookback_control} else {lookback_control[[i]]}
      }
      data_x
    })
  }

  attr(data_out, "outcome_cols") <- outcome_cols
  attr(data_out, "outcome_names") <- outcome_names
  attr(data_out, "predictor_names") <- var_names
  attr(data_out, "row_indices") <- as.numeric(row_names[-(1:lookback_max)])
  attr(data_out, "data_start") <- lookback_max + 1
  attr(data_out, "data_stop") <- max(as.numeric(row_names))
  attr(data_out, "horizons") <- horizon

  class(data_out) <- c("lagged_df", class(data_out))

  return(data_out)
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#' @export
summary.lagged_df <- function(object) {

  data <- object

  if(!methods::is(data, "lagged_df")) {
    stop("This method takes an object of class 'lagged_df' as input. Run create_lagged_df() first.")
  }

  cat(paste0(
    "Number of forecast horizons: ", length(data), " \n",
    "Number of models to train: ", length(data), " \n",
    "Data start index: ", attributes(data)$data_start, " \n",
    "Data stop index: ", attributes(data)$data_stop, " \n",
    "Number of predictors in input data: ", length(attributes(data)$predictor_names), " \n",
    "Minimum number of predictors with lags: ", ncol(data[[length(data)]]) - length(attributes(data)$outcome_cols), " at forecast horizon ", attributes(data)$horizons[length(data)], " \n",
    "Maximum number of predictors with lags: ", ncol(data[[1]]) - length(attributes(data)$outcome_cols), " at forecast horizon ", attributes(data)$horizons[1], " \n"
  ))
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#' Plot datasets with lagged predictors
#'
#' Plot datasets with lagged predictors to view your direct forecasting setup across horizons.
#'
#' @param object An object of class 'lagged_df' from create_lagged_df().
#' @return A single plot if 'lookback' was specified in create_lagged_df();
#' a list of plots, one per predictor, if 'lookback_control' was specified.
#' @example /R/examples/example_plot_lagged_df.R
#' @export
plot.lagged_df <- function(object) {

  data <- object

  if(!methods::is(data, "lagged_df")) {
    stop("This method takes an object of class 'lagged_df' as input. Run create_lagged_df() first.")
  }

  horizon <- lapply(data, function(x){attributes(x)$horizon})

  # Within a given horizon, an indicator for different lag vectors for each predictor.
  if (methods::is(attributes(data[[1]])$lookback, "list")) {
    lookback_per_predictor <- TRUE
  } else {
    lookback_per_predictor <- FALSE
  }

  predictor_names <- attributes(data)$predictor_names
  n_predictors <- length(attributes(data)$predictor_names)

  lookback <- lapply(data, function(x) {attributes(x)$lookback})
  lookback_max <- max(unlist(lookback), na.rm = TRUE)

  if (lookback_per_predictor) {

    # seq_along horizon > predictor
    data_horizon <- lapply(seq_along(lookback), function(i) {

      data_predictor <- lapply(seq_along(lookback[[i]]), function(j) {

        lookback_predictor <- lookback[[i]][[j]]

        if (!is.na(lookback_predictor) && length(lookback_predictor) > 0) {
          data_predictor <- expand.grid("horizon" = unlist(horizon)[i], "time" = lookback_predictor)
          data_predictor$predictor_number <- j
        } else {
          data_predictor <- expand.grid("horizon" = NA, "time" = NA)
          data_predictor$predictor_number <- j
        }
        data_predictor
      })
      data_horizon <- dplyr::bind_rows(data_predictor)
      data_horizon
    })

    data_plot <- as.data.frame(dplyr::bind_rows(data_horizon))
    data_plot <- data_plot[complete.cases(data_plot), ]
    data_plot <- dplyr::distinct(data_plot, .keep_all = TRUE)
  } else {
    data_plot <- purrr::map2(horizon, lookback, function(x, y) {expand.grid("horizon" = x, "time" = y)})
    data_plot <- dplyr::bind_rows(data_plot)
  }

  # Set the scale of the x-axis by multiplying time by -1, placing the features to the left of the
  # zero line--i.e., in the past.
  data_plot$time <- -1 * data_plot$time

  # Plot fill and legend contents.
  data_plot$feature <- "Feature"  # Feature is present in plot.

  # Make a grid for plotting the presence of lagged features, no lagged features, and forecast horizons.
  data_grid_past <- expand.grid("horizon" = min(unlist(horizon)):max(unlist(horizon)),
                                "time" = (-1 * lookback_max):-1)
  data_grid_future <- expand.grid("horizon" = min(unlist(horizon)):max(unlist(horizon)),
                                  "time" = 1:max(unlist(horizon)))
  # For forecast horizon plot fill, remove the rows in the data that exceed the horizon.
  data_grid_future <- data_grid_future[with(data_grid_future, time <= horizon), ]
  data_grid <- dplyr::bind_rows(data_grid_past, data_grid_future)

  data_plot <- dplyr::left_join(data_grid, data_plot, by = c("horizon", "time"))

  # Plot fill and legend contents.
  data_plot$feature[is.na(data_plot$feature) & data_plot$time <= 0] <- "No feature"
  data_plot$feature[is.na(data_plot$feature)] <- "Forecast"

  # Filter to remove user-specified unused horizons from the plot grid.
  data_plot <- data_plot[data_plot$horizon %in% unlist(horizon), ]

  if (!lookback_per_predictor) {
    p <- ggplot(data_plot, aes(x = time, y = ordered(horizon), fill = feature))
    p <- p + geom_tile(color = "gray85")
    p <- p + scale_fill_viridis_d()
    p <- p + geom_vline(xintercept = 0, size = 2)
    # With a continuous scale, the limits need to extend 1 unit beyond the min and max to capture the bookend geom_tile-s.
    p <- p + scale_x_continuous(limits = c(eval(parse(text = (lookback_max * -1) - 1)), max(data_plot$time, na.rm = TRUE) + 1))
    p <- p + theme_bw()
    p <- p + xlab("Time (0 is the current time)") + ylab("Forecast horizon") +
      labs(fill = NULL) + ggtitle("Map of Predictor Lags for a Single Predictor")
    return(p)
  } else {

    lapply(1:n_predictors, function(i) {

      data_plot_predictor_1 <- dplyr::filter(data_plot, feature == "Feature" & predictor_number == i)
      data_plot_predictor_1$feature_match <- paste0(data_plot_predictor_1$horizon, "-", data_plot_predictor_1$time)
      data_plot_predictor_2 <- data_grid_past
      data_plot_predictor_2$feature <- "No feature"
      data_plot_predictor_2$feature_match <- paste0(data_plot_predictor_2$horizon, "-", data_plot_predictor_2$time)
      data_plot_predictor_2 <- data_plot_predictor_2[!(data_plot_predictor_2$feature_match %in% data_plot_predictor_1$feature_match), ]
      data_plot_predictor_3 <- dplyr::filter(data_plot, feature == "Forecast")
      data_plot_predictor <- dplyr::bind_rows(data_plot_predictor_1, data_plot_predictor_2, data_plot_predictor_3)

      p <- ggplot(data_plot_predictor, aes(x = time, y = ordered(horizon), fill = feature))
      p <- p + geom_tile(color = "gray85")
      p <- p + scale_fill_viridis_d()
      p <- p + geom_vline(xintercept = 0, size = 2)
      # With a continuous scale, the limits need to extend 1 unit beyond the min and max to capture the bookend geom_tile-s.
      p <- p + scale_x_continuous(limits = c(eval(parse(text = (lookback_max * -1) - 1)), max(data_plot$time, na.rm = TRUE) + 1))
      p <- p + theme_bw()
      p <- p + xlab("Time (0 is the current time)") + ylab("Forecast horizon") +
        labs(fill = NULL) + ggtitle(paste0("Map of Predictor Lags: ", predictor_names[i]))
    })
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
