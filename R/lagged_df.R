#' Create datasets with lagged features
#'
#' Create a list of datasets with lagged predictors to (a) train forecasting models for
#' specified forecast horizons and (b) forecast with lagged predictors.
#'
#' @param data A data.frame with the target to be forecasted and features/predictors. An optional date column can be given in the 'dates' argument
#' (required for grouped time-series). Note that forecastML only works with regularly spaced time/date intervals and that missing
#' rows--usually due to periods when no data was collected--will result in poorly trained models due to incorrect predictor lags.
#' @param type The type of dataset to return--(a) model training or (b)forecast prediction. The default is 'train'.
#' @param outcome_cols The column index--an integer--of the target to be forecasted. Forecasting only one outcome column is allowed at present, however,
#' groups of time-series can be forecasted if they are stacked vertically in a long dataset and the 'groups', 'dates', and 'frequency' arguments
#' are specified.
#' @param horizons A vector of one or more forecast horizons, h, measured in dataset rows. For each horizon, 1:h
#' forecasts are returned (e.g., 'horizon = 12' trains a model to minimize 12-step-ahead error and returns forecasts
#' for 1:12 steps into the future).
#' @param lookback A vector giving the lags--in dataset rows--for creating the lagged features. All non-grouping features in the
#' input dataset, 'data', are lagged by the same values which, for non-grouped time-series, will produce an input dataset with dimensions
#' nrow(data) by (n_features * length(lookback)). Lags that don't support direct forecasting for a given horizon
#' are dropped. Either 'lookback' or 'lookback_control' need to be specified.
#' @param lookback_control A list of vectors, specifying potentially unique lags for each feature. The length
#' of the list should equal ncol(data) and be ordered the same as the columns in 'data'. For grouped time-series, lags for the grouping columns
#' should have a lookback_control value of 0. 'NULL' lookback_control values drop columns from the input dataset.
#' Lags that don't support direct forecasting for a given horizon
#' are dropped. Either 'lookback' or 'lookback_control' need to be specified.
#' @param groups Column name(s) that give the groups/hierarchies when multiple time-series are present. These columns are used as model predictors but are not lagged.
#' Note that combining feature lags with grouped time-series will result in NA values throughout the data.
#' @param dates A vector or 1-column data.frame of dates with class 'Date'. The length of dates should equal nrow(data). Required if 'groups' are given.
#' @param frequency A string taking the same input as `scales::date_breaks()` e.g., '1 month', '7 days', etc.". Required if 'dates' are given.
#' @return A 'lagged_df' or 'grouped_lagged_df' object: A list of data.frames with new columns for the lagged predictors.
#' @example /R/examples/example_create_lagged_df.R
#'
#' @import ggplot2
#' @importFrom stats as.formula complete.cases cov sd
#' @importFrom magrittr %>%
#'
#' @export
create_lagged_df <- function(data, type = c("train", "forecast"), outcome_cols = 1,
                             horizons, lookback = NULL, lookback_control = NULL,
                             groups = NULL, dates = NULL, frequency = NULL) {

  if (!methods::is(data, c("data.frame"))) {stop("The 'data' argument takes an object of class 'data.frame'.")}

  type <- type[1]  # Model-training datasets are the default.

  outcome_cols <- as.numeric(outcome_cols[1])  # Restricting to one outcome at present.
  horizon <- as.numeric(horizons)  # Forecast horizons.

  row_names <- 1:nrow(data)

  outcome_names <- names(data)[outcome_cols]

  # The following columns are used as predictors: outcome_names, 'NULL' in a list slot in 'lookback_control'
  # drops columns from from the input data, and grouping columns are retained as features but not lagged.
  var_names <- names(data)

  # Outcome data.
  data_y <- data[, outcome_cols, drop = FALSE]

  #----------------------------------------------------------------------------
  # Group column indices to keep grouping columns when checking the 'lookback_cotrol' argument--the value of which should be 0 for grouping columns.
  if (!is.null(groups)) {
    group_cols <- which(names(data) %in% groups)
  }

  if (!xor(is.null(lookback), is.null(lookback_control))) {stop("Enter an argument for either `lookback`--a feature lag vector--or `lookback_control`--a list of feature lag vectors.")}

  if (!is.null(lookback)) {
    lookback <- as.numeric(lookback)
    if (!all(lookback >= 0)) {stop("The 'lookback' argument needs to be a vector of positive numbers.")}
    if (!max(lookback) >= min(horizon)) {stop("The highest lookback needs to be >= the shortest forecast horizon to allow for direct forecasting with lagged predictors.")}
  }

  # Check the lookback_control argument.
  if (length(horizon) == 1 & !is.null(lookback_control)) {  # A 1-horizon, non-nested lookback_control of feature lags.

    # Check if there is one list location for each feature in the dataset.
    if(length(lookback_control) != (ncol(data))) {
      stop("For a single forecast horizon, the length of the 'lookback_control' list should equal the number of features in
           the dataset. For multiple forecast horizons, 'lookback_control' is a nested list with length(lookback_control) ==
           length(horizons) and, one layer down, the nested list should have a length equal to the number of features in the dataset.
           'lookback_control' list slots with 'NULL' drops columns from the input data, and 'lookback_control' list slots with 0 are used for
           grouping columns.")
    }

    # Clean the lookback_control list of impossible lag values for the given forecast horizon.
    # This will be an attribute for this function's return which is used for plotting.
    lookback_control <- lapply(seq_along(lookback_control), function(i) {

      if (is.null(groups)) {

        lookback_control[[i]][lookback_control[[i]] >= horizon]

      } else {

        if (i %in% group_cols) {  # Set lags for grouping columns to 0.

          lookback_control[[i]] <- 0

        } else {

          lookback_control[[i]] <- lookback_control[[i]][lookback_control[[i]] >= horizon]
        }
        lookback_control[[i]]
      }
    })

  } else if (length(horizon) > 1 & !is.null(lookback_control)) {  # A multiple-horizon, nested lookback_control of feature lags.

    lookback_control <- lapply(seq_along(lookback_control), function(i) {
      lapply(seq_along(lookback_control[[i]]), function(j) {

        if (is.null(groups)) {

          lookback_control[[i]][[j]][lookback_control[[i]][[j]] >= horizon[i]]

        } else {

          if (j %in% group_cols) {  # Set lags for grouping columns to 0.

            lookback_control[[i]][[j]] <- 0

          } else {

            lookback_control[[i]][[j]] <- lookback_control[[i]][[j]][lookback_control[[i]][[j]] >= horizon[i]]
          }
          lookback_control[[i]][[j]]
        }
      })
    })
  }

  # This will be used to remove the rows with NAs in our new lagged predictor dataset--rows 1:lookback_max at the begnning of the dataset.
  # This is only used for non-grouped datasets.
  if (!is.null(lookback)) {
    lookback_max <- max(lookback, na.rm = TRUE)
  } else {
    # TO-do: Greater flexibility could be added in the case of different horizons having different max lags. At present, the lags for longer forecast
    # horizons are also used to remove dataset rows from input datasets with shorter forecast horizons, which may only use shorter lags. This amounts
    # to unnecessarily discarding training data for short-term forecast horizons when these models are trained alongside long-term models.
    lookback_max <- max(unlist(lookback_control), na.rm = TRUE)
  }
  #----------------------------------------------------------------------------

  if (!all(horizon < nrow(data))) {stop("The forecast horizon needs to be less than nrow(data).")}

  dates <- if (methods::is(dates, "data.frame")) {
    dates[, 1, drop = TRUE]
  } else {
    dates
  }

  if (!is.null(dates) && !methods::is(dates, "Date") && length(dates) != nrow(data)) {
    stop("The 'dates' argument needs to be a vector or 1 column data.frame of length nrow(data)
         of dates with class `Date`.")
  }

  if (!is.null(dates) && is.null(frequency)) {
    stop("The 'frequency' argument needs to be specified along with the `dates` argument.
         It takes the same input as `scales::date_breaks()` e.g., '1 month', '7 days', etc.")
  }

  if (!is.null(groups) && is.null(dates)) {
    stop("The 'dates' argument needs to be specified with grouped data.")
  }
  #----------------------------------------------------------------------------
  # Each item in the list is a data.frame of lagged features that allow direct forecasting.
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

            # Set for feature lags for grouping features.
            if (length(lookback_over_horizon) == 0 && var_names[j] %in% groups) {
              lookback_over_horizon <- 0
            }
          }
        }

        # If there are no feature-level lags suitable for the forecast horizon, return NULL for this feature-level lagged data.frame.
        # However, grouping features will pass through and be added to the output dataset without lags.
        if ((!is.na(lookback_over_horizon) && length(lookback_over_horizon) > 0) || var_names[j] %in% groups) {

          data_x <- as.data.frame(sapply(lookback_over_horizon, function(k) {

            if (!is.null(groups)) {  # Create lagged features by group.

              # If the current feature in the loop is a grouping feature, return the grouping feature without lags.
              if (var_names[j] %in% groups) {

                data_x_var <- data[, var_names[j], drop = TRUE]  # Exit the 'k' loop.

              } else {  # This feature is not a grouping feature and we'll compute lagged versions.

                data_out <- data[, c(groups, var_names[j])] %>%
                  dplyr::group_by_at(dplyr::vars(groups)) %>%
                  dplyr::mutate('lagged_var' = dplyr::lag(!!rlang::sym(var_names[j]), k))

                data_x_var <- data_out$lagged_var
              }

            } else {  # No user-defined groupings, compute lagged variables without `dplyr::group_by`.

              data_out <- data %>%
                dplyr::mutate('lagged_var' = dplyr::lag(!!rlang::sym(var_names[j]), k))

              data_x_var <- data_out$lagged_var
            }
            data_x_var
            }))  # End loop 'k', creation of lagged features for a given feature.

          data_x <- dplyr::bind_cols(data_x)

          if (!var_names[j] %in% groups) {

            names(data_x) <- unlist(lapply(var_names[j], function(x){paste0(x, "_lag_", lookback_over_horizon)}))

          } else {

            data_x <- data_x[, 1, drop = FALSE]
            names(data_x) <- var_names[j]
          }

          data_x

        } else {  # There are no user-specified feature lags appropriate for direct forecasting for this feature.

          data_x <- NULL
        }
      })  # End loop 'j', the creation of lagged features for a given forecast model horizon.

      data_x <- dplyr::bind_cols(data_x)  # A single data.frame of lags for all features at this forecast model horizon.

      # Re-order the columns so that the grouping features, if any, are first.
      if (!is.null(groups)) {
        data_x <- dplyr::select(data_x, groups, names(data_x)[!names(data_x) %in% groups])
      }

      # The complete dataset for a given forecast model horizon.
      data_out <- dplyr::bind_cols(data_y, data_x)

      # If the forecast is grouped, leave the NAs in the dataset for the user because the ML model used for these
      # cases will likely handle NA values.
      if (is.null(groups)) {
        data_out <- data_out[-(1:lookback_max), ]  # To-do: make this more flexible for multiple forecast model horizons.
      }

      attr(data_out, "horizon") <- forecast_horizon
      if (!is.null(lookback)) {
        attr(data_out, "lookback") <- lookback_over_horizon
      } else {
        attr(data_out, "lookback") <- if (length(horizon) == 1) {lookback_control} else {lookback_control[[i]]}
      }

      data_out
    })  # End the creation of modeling datasets for all forecast model horizons.
  }
  #----------------------------------------------------------------------------

  if (type == "forecast") {  # Create a dataset for forecasting H steps into the future.

    data_stop <- NULL  # An attribute for grouped datasets that is assigned to with <<- in the loop below.

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

            # Set for feature lags for grouping features.
            if (length(lookback_over_horizon) == 0 && var_names[j] %in% groups) {
              lookback_over_horizon <- 0
            }
          }
        }

        # If there are no feature-level lags suitable for the forecast horizon, return NULL for this feature-level lagged data.frame.
        # However, grouping features will pass through and be added to the output dataset without lags.
        if ((!is.na(lookback_over_horizon) && length(lookback_over_horizon) > 0) || var_names[j] %in% groups) {

          #k <- lookback_over_horizon[1]
          data_x <- lapply(lookback_over_horizon, function(k) {

            # Create lagged features by group.
            if (!is.null(groups)) {

              # If the feature is a grouping variable, it will be included as an appropriately unlagged feature in
              # the prediction dataset from using `dplyr::group_by`.
              if (!var_names[j] %in% groups) {  # computing lagged features.

                data_out <- data[, c(groups, var_names[j])] %>%
                  dplyr::group_by_at(dplyr::vars(groups)) %>%
                  dplyr::do({
                    setNames(data.frame(sapply(forecast_horizon:1, function(l){dplyr::lag(.[, var_names[j], drop = TRUE], k - l)})), forecast_horizon:1)
                    }) %>%
                  dplyr::mutate("row_number" = 1:dplyr::n()) %>%
                  dplyr::filter(row_number == max(row_number, na.rm = TRUE)) %>%
                  dplyr::ungroup()

                data_out <- tidyr::gather(data_out, key = "horizon", value = !!paste0(var_names[j], "_lag_", k),
                                          -!!groups, -row_number)  # intermediate data.frame for debugging.
                data_out

              } else {  # A non-lagged grouping feature.

                data_out <- data[, groups, drop = FALSE] %>%
                  dplyr::group_by_at(dplyr::vars(groups)) %>%
                  dplyr::do({
                    setNames(data.frame(sapply(forecast_horizon:1, function(l){.[, var_names[j], drop = TRUE]})), forecast_horizon:1)
                  }) %>%
                  dplyr::mutate("row_number" = 1:dplyr::n()) %>%
                  dplyr::filter(row_number == max(row_number, na.rm = TRUE)) %>%
                  dplyr::ungroup()

                data_out <- tidyr::gather(data_out, key = "horizon", value = "drop_this_duplicate_grouping_var",
                                          -!!groups, -row_number)

                data_out <- dplyr::select(data_out, -drop_this_duplicate_grouping_var)

                data_out
              }

            } else {  # Compute lagged features for non-grouped data.

              data_out <- data %>%
                dplyr::do({
                  setNames(data.frame(sapply(forecast_horizon:1, function(l){dplyr::lag(.[, var_names[j], drop = TRUE], k - l)})), forecast_horizon:1)
                  }) %>%
                dplyr::mutate("row_number" = 1:dplyr::n()) %>%
                dplyr::filter(row_number == max(row_number, na.rm = TRUE))

              data_out <- tidyr::gather(data_out, key = "horizon", value = !!paste0(var_names[j], "_lag_", k), -row_number)
              data_out
            }
          })  # End lagged feature creating for each feature.

          data_x <- data_x[!sapply(data_x, is.null)]

          if (is.null(groups)) {

            data_x <- Reduce(function(x, y) {dplyr::full_join(x, y, by = c("row_number", "horizon"))}, data_x)

          } else {

            data_x <- Reduce(function(x, y) {dplyr::full_join(x, y, by = c("row_number", "horizon", groups))}, data_x)
          }

          data_x

        } else {

          data_x <- NULL
        }
      })  # End feature-level lagged feature creation.

      # Merge all feature-level lags into 1 data.frame.
      if (is.null(groups)) {

        data_x <- data_x[!sapply(data_x, is.null)]
        data_x <- Reduce(function(x, y) {dplyr::full_join(x, y, by = c("row_number", "horizon"))}, data_x)

        data_x$horizon <- as.integer(data_x$horizon)

      } else {  # Grouped dataset.

        data_x <- data_x[!sapply(data_x, is.null)]
        data_x <- Reduce(function(x, y) {try(dplyr::full_join(x, y, by = c("row_number", "horizon", groups)))}, data_x)

        # Re-order the columns so the grouping features, if any, are first.
        data_x <- dplyr::select(data_x, row_number, horizon, groups,
                                names(data_x)[!names(data_x) %in% c("row_number", "horizon", groups)])

        data_x$horizon <- as.integer(data_x$horizon)

        # Keep row number, which gives the last row of actuals for each time-series, for the data_stop attribute.
        data_stop <<- data_x[, "row_number", drop = TRUE]

        data_x <- dplyr::select(data_x, -row_number)
      }

      attr(data_x, "horizon") <- forecast_horizon
      if (!is.null(lookback)) {
        attr(data_x, "lookback") <- lookback_over_horizon
      } else {
        attr(data_x, "lookback") <- if (length(horizon) == 1) {lookback_control} else {lookback_control[[i]]}
      }
      data_x
    })
  }  # End `type = forecast` dataset creation.
  #----------------------------------------------------------------------------

  # Global classes and attributes for the return object.
  attr(data_out, "outcome_cols") <- outcome_cols
  attr(data_out, "outcome_names") <- outcome_names
  attr(data_out, "predictor_names") <- var_names

  if (is.null(groups)) {
    attr(data_out, "row_indices") <- row_names[-(1:lookback_max)]  # To-do: match with date indices.
    if (is.null(dates)) {
      attr(data_out, "data_start") <- lookback_max + 1  # Removes NAs at the beginning of the dataset
      attr(data_out, "data_stop") <- max(row_names, na.rm = TRUE)
    } else {
      attr(data_out, "data_start") <- min(dates[lookback_max + 1], na.rm = TRUE)  # Removes NAs at the beginning of the dataset
      attr(data_out, "data_stop") <- max(dates, na.rm = TRUE)
      attr(data_out, "date_indices") <- dates
      attr(data_out, "frequency") <- frequency
    }
  } else {  # Grouped data requires a 'date' argument.
    attr(data_out, "row_indices") <- row_names  # Don't remove intial rows because the data is grouped and will have lots of NAs throughout.
    attr(data_out, "data_start") <- min(dates, na.rm = TRUE)
    attr(data_out, "data_stop") <- max(dates, na.rm = TRUE) #  To-do: decide if we need the vector of stop dates from data_stop.
    attr(data_out, "date_indices") <- dates
    attr(data_out, "frequency") <- frequency
  }
  attr(data_out, "horizons") <- horizon
  attr(data_out, "groups") <- groups

  if (is.null(groups)) {
    class(data_out) <- c("lagged_df", class(data_out))
  } else {
    class(data_out) <- c("grouped_lagged_df", "lagged_df", class(data_out))
  }

  return(data_out)
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#' @export
summary.lagged_df <- function(object) {

  data <- object

  n_predictors_at_each_horizon <- unlist(lapply(data, function(x) {ncol(x) - 1}))
  horizons <- attributes(data)$horizons

  if(!methods::is(data, "lagged_df")) {
    stop("This method takes an object of class 'lagged_df' as input. Run create_lagged_df() first.")
  }

  cat(paste0(
    "Number of forecast horizons: ", length(data), " \n",
    "Number of models to train: ", length(data), " \n",
    "Data start index: ", attributes(data)$data_start, " \n",
    "Data stop index: ", attributes(data)$data_stop, " \n",
    "Number of predictors in input data: ", length(attributes(data)$predictor_names), " \n",
    "Minimum number of predictors with lags: ", min(n_predictors_at_each_horizon, na.rm = TRUE), " at forecast horizon ", horizons[which.min(n_predictors_at_each_horizon)], " \n",
    "Maximum number of predictors with lags: ", max(n_predictors_at_each_horizon, na.rm = TRUE), " at forecast horizon ", horizons[which.max(n_predictors_at_each_horizon)], " \n"
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

  horizon <- attributes(data)$horizons

  # Within a given horizon, an indicator for different lag vectors for each predictor.
  if (methods::is(attributes(data[[1]])$lookback, "list")) {
    lookback_per_predictor <- TRUE
  } else {
    lookback_per_predictor <- FALSE
  }

  groups <- attributes(data)$groups

  # Grouping features won't be plotted because they aren't lagged.
  predictor_names <- attributes(data)$predictor_names
  dont_plot_these_predictors <- which(predictor_names %in% groups)  # Indices of grouping features.
  predictor_names <- predictor_names[!predictor_names %in% groups]

  n_predictors <- length(predictor_names)

  lookback <- lapply(data, function(x) {attributes(x)$lookback})
  lookback_max <- max(unlist(lookback), na.rm = TRUE)

  if (lookback_per_predictor) {

    # seq_along horizon > predictor.
    data_horizon <- lapply(seq_along(lookback), function(i) {

      data_predictor <- lapply(seq_along(lookback[[i]]), function(j) {

        if (!j %in% dont_plot_these_predictors) {

          lookback_predictor <- lookback[[i]][[j]]

          if (!is.na(lookback_predictor) && length(lookback_predictor) > 0) {
            data_predictor <- expand.grid("horizon" = horizon[i], "time" = lookback_predictor)
            data_predictor$predictor_number <- j

          } else {  # Nothing to plot, no feature lags appropriate for this horizon.

            data_predictor <- expand.grid("horizon" = NA, "time" = NA)
            data_predictor$predictor_number <- j
          }
          data_predictor
        }
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
  data_grid_past <- expand.grid("horizon" = min(horizon):max(horizon),
                                "time" = (-1 * lookback_max):-1)
  data_grid_future <- expand.grid("horizon" = min(horizon):max(horizon),
                                  "time" = 1:max(horizon))
  # For forecast horizon plot fill, remove the rows in the data that exceed the horizon.
  data_grid_future <- data_grid_future[with(data_grid_future, time <= horizon), ]
  data_grid <- dplyr::bind_rows(data_grid_past, data_grid_future)

  data_plot <- dplyr::left_join(data_grid, data_plot, by = c("horizon", "time"))

  # Plot fill and legend contents.
  data_plot$feature[is.na(data_plot$feature) & data_plot$time <= 0] <- "No feature"
  data_plot$feature[is.na(data_plot$feature)] <- "Forecast"

  # Filter to remove user-specified unused horizons from the plot grid.
  data_plot <- data_plot[data_plot$horizon %in% horizon, ]

  if (!lookback_per_predictor) {

    data_plot$horizon <- factor(data_plot$horizon, levels = sort(unique(as.numeric(data_plot$horizon))), ordered = TRUE)

    p <- ggplot(data_plot, aes(x = time, y = horizon, fill = feature))
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

      data_plot_predictor <- data_plot_predictor[as.numeric(data_plot_predictor$horizon) %in% horizon, ]
      data_plot_predictor$horizon <- factor(data_plot_predictor$horizon, levels = sort(unique(as.numeric(data_plot_predictor$horizon))), ordered = TRUE)

      p <- ggplot(data_plot_predictor, aes(x = time, y = horizon, fill = feature))
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
