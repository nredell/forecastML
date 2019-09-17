#' Create model training and forecasting datasets with lagged, grouped, and static features
#'
#' Create a list of datasets with lagged, grouped, and static features to (a) train forecasting models for
#' specified forecast horizons and (b) forecast into the future with a trained ML model.
#'
#' @param data A data.frame with the (a) target to be forecasted and (b) features/predictors. An optional date column can be given in the
#' \code{dates} argument (required for grouped time-series). Note that forecastML only works with regularly spaced time/date intervals and that missing
#' rows--usually due to periods when no data was collected--will result in poorly trained models due to incorrect feature lags.
#' Use \code{\link{fill_gaps}} to fill in any missing rows/data prior to running this function.
#' @param type The type of dataset to return--(a) model training or (b) forecast prediction. The default is \code{train}.
#' @param outcome_cols The column index--an integer--of the target to be forecasted. Forecasting only one outcome column is allowed at present, however,
#' groups of time-series can be forecasted if they are stacked vertically in a long dataset and the \code{groups}, \code{dates},
#' and \code{frequency} arguments are specified.
#' @param horizons A numeric vector of one or more forecast horizons, h, measured in input dataset rows. For each horizon, 1:h
#' forecasts are returned (e.g., \code{horizon = 12} trains a model to minimize 1 to 12-step-ahead error and returns forecasts
#' for 1:12 steps into the future). If \code{dates} are given, a horizon of 1, for example, would equal 1 * \code{frequency} in calendar time.
#' @param lookback A numeric vector giving the lags--in dataset rows--for creating the lagged features. All non-grouping and
#' non-static features in the input dataset, \code{data}, are lagged by the same values which, for
#' non-grouped time-series, will produce an input dataset with dimensions
#' nrow(\code{data}) by (n_features * \code{length(lookback))}. Lags that don't support direct forecasting for a given horizon
#' are silently dropped. Either \code{lookback} or \code{lookback_control} need to be specified.
#' @param lookback_control A list of numeric vectors, specifying potentially unique lags for each feature. The length
#' of the list should equal \code{ncol(data)} and be ordered the same as the columns in \code{data}. For grouped time-series, lags for the grouping column(s)
#' and static feature columns should have a \code{lookback_control} value of 0. \code{list(NULL)} \code{lookback_control} values drop columns
#' from the input dataset. Lags that don't support direct forecasting for a given horizon
#' are silently dropped. Either \code{lookback} or \code{lookback_control} need to be specified.
#' @param dates A vector or 1-column data.frame of dates with class 'Date'. The length of dates should equal \code{nrow(data)}. Required if \code{groups}
#' are given.
#' @param frequency Date frequency. A string taking the same input as \code{base::seq(..., by = "frequency")} e.g., '1 month', '7 days', etc.
#' Required if \code{dates} are given.
#' @param groups Column name(s) that identify the groups/hierarchies when multiple time-series are present. These columns are used as model predictors but
#' are not lagged. Note that combining feature lags with grouped time-series will result in \code{NA} values throughout the data.
#' @param static_features For grouped time-series only. Column name(s) that identify features that do not change through time.
#' These columns are used as model features but are not lagged.
#' Note that combining feature lags with grouped time-series will result in \code{NA} values throughout the data.
#' @param use_future Boolean. If \code{TRUE}, the \code{future} package is used for creating lagged data.frames.
#' \code{multisession} or \code{multicore} futures are especially useful for (a) grouped time series with many groups and
#' (b) high-dimensional datasets with many lags per feature. Run \code{future::plan(future::multiprocess)} prior to this
#' function to set up multissession or multicore parallel dataset creation.
#' @return An S3 object of class 'lagged_df' or 'grouped_lagged_df': A list of data.frames with new columns for the lagged/non-lagged features.
#' The length of the returned list is equal to the number of forecast horizons and is in the order of
#' horizons supplied to the \code{horizons} argument. Horizon-specific datasets can be accessed with
#' \code{my_lagged_df$horizon_h} where 'h' gives the forecast horizon.
#'
#' The contents of the returned data.frame(s) are as follows:
#'
#' \describe{
#'   \item{\strong{type = 'train', non-grouped:}}{A data.frame of lagged features.}
#'   \item{\strong{type = 'train', grouped:}}{A data.frame of unlagged grouping columns followed by lagged and static features.}
#'   \item{\strong{type = 'forecast', non-grouped:}}{(1) An 'index' column giving the row index or date of the
#'   forecast periods (e.g., a 100 row non-date-based training dataset would start with an index of 101). (2) A 'horizon' column
#'   that indicates the forecast period from 1:max(horizons). (3) Lagged features identical to the
#'   'train', non-grouped dataset.}
#'   \item{\strong{type = 'forecast', grouped:}}{(1) An 'index' column giving the date of the
#'   forecast periods. The first forecast date for each group is the maximum date from the \code{dates} argument
#'   + 1 * \code{frequency} which is the user-supplied date frequency.(2) A 'horizon' column that indicates
#'   the forecast period from \code{1:max(horizons)}. (3) Lagged and static features identical to the train', grouped dataset.}
#' }
#' @section Attributes:
#'
#' \itemize{
#'   \item \code{names}: The horizon-specific datasets that can be accessed by \code{my_lagged_df$horizon_h} where 'h' gives
#'   the forecast horizon.
#'   \item \code{type}: Training, \code{train}, or forecasting, \code{forecast}, dataset(s).
#'   \item \code{horizons}: Forecast horizons measured in dataset rows.
#'   \item \code{outcome_cols}: The column index of the target being forecasted.
#'   \item \code{outcome_names}: The name of the target being forecasted.
#'   \item \code{predictor_names}: The predictor or feature names from the input dataset.
#'   \item \code{row_indices}: The \code{row.names()} of the output dataset. For non-grouped datasets, the first
#'   \code{lookback} + 1 rows are removed from the beginning of the dataset to remove \code{NA} values in the lagged features.
#'   \item \code{date_indices}: If \code{dates} are given, the vector of \code{dates}.
#'   \item \code{frequency}: If \code{dates} are given, the date/time frequency.
#'   \item \code{data_start}: \code{min(row_indices)} or \code{min(date_indices)}.
#'   \item \code{data_stop}: \code{max(row_indices)} or \code{max(date_indices)}.
#'   \item \code{groups}: If \code{groups} are given, a vector of group names.
#'   \item \code{class}: grouped_lagged_df, lagged_df, list
#' }
#'
#' @section Methods and related functions:
#'
#' The output of \code{create_lagged_df()} is passed into
#'
#' \itemize{
#'   \item \code{\link{create_windows}}
#' }
#'
#' and has the following generic S3 methods
#'
#' \itemize{
#'   \item \code{\link[=summary.lagged_df]{summary}}
#'   \item \code{\link[=plot.lagged_df]{plot}}
#' }
#' @example /R/examples/example_create_lagged_df.R
#'
#' @import ggplot2
#' @importFrom stats as.formula complete.cases cov sd
#' @importFrom magrittr %>%
#' @importFrom lubridate %m-%
#' @importFrom rlang .data
#' @importFrom purrr map2
#'
#' @export
create_lagged_df <- function(data, type = c("train", "forecast"), outcome_cols = 1, horizons, lookback = NULL,
                             lookback_control = NULL, dates = NULL, frequency = NULL,
                             groups = NULL, static_features = NULL, use_future = FALSE) {

  if (!methods::is(data, c("data.frame"))) {stop("The 'data' argument takes an object of class 'data.frame'.")}

  data <- as.data.frame(data)

  type <- type[1]  # Model-training datasets are the default.

  outcome_cols <- as.numeric(outcome_cols[1])  # Restricting to one outcome at present.
  horizon <- as.numeric(horizons)  # Forecast horizons.

  row_names <- 1:nrow(data)

  outcome_names <- names(data)[outcome_cols]  # To-do: future versions will work with column names or indices.

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
  if (length(horizon) == 1 && !is.null(lookback_control)) {  # A 1-horizon, non-nested lookback_control of feature lags.

    # Check if there is one list location for each feature in the dataset.
    if (length(lookback_control) != (ncol(data))) {
      stop("For a single forecast horizon, the length of the 'lookback_control' list should equal the number of features in
           the dataset. For multiple forecast horizons, 'lookback_control' is a nested list with length(lookback_control) ==
           length(horizons) and, one layer down, the nested list should have a length equal to the number of features in the dataset.
           'lookback_control' list slots with 'NULL' drops columns from the input data, and 'lookback_control' list slots with 0 are used for
           grouping columns and static features.")
    }

    # Clean the lookback_control list of impossible lag values for the given forecast horizon.
    # This will be an attribute for this function's return which is used for plotting.
    lookback_control <- lapply(seq_along(lookback_control), function(i) {

      if (is.null(groups)) {

        lookback_control[[i]][lookback_control[[i]] >= horizon]

      } else {

        if (i %in% c(group_cols, static_features)) {  # Set lags for grouping columns and static features to 0.

          lookback_control[[i]] <- 0

        } else {

          lookback_control[[i]] <- lookback_control[[i]][lookback_control[[i]] >= horizon]
        }
        lookback_control[[i]]
      }
    })  # Impossible lags for non-static features have been removed.

  } else if (length(horizon) > 1 && !is.null(lookback_control)) {  # A multiple-horizon, nested lookback_control of feature lags.

    lookback_control <- lapply(seq_along(lookback_control), function(i) {
      lapply(seq_along(lookback_control[[i]]), function(j) {

        if (is.null(groups)) {

          lookback_control[[i]][[j]][lookback_control[[i]][[j]] >= horizon[i]]

        } else {

          if (j %in% c(group_cols, static_features)) {  # Set lags for grouping columns to 0.

            lookback_control[[i]][[j]] <- 0

          } else {

            lookback_control[[i]][[j]] <- lookback_control[[i]][[j]][lookback_control[[i]][[j]] >= horizon[i]]
          }
          lookback_control[[i]][[j]]
        }
      })
    }) # Impossible lags for non-static features have been removed.
  } # Impossible lags in 'lookback_control' have been removed.

  # This will be used to remove the rows with NAs in our new lagged predictor dataset--rows 1:lookback_max at the begnning of the dataset.
  # This is only used for non-grouped datasets and allows easy forecasting with methods that can't handle NA values.
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
  # Setting the lagged feature loops to parallel processing depending on user input.
  if (isTRUE(use_future)) {
    lapply_function <- future.apply::future_lapply
  } else {
    lapply_function <- lapply
  }
  #----------------------------------------------------------------------------
  # Each item in the list is a data.frame of lagged features that allow direct forecasting.
  # to the given horizon.
  if (type == "train") {

    #i <- j <- 1
    # Loop over forecast model horizons [i] > features in dataset [j].
    data_out <- lapply(seq_along(horizon), function(i) {

      forecast_horizon <- horizon[i]

      # Only create lagged features that allow direct forecasting to max(i). If a single lookback vector is defined,
      # we'll do this filtering outside of the inner loop below.
      if (!is.null(lookback)) {
        lookback_over_horizon <- lookback[lookback >= forecast_horizon]
      }

      data_x <- lapply_function(1:ncol(data), function(j) {
        # Only create lagged features that allow direct forecasting to max(i)--unique lags for each feature.
        if (!is.null(lookback_control)) {
          # As a convenience to the user, a single-horizon forecast that uses a custom lookback doesn't need to be a nested list.
          if (length(horizon) == 1) {

            lookback_over_horizon <- lookback_control[[j]]

          } else {

            # A nested list of lags is needed for custom lookback_control(s) for each forecast horizon.
            lookback_over_horizon <- lookback_control[[i]][[j]]

            # Set for feature lags for grouping features and static features.
            if (length(lookback_over_horizon) == 0 && var_names[j] %in% c(groups, static_features)) {
              lookback_over_horizon <- 0
            }
          }
        }

        # If there are no feature-level lags suitable for the forecast horizon, return NULL for this feature-level lagged data.frame.
        # However, grouping and static features will pass through and be added to the output dataset without lags.
        if (all(!is.na(lookback_over_horizon), length(lookback_over_horizon) > 0) || var_names[j] %in% c(groups, static_features)) {

          #--------------------------------------------------------------------
          # Create a list of lag functions for dplyr::mutate_at(). This approach is approximately 30% faster than the
          # previous sapply approach of mutating and adding one lagged feature column at a time.
          lag_functions <- vector("list", length(lookback_over_horizon))
          for (i in seq_along(lag_functions)) {

            lag_functions[[i]] <- function(.) {
              dplyr::lag(unlist(.), lookback_over_horizon[i])  # Custom lag for this feature.
            }

            body(lag_functions[[i]])[[2]][[3]] <- get("lookback_over_horizon")[i]  # Change the body of the function to reflect the feature-specific lag.
          }
          #--------------------------------------------------------------------

            if (!is.null(groups)) {  # Create lagged features by group.

              # If the current feature in the loop is a grouping or static feature, return the grouping feature without lags.
              if (var_names[j] %in% c(groups, static_features)) {

                data_x <- data[, var_names[j], drop = FALSE]  # Exit the 'j' loop.

              } else {  # This feature is not a grouping feature and we'll compute lagged versions.

                data_x <- data[, c(groups, var_names[j]), drop = FALSE] %>%
                  dplyr::group_by_at(dplyr::vars(groups)) %>%
                  dplyr::mutate_at(dplyr::vars(var_names[j]), lag_functions)
                data_x <- data_x[, (ncol(data_x) - length(lag_functions) + 1):ncol(data_x), drop = FALSE]  # Keep only the lagged feature columns.
              }

            } else {  # No user-defined groupings, compute lagged variables without `dplyr::group_by`.

              data_x <- data[, var_names[j], drop = FALSE] %>%
                dplyr::mutate_at(dplyr::vars(var_names[j]), .funs = lag_functions)
              data_x <- data_x[, (ncol(data_x) - length(lag_functions) + 1):ncol(data_x), drop = FALSE]  # Keep only the lagged feature columns.

            }  # End feature-level lag creation across `lookback_over_horizon`.

          # Rename lagged features.
          if (!var_names[j] %in% c(groups, static_features)) {  # Non-grouped data.

            names(data_x) <- unlist(lapply(var_names[j], function(x){paste0(x, "_lag_", lookback_over_horizon)}))

          } else {  # Grouped data.

            data_x <- data_x[, 1, drop = FALSE]
            names(data_x) <- var_names[j]
          }

        } else {  # There are no user-specified feature lags appropriate for direct forecasting for this feature.

          data_x <- NULL
        }

        data_x
      })  # End loop 'j', the creation of lagged features for a given forecast model horizon.

      data_x <- dplyr::bind_cols(data_x)  # A single data.frame of lags for all features at this forecast model horizon.

      # Re-order the columns so that the grouping features, if any, are first; static features remain in-place.
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
        attr(data_out, "lookback") <- lookback_over_horizon  # Non-grouped data.
      } else {
        attr(data_out, "lookback") <- if (length(horizon) == 1) {lookback_control} else {lookback_control[[i]]}  # length(horizon) == 1 is the user convenience mentioned earlier.
      }

      data_out
    })  # End loop 'i' and return 'data_out'.
  }  # End `type = train` dataset creation.
  #----------------------------------------------------------------------------

  if (type == "forecast") {  # Create a dataset for forecasting H steps into the future.

    data_stop <- NULL  # An attribute for grouped datasets that is assigned to with <<- in the loop below.

    # Loop over forecast model horizons [i] > features in dataset [j].
    data_out <- lapply(seq_along(horizon), function(i) {

      forecast_horizon <- horizon[i]

      # Only create lagged features that allow direct forecasting to max(i). If a single lookback vector is defined,
      # we'll do this filtering outside of the inner loop below.
      if (!is.null(lookback)) {
        lookback_over_horizon <- lookback[lookback >= forecast_horizon]
      }

      data_x <- lapply_function(1:ncol(data), function(j) {

        # Only create lagged features that allow direct forecasting to max(i)--unique lags for each feature.
        if (!is.null(lookback_control)) {
          # As a convenience to the user a single-horizon forecast that uses a custom lookback doesn't need to be a nested list.
          if (length(horizon) == 1) {

            lookback_over_horizon <- lookback_control[[j]]

          } else {

            # A nested list of lags is needed for custom lookback_control(s) for each forecast horizon.
            lookback_over_horizon <- lookback_control[[i]][[j]]

            # Set feature lags for grouping features.
            if (var_names[j] %in% c(groups, static_features)) {
              lookback_over_horizon <- 0
            }
          }
        }

        # If there are no feature-level lags suitable for the forecast horizon, return NULL for this feature-level lagged data.frame.
        # However, grouping features will pass through and be added to the output dataset without lags.
        if (all(!is.na(lookback_over_horizon), length(lookback_over_horizon) > 0) || var_names[j] %in% c(groups, static_features)) {

          # Create a list of lag functions for dplyr::mutate_at(). This approach is approximately 30% faster than the
          # previous sapply approach of mutating and adding one lagged feature column at a time.
          # This list of functions has slightly different lags from type = 'train' to account for the whole
          # future aspect of this data.frame.
          lag_functions <- vector("list", length(lookback_over_horizon))
          for (k in seq_along(lag_functions)) {

            lag_functions[[k]] <- function(.) {
              dplyr::lag(unlist(.), lookback_over_horizon[k] - forecast_horizon)
            }

            body(lag_functions[[k]])[[2]][[3]] <- get('lookback_over_horizon')[k] - forecast_horizon  # Change the body of the function to reflect the feature-specific lag.
            names(lag_functions)[k] <- paste0(var_names[j], "_lag_", lookback_over_horizon[k])
          }

          if (!is.null(groups)) {  # Create lagged features by group.

            #------------------------------------------------------------------
            # If the current feature in the loop is a grouping feature, return the grouping feature without lags.
            # This operation only needs to be performed once. So, when the first grouping feature is encountered,
            # the block of code below returns the appropriate level of grouping with all group information as well
            # as all static feature information. The static features used in the forecast data.frame, if any, come from
            # the most recent data--i.e., the last row--from the grouped data.frame. This is a logical choice as far
            # as pulling static features into the future goes. To-do: Perhaps raise a warning if there are multiple,
            # non-unique values for a static feature or if there are NAs in the most recent data for static features.
            if (var_names[j] %in% c(groups)) {

              # Now that we know the column is a grouping column, is it the first grouping column?
              if (j == min(group_cols)) {

                data_x <- data[, c(groups, static_features), drop = FALSE] %>%
                  dplyr::group_by_at(dplyr::vars(groups)) %>%
                  dplyr::mutate("row_number" = 1:dplyr::n(),
                                "max_row_number" = max(.data$row_number, na.rm = TRUE)) %>%
                  dplyr::filter(.data$row_number == .data$max_row_number) %>%
                  dplyr::ungroup() %>%
                  dplyr::select(!!groups, !!static_features, .data$max_row_number)

                # Create the same, static dataset for forecasting into the future, the only difference
                # being an index which indicates the forecast horizon.
                data_x <- lapply(forecast_horizon:1, function(k) {
                  data_x$horizon <- k
                  data_x
                })

                # Create 1 forecasting dataset for the grouping and static features.
                data_x <- dplyr::bind_rows(data_x)

                # To-do: This calculation isn't really necessary at present because all forecasts are produced
                # for max(dates) + 1 and onward. I need to see if its removal affects anything downstream.
                data_x$row_number <- data_x$max_row_number + data_x$horizon

                data_x <- dplyr::select(data_x, .data$row_number, .data$horizon,
                                        groups, static_features)

                } else {  # Exit the 'j' loop and return 'NULL' because the group/static features are already computed.

                  data_x <- NULL
                }  # End grouped/static feature dataset creation for grouped data.

              } else if (!var_names[j] %in% c(static_features)) {  # A non-group or non-static-feature lagged feature for grouped data.

              # The purpose of the block of code below is to address the issue of creating forecasting data.frames for grouped
              # data when the groups have no data in the recent past--past being from the max(dates) or forecast date.
              # Instead of removing these groups from the dataset, we'll set the predictor values to NA for lags that would
              # be impossible given the forecast date and the date frequency. Namely, data from any date older than
              # frequency * max(lookback) isn't used by the model when forecasting. Setting these older values to NA is
              # simpler than adjusting lag values on the fly using the most recent group-level data.
              data_x <- dplyr::bind_cols("date" = dates, data[, c(groups, var_names[j])])

              numeric_date_frequency <- as.numeric(paste(unlist(stringr::str_extract_all(frequency, "0|1|2|3|4|5|6|7|8|9")), collapse = ""))

              furthest_lookback_in_time <- paste0(max(lookback_over_horizon, na.rm = TRUE) * numeric_date_frequency - 1, gsub("0|1|2|3|4|5|6|7|8|9", "", frequency))

              furthest_lookback_in_time <- max(dates, na.rm = TRUE) %m-% lubridate::period(furthest_lookback_in_time)

              data_x[data_x$date < furthest_lookback_in_time, var_names[j]] <- NA

              data_x <- data_x %>%
                dplyr::group_by_at(dplyr::vars(groups)) %>%
                dplyr::mutate("row_number" = 1:dplyr::n()) %>%
                dplyr::mutate_at(dplyr::vars(var_names[j]), lag_functions) %>%
                dplyr::mutate("max_row_number" = max(.data$row_number, na.rm = TRUE),
                              "horizon" = .data$max_row_number - .data$row_number + 1) %>%
                dplyr::filter(horizon <= forecast_horizon) %>%
                dplyr::mutate("horizon" = rev(.data$horizon),
                              "row_number" = .data$max_row_number + .data$horizon) %>%
                dplyr::ungroup() %>%
                dplyr::select(.data$row_number, horizon, groups, names(lag_functions))

              } else {  # Return 'NULL' for non-group static features.

                data_x <- NULL
              }
            #------------------------------------------------------------------

          } else {  # No user-defined groupings, compute lagged variables without `dplyr::group_by`.

            data_x <- data[, var_names[j], drop = FALSE] %>%
              dplyr::mutate("row_number" = 1:dplyr::n()) %>%
              dplyr::mutate_at(dplyr::vars(var_names[j]), lag_functions) %>%
              dplyr::mutate("max_row_number" = max(.data$row_number, na.rm = TRUE),
                            "horizon" = .data$max_row_number - .data$row_number + 1) %>%
              dplyr::filter(.data$horizon <= forecast_horizon) %>%
              dplyr::mutate("horizon" = rev(.data$horizon),
                            "row_number" = .data$max_row_number + .data$horizon) %>%
              dplyr::select(.data$row_number, .data$horizon, groups, names(lag_functions))

          }  # End feature-level lag creation across `lookback_over_horizon`.

        } else {  # There are no user-specified feature lags appropriate for direct forecasting for this feature.

          data_x <- NULL
        }

        data_x
      })  # End loop 'j', the creation of lagged features for a given forecast model horizon.

      # Merge all feature-level lags into 1 data.frame.
      if (is.null(groups)) {

        data_x <- data_x[!sapply(data_x, is.null)]
        data_x <- Reduce(function(x, y) {dplyr::full_join(x, y, by = c("row_number", "horizon"))}, data_x)

        if (is.null(dates)) {

          names(data_x)[names(data_x) == "row_number"] <- "index"

        } else {

          data_x <- dplyr::select(data_x, -.data$row_number)

          date_of_forecast <- data.frame("horizon" = 1:forecast_horizon,
                                         "index" = seq(max(dates, na.rm = TRUE), by = frequency, length.out = forecast_horizon + 1)[-1])

          data_x <- dplyr::left_join(data_x, date_of_forecast, by = "horizon")

          data_x <- data_x[, c(ncol(data_x), 1:(ncol(data_x) - 1))]
        }

      } else {  # Grouped dataset.

        data_x <- data_x[!sapply(data_x, is.null)]
        data_x <- Reduce(function(x, y) {try(dplyr::full_join(x, y, by = c("row_number", "horizon", groups)))}, data_x)

        # Keep row number, which gives the last row of actuals for each time-series, for the data_stop attribute.
        data_stop <<- data_x[, "row_number", drop = TRUE] - 1  # To-do: check the downstream effects of removing this 1 and re-indexing.

        data_x <- dplyr::select(data_x, -.data$row_number)

        date_of_forecast <- data.frame("horizon" = 1:forecast_horizon,
                                       "index" = seq(max(dates, na.rm = TRUE), by = frequency, length.out = forecast_horizon + 1)[-1])

        data_x <- dplyr::left_join(data_x, date_of_forecast, by = "horizon")

        data_x <- data_x[, c(ncol(data_x), 1:(ncol(data_x) - 1))]
      }

      attr(data_x, "horizon") <- forecast_horizon
      if (!is.null(lookback)) {
        attr(data_x, "lookback") <- lookback_over_horizon
      } else {
        attr(data_x, "lookback") <- if (length(horizon) == 1) {lookback_control} else {lookback_control[[i]]}
      }
      as.data.frame(data_x)
    })  # End loop 'i' and return 'data_out'
  }  # End `type = forecast` dataset creation.
  #----------------------------------------------------------------------------

  names(data_out) <- paste0("horizon_", horizons)

  # Global classes and attributes for the return object.
  attr(data_out, "type") <- type
  attr(data_out, "horizons") <- horizon
  attr(data_out, "outcome_cols") <- outcome_cols
  attr(data_out, "outcome_names") <- outcome_names
  attr(data_out, "predictor_names") <- var_names

  if (is.null(groups)) {
    attr(data_out, "row_indices") <- row_names[-(1:lookback_max)]
    if (is.null(dates)) {
      attr(data_out, "data_start") <- lookback_max + 1  # Removes NAs at the beginning of the dataset
      attr(data_out, "data_stop") <- max(row_names, na.rm = TRUE)
    } else {
      attr(data_out, "date_indices") <- dates
      attr(data_out, "frequency") <- frequency
      attr(data_out, "data_start") <- min(dates[lookback_max + 1], na.rm = TRUE)  # Removes NAs at the beginning of the dataset
      attr(data_out, "data_stop") <- max(dates, na.rm = TRUE)
    }
  } else {  # Grouped data requires a 'dates' argument.
    attr(data_out, "row_indices") <- row_names  # Don't remove intial rows because the data is grouped and will have lots of NAs throughout.
    attr(data_out, "date_indices") <- dates
    attr(data_out, "frequency") <- frequency
    attr(data_out, "data_start") <- min(dates, na.rm = TRUE)
    attr(data_out, "data_stop") <- max(dates, na.rm = TRUE) #  To-do: decide if we need the vector of stop dates from data_stop.
  }
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
#' Return a summary of a lagged_df object
#'
#' @param object An object of class 'lagged_df' from \code{create_lagged_df}.
#' @param ... Arguments passed to \code{base::summary}.
#' @return A printed summary of the contents of the lagged_df object.
#' @export
summary.lagged_df <- function(object, ...) {

  data <- object

  n_predictors_at_each_horizon <- unlist(lapply(data, function(x) {ncol(x) - 1}))
  horizons <- attributes(data)$horizons

  if (!methods::is(data, "lagged_df")) {
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
#' @param x An object of class 'lagged_df' from \code{create_lagged_df()}.
#' @param ... Arguments passed to \code{base::plot()}
#' @return A single plot of class 'ggplot' if \code{lookback} was specified in \code{create_lagged_df()};
#' a list of plots, one per feature, of class 'ggplot' if \code{lookback_control} was specified.
#' @example /R/examples/example_plot_lagged_df.R
#' @export
plot.lagged_df <- function(x, ...) {

  data <- x

  if (!methods::is(data, "lagged_df")) {
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

          if (all(!is.na(lookback_predictor) && length(lookback_predictor) > 0)) {
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

    p <- ggplot(data_plot, aes(x = .data$time, y = .data$horizon, fill = .data$feature))
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

      data_plot_predictor_1 <- dplyr::filter(data_plot, .data$feature == "Feature" & .data$predictor_number == i)
      data_plot_predictor_1$feature_match <- paste0(data_plot_predictor_1$horizon, "-", data_plot_predictor_1$time)
      data_plot_predictor_2 <- data_grid_past
      data_plot_predictor_2$feature <- "No feature"
      data_plot_predictor_2$feature_match <- paste0(data_plot_predictor_2$horizon, "-", data_plot_predictor_2$time)
      data_plot_predictor_2 <- data_plot_predictor_2[!(data_plot_predictor_2$feature_match %in% data_plot_predictor_1$feature_match), ]
      data_plot_predictor_3 <- dplyr::filter(data_plot, .data$feature == "Forecast")
      data_plot_predictor <- dplyr::bind_rows(data_plot_predictor_1, data_plot_predictor_2, data_plot_predictor_3)

      data_plot_predictor <- data_plot_predictor[as.numeric(data_plot_predictor$horizon) %in% horizon, ]
      data_plot_predictor$horizon <- factor(data_plot_predictor$horizon, levels = sort(unique(as.numeric(data_plot_predictor$horizon))), ordered = TRUE)

      p <- ggplot(data_plot_predictor, aes(x = .data$time, y = .data$horizon, fill = .data$feature))
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
