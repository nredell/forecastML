#' Create model training and forecasting datasets with lagged, grouped, dynamic, and static features
#'
#' Create a list of datasets with lagged, grouped, dynamic, and static features to (a) train forecasting models for
#' specified forecast horizons and (b) forecast into the future with a trained ML model.
#'
#' @param data A data.frame with the (a) target to be forecasted and (b) features/predictors. An optional date column can be given in the
#' \code{dates} argument (required for grouped time series). Note that `\code{orecastML} only works with regularly spaced date/time intervals and that missing
#' rows--usually due to periods when no data was collected--will result in incorrect feature lags.
#' Use \code{\link{fill_gaps}} to fill in any missing rows/data prior to running this function.
#' @param type The type of dataset to return--(a) model training or (b) forecast prediction. The default is \code{train}.
#' @param method The type of modeling dataset to create. \code{direct} returns 1 data.frame for each forecast horizon and
#' \code{multi_output} returns 1 data.frame for simultaneously modeling all forecast horizons. The default is \code{direct}.
#' @param outcome_col The column index--an integer--of the target to be forecasted. If \code{outcome_col != 1}, the
#' outcome column will be moved to position 1 and \code{outcome_col} will be set to 1 internally.
#' @param horizons A numeric vector of one or more forecast horizons, h, measured in dataset rows.
#' If \code{dates} are given, a horizon of 1, for example, would equal 1 * \code{frequency} in calendar time.
#' @param lookback A numeric vector giving the lags--in dataset rows--for creating the lagged features. All non-grouping,
#' non-static, and non-dynamic features in the input dataset, \code{data}, are lagged by the same values. The outcome is
#' also lagged by default. Either \code{lookback} or \code{lookback_control} need to be specified--but not both.
#' @param lookback_control A list of numeric vectors, specifying potentially unique lags for each feature. The length
#' of the list should equal \code{ncol(data)} and be ordered the same as the columns in \code{data}. Lag values for any grouping,
#' static, or dynamic feature columns are automatically coerced to 0 and not lagged. \code{list(NULL)} \code{lookback_control} values drop columns
#' from the input dataset. Either \code{lookback} or \code{lookback_control} need to be specified--but not both.
#' @param dates A vector or 1-column data.frame of dates/times with class 'Date' or 'POSIXt'. The length
#' of \code{dates} should equal \code{nrow(data)}. Required if \code{groups} are given.
#' @param frequency Date/time frequency. Required if \code{dates} are given. A string taking the same input as \code{base::seq.Date(..., by = "frequency")} or
#' \code{base::seq.POSIXt(..., by = "frequency")} e.g., '1 hour', '1 month', '7 days', '10 years' etc.
#' The highest frequency supported at present is '1 sec'.
#' @param dynamic_features A character vector of column names that identify features that change through time but which are not lagged (e.g., weekday or year).
#' If \code{type = "forecast"} and \code{method = "direct"}, these features will receive \code{NA} values; though, they can be filled in by the user after running this function.
#' @param groups A character vector of column names that identify the groups/hierarchies when multiple time series are present. These columns are used as model features but
#' are not lagged. Note that combining feature lags with grouped time series will result in \code{NA} values throughout the data.
#' @param static_features For grouped time series only. A character vector of column names that identify features that do not change through time.
#' These columns are not lagged. If \code{type = "forecast"}, these features will be filled forward using the most recent value for the group.
#' @param use_future Boolean. If \code{TRUE}, the \code{future.apply} package is used for creating lagged data.frames.
#' \code{multisession} or \code{multicore} futures are especially useful for (a) grouped time series with many groups and
#' (b) high-dimensional datasets with many lags per feature. Run \code{future::plan(future::multiprocess)} prior to this
#' function to set up multissession or multicore parallel dataset creation.
#' @param keep_rows Boolean. For non-grouped time series, keep the \code{1:max(lookback)} rows at the beginning of the time series. These rows will
#' contain missing values for lagged features that "look back" before the start of the dataset.
#' @return An S3 object of class 'lagged_df' or 'grouped_lagged_df': A list of data.frames with new columns for the lagged/non-lagged features.
#' For \code{method = "direct"}, the length of the returned list is equal to the number of forecast horizons and is in the order of
#' horizons supplied to the \code{horizons} argument. Horizon-specific datasets can be accessed with
#' \code{my_lagged_df$horizon_h} where 'h' gives the forecast horizon.
#' For \code{method = "multi_output"}, the length of the returned list is 1. Horizon-specific datasets can be accessed with
#' \code{my_lagged_df$horizon_1_3_5} where "1_3_5" represents the forecast horizons passed in \code{horizons}.
#'
#' The contents of the returned data.frames are as follows:
#'
#' \describe{
#'   \item{\strong{type = 'train', non-grouped:}}{A data.frame with the outcome and lagged/dynamic features.}
#'   \item{\strong{type = 'train', grouped:}}{A data.frame with the outcome and unlagged grouping columns followed by lagged, dynamic, and static features.}
#'   \item{\strong{type = 'forecast', non-grouped:}}{(1) An 'index' column giving the row index or date of the
#'   forecast periods (e.g., a 100 row non-date-based training dataset would start with an index of 101). (2) A 'horizon' column
#'   that indicates the forecast period from \code{1:max(horizons)}. (3) Lagged features identical to the
#'   'train', non-grouped dataset.}
#'   \item{\strong{type = 'forecast', grouped:}}{(1) An 'index' column giving the date of the
#'   forecast periods. The first forecast date for each group is the maximum date from the \code{dates} argument
#'   + 1 * \code{frequency} which is the user-supplied date/time frequency.(2) A 'horizon' column that indicates
#'   the forecast period from \code{1:max(horizons)}. (3) Lagged, static, and dynamic features identical to the 'train', grouped dataset.}
#' }
#' @section Attributes:
#'
#' \itemize{
#'   \item \code{names}: The horizon-specific datasets that can be accessed with \code{my_lagged_df$horizon_h}.
#'   \item \code{type}: Training, \code{train}, or forecasting, \code{forecast}, dataset(s).
#'   \item \code{method}: \code{direct} or \code{multi_output}.
#'   \item \code{horizons}: Forecast horizons measured in dataset rows.
#'   \item \code{outcome_col}: The column index of the target being forecasted.
#'   \item \code{outcome_cols}: If \code{method = multi_output}, the column indices of the multiple outputs in the transformed dataset.
#'   \item \code{outcome_name}: The name of the target being forecasted.
#'   \item \code{outcome_names}: If \code{method = multi_output}, the column names of the multiple outputs in the transformed dataset.
#'   The names take the form "outcome_name_h" where 'h' is a horizon passed in \code{horizons}.
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
#' @import dplyr
#' @import ggplot2
#' @importFrom stats as.formula complete.cases cov sd
#' @importFrom magrittr %>%
#' @importFrom lubridate %m-%
#' @importFrom rlang .data
#' @importFrom purrr map2
#' @importFrom data.table :=
#'
#' @export
create_lagged_df <- function(data, type = c("train", "forecast"), method = c("direct", "multi_output"),
                             outcome_col = 1L, horizons, lookback = NULL,
                             lookback_control = NULL, dates = NULL, frequency = NULL, dynamic_features = NULL,
                             groups = NULL, static_features = NULL, use_future = FALSE, keep_rows = FALSE) {

  if (!methods::is(data, c("data.frame"))) {
    stop("The 'data' argument takes an object of class 'data.frame'.")
  }

  if (!any(type %in% c("train", "forecast"))) {
    stop("The 'type' argument needs to be one of 'train' or 'forecast'.")
  }

  if (length(outcome_col) != 1 || !methods::is(outcome_col, c("numeric"))) {
    stop("The 'outcome_col' argument needs to be a positive integer of length 1.")
  }

  if (!length(horizons) >= 1 || !methods::is(horizons, c("numeric"))) {
    stop("The 'horizons' argument needs to be a numeric vector of length >= 1.")
  }

  if (!all(horizons < nrow(data))) {
    stop("The forecast horizons need to be less than nrow(data).")
  }

  if (all(is.null(lookback), is.null(lookback_control))) {
    stop("Enter an argument for either `lookback`--a feature lag vector--or `lookback_control`--a list of feature lag vectors.")
  }

  if (!xor(is.null(lookback), is.null(lookback_control))) {
    stop("Enter an argument for either `lookback`--a feature lag vector--or `lookback_control`--a list of feature lag vectors.")
  }

  if (!is.null(lookback) && (!length(lookback) >= 1 || !all(lookback > 0) || !methods::is(lookback, c("numeric")))) {
    stop("The 'lookback' argument needs to be a numeric vector of positive integers of length >= 1.")
  }

  if (method[1] == "direct" && !is.null(lookback) && !max(lookback) >= min(horizons)) {
    stop("The highest lookback needs to be >= the shortest forecast horizons to allow for direct forecasting with lagged features.")
  }

  if (length(horizons) == 1 && !is.null(lookback_control)) {  # A 1-horizon, non-nested lookback_control of feature lags.

    # Check if there is one list location for each feature in the dataset.
    if (length(lookback_control) != ncol(data)) {
      stop("For a single forecast horizons, the length of the 'lookback_control' list should equal the number of features in
           the dataset. For multiple forecast horizons, 'lookback_control' is a nested list with length(lookback_control) ==
           length(horizons) and, one layer down, the nested list should have a length equal to the number of features in the dataset.
           'lookback_control' list slots with 'NULL' drops columns from the input data, and 'lookback_control' list slots with 0 are used for
           grouping columns and dynamic/static features (grouping/dynamic/static features will automatically be coerced to 0s).")
    }
  }

  if (!is.null(dynamic_features) && (!length(dynamic_features) >= 1 || !methods::is(dynamic_features, c("character")))) {
    stop("The 'dynamic_features' argument needs to be a character vector of length >= 1.")
  }

  if (!is.null(static_features) && (!length(static_features) >= 1 || !methods::is(static_features, c("character")))) {
    stop("The 'static_features' argument needs to be a character vector of length >= 1.")
  }

  dates <- if (methods::is(dates, "data.frame")) {

    dates[, 1, drop = TRUE]

  } else {

    dates
  }

  if (!is.null(dates) && !methods::is(dates, "Date") && !methods::is(dates, c("POSIXt"))) {
    stop("The 'dates' argument should be an object of class 'Date' or 'POSIXt'.")
  }

  if (!is.null(dates) && length(dates) != nrow(data)) {
    stop("The 'dates' argument needs to be a vector or 1-column data.frame of length nrow(data)
         of dates with class 'Date' or 'POSIXt'.")
  }

  if (!is.null(dates) && is.null(frequency)) {
    stop("The 'frequency' argument needs to be specified along with the 'dates' argument.
         See base::seq.Date() or base::seq.POSIXt() for valid date/time frequencies e.g., '1 hour',
         '1 day', '3 months', '10 years' etc.")
  }

  if (!is.null(frequency) && !grepl("sec|min|hour|day|week|month|quarter|year", frequency)) {
    stop("The 'frequency' argument should be a string containing one of 'sec', 'min', 'hour', 'day', 'week',
         'month', 'quarter', or 'year'. This can optionally be preceded by a positive integer and a space
         and/or followed by an 's'.")
  }

  if (!is.null(groups) && is.null(dates)) {
    stop("The 'dates' argument needs to be specified with grouped data.")
  }
  #--------------------------------------------------------------------------
  # The outcomes will always be moved to the front of the dataset and eventually more than one
  # outcome will be supported.
  if (outcome_col != 1) {
    data <- dbplyr::bind_cols(data[, outcome_col, drop = FALSE], data[, -(outcome_col), drop = FALSE])
    outcome_col <- 1
  }

  outcome_name <- names(data)[outcome_col]
  type <- type[1]  # Model-training datasets are the default.
  method <- method[1]  # Direct forecasting is the default.
  row_names <- 1:nrow(data)
  n_instances <- max(row_names, na.rm = TRUE)
  var_names <- names(data)

  dynamic_feature_cols <- which(names(data) %in% dynamic_features)

  # Group column indices to keep grouping columns when checking the 'lookback_cotrol' argument--the value of which should be 0 for grouping columns.
  if (!is.null(groups)) {

    group_cols <- which(names(data) %in% groups)

    static_feature_cols <- which(names(data) %in% static_features)

    # This block of code ensures that unsorted data.frames will return the correct lagged feature values.
    data$forecastML_dates <- dates  # Adding to the data temporarily for sorting.

    data <- data %>%
      dplyr::arrange(!!!rlang::syms(groups), .data$forecastML_dates)

    dates <- data$forecastML_dates
    data$forecastML_dates <- NULL
  }
  #----------------------------------------------------------------------------
  # If the outcome is a factor, save the levels out as an attribute.
  if (methods::is(data[, outcome_col], "factor")) {

    outcome_levels <- levels(data[, outcome_col])

  } else {

    outcome_levels <- NULL
  }
  #----------------------------------------------------------------------------
  # Outcome data.
  if (method == "direct") {  # An nrow(data) by 1 data.frame.

    data_y <- data[, outcome_col, drop = FALSE]

  } else if (method == "multi_output") {  # An nrow(data) by length(horizons) data.frame.

    data_y <- forecastML_create_multi_outcome(data, outcome_name, horizons, groups, outcome_levels)
  }
  #--------------------------------------------------------------------------
  # For method = "direct", remove feature lags in lookback_control that don't support forecasting to the given horizon.
  # For method = "multi_output", all feature lags can be used for all forecast horizons.
  if (!is.null(lookback_control) && method == "direct") {

    lookback_control <- forecastML_filter_lookback_control(lookback_control, horizons, groups, group_cols, static_feature_cols, dynamic_feature_cols)
  }
  #--------------------------------------------------------------------------
  # This will be used to remove the rows with NAs in our new lagged predictor dataset--rows 1:lookback_max at the begnning of the dataset.
  # This is only used for non-grouped datasets and allows easy forecasting with methods that can't handle NA values.
  if (!is.null(lookback)) {

    lookback_max <- max(lookback, na.rm = TRUE)

  } else {

    lookback_max <- max(unlist(lookback_control), na.rm = TRUE)
  }
  #----------------------------------------------------------------------------
  # Setting the lagged feature loops to parallel processing depending on user input.
  if (isTRUE(use_future)) {

    lapply_function <- future.apply::future_lapply

  } else {

    lapply_function <- base::lapply
  }
  #----------------------------------------------------------------------------
  # Each item in the list is a data.frame of lagged features that allow direct forecasting.
  # to the given horizons.
  if (type == "train") {

    # Loop over forecast model horizons [i] > features in dataset [j].
    data_out <- lapply(

      if (method == "direct") {  # 1 dataset for each model.

        seq_along(horizons)

        } else if (method == "multi_output") {  # 1 dataset for the 1 model.

          1

        }, function(i) {

      forecast_horizon <- horizons[i]

      # Only create lagged features that allow direct forecasting to max(i). If a single lookback vector is defined,
      # we'll do this filtering outside of the inner loop below.
      if (!is.null(lookback)) {

        if (method == "direct") {

          lookback_over_horizon <- lookback[lookback >= forecast_horizon]

        } else if (method == "multi_output") {

          lookback_over_horizon <- (lookback - 1)[(lookback - 1) >= 0]
        }
      }

      data_x <- lapply_function(1:ncol(data), function(j, future.packages, future.seed) {

        if (!is.null(lookback_control)) {
          # As a convenience to the user, a single-direct-horizon forecast that uses a custom lookback doesn't need to be a nested list.
          if (length(horizons) == 1) {

            if (method == "direct") {

              lookback_over_horizon <- lookback_control[[j]]

            } else if (method == "multi_output") {

              lookback_over_horizon <- (lookback_control[[j]] - 1)[(lookback_control[[j]] - 1) >= 0]
            }

            } else {  # Multiple forecast horizons.

              if (method == "direct") {

                lookback_over_horizon <- lookback_control[[i]][[j]]

              } else if (method == "multi_output") {

                lookback_over_horizon <- (lookback_control[[i]][[j]] - 1)[(lookback_control[[i]][[j]] - 1) >= 0]
              }
          }
        }  # End lookback_control lag adjustment.

        # If there are no feature-level lags suitable for the forecast horizons, skip the code below and return NULL for this feature-level lagged data.frame.
        # However, grouping, dynamic, and static features will pass through and be added to the output dataset without lags.
        if (length(lookback_over_horizon) > 0 || var_names[j] %in% c(groups, dynamic_features, static_features)) {

          #--------------------------------------------------------------------
          # Create a list of lag functions for dplyr::mutate(). This approach is approximately 30% faster than the
          # previous dplyr::do sapply() approach of mutating and adding one lagged feature column at a time.
          lag_functions <- vector("list", length(lookback_over_horizon))
          for (k in seq_along(lag_functions)) {

            lag_functions[[k]] <- function(.) {
              dplyr::lag(unlist(.), lookback_over_horizon[k])  # Custom lag for this feature.
            }

            body(lag_functions[[k]])[[2]][[3]] <- get("lookback_over_horizon")[k]  # Change the body of the function to reflect the feature-specific lag.
            names(lag_functions)[k] <- if (method == "direct") {
                paste0(var_names[j], "_lag_", lookback_over_horizon[k])
              } else if (method == "multi_output") {
                paste0(var_names[j], "_lag_", lookback_over_horizon[k] + 1)
              }
            }
          #--------------------------------------------------------------------

            if (!is.null(groups)) {  # Create lagged features by group.

              # If the current feature in the loop is a grouping/dynamic/static feature, return the feature without lags.
              if (var_names[j] %in% c(groups, dynamic_features, static_features)) {

                data_x <- data[, var_names[j], drop = FALSE]  # Exit the 'j' loop.

              } else {  # This feature is not a grouping/dynamic/static feature and we'll compute lagged versions.

                # data_dt <- dtplyr::lazy_dt(data[, c(groups, var_names[j]), drop = FALSE])  # Hold off until dplyr 1.0.0.
                data_dt <- data[, c(groups, var_names[j]), drop = FALSE]

                data_x <- data_dt %>%
                  dplyr::group_by_at(dplyr::vars(groups)) %>%
                  dplyr::mutate(dplyr::across(dplyr::all_of(!!var_names[j]), lag_functions, .names = "{fn}")) %>%
                  dplyr::as_tibble()

                data_x <- data_x[, (ncol(data_x) - length(lag_functions) + 1):ncol(data_x), drop = FALSE]  # Keep only the lagged feature columns.
              }

            } else {  # No user-defined groupings, compute lagged variables without dplyr::group_by().

              # If the current feature in the loop is a dynamic feature, return the feature feature without lags.
              if (var_names[j] %in% dynamic_features) {

                data_x <- data[, var_names[j], drop = FALSE]  # Exit the 'j' loop.

              } else {

                # data_dt <- dtplyr::lazy_dt(data[, var_names[j], drop = FALSE])  # Hold off until dplyr 1.0.0.
                data_dt <- data[, var_names[j], drop = FALSE]

                data_x <- data_dt %>%
                  dplyr::mutate(dplyr::across(dplyr::all_of(!!var_names[j]), lag_functions, .names = "{fn}")) %>%
                  dplyr::as_tibble()
                data_x <- data_x[, (ncol(data_x) - length(lag_functions) + 1):ncol(data_x), drop = FALSE]  # Keep only the lagged feature columns.
              }
            }  # End feature-level lag creation across `lookback_over_horizon`.

        } else {  # There are no user-specified feature lags appropriate for direct forecasting for this feature.

          data_x <- NULL
        }

        data_x
      }, future.packages = "dplyr", future.seed = 1)  # End loop 'j', the creation of lagged features for a given forecast model horizons.

      data_x <- dplyr::bind_cols(data_x)  # A single data.frame of lags for all features at a given forecast model horizons.

      # Re-order the columns so that the grouping features, if any, are first; dynamic/static features remain in-place.
      if (!is.null(groups)) {
        data_x <- dplyr::select(data_x, groups, names(data_x)[!names(data_x) %in% groups])
      }

      # The complete dataset for a given forecast model horizons.
      data_out <- dplyr::bind_cols(data_y, data_x)

      # If the forecast is grouped, leave the NAs in the dataset for the user because the ML model used for these
      # cases will likely handle NA values.
      if (is.null(groups) && method == "direct") {

        if (isFALSE(keep_rows)) {
          data_out <- data_out[-(1:lookback_max), ]  # Remove the first rows with NAs in lagged features.
        }
      }

      if (method == "direct") {
        attr(data_out, "horizons") <- forecast_horizon  # 1 dataset per horizon.
      } else if (method == "multi_output") {
        attr(data_out, "horizons") <- horizons  # 1 dataset for all horizons.
      }

      if (!is.null(lookback)) {
        attr(data_out, "lookback") <- lookback_over_horizon
      } else {
        attr(data_out, "lookback") <- if (length(horizons) == 1) {lookback_control} else {lookback_control[[i]]}  # length(horizons) == 1 is the user convenience mentioned earlier.
      }

      as.data.frame(data_out)
    })  # End loop 'i' and return 'data_out'.
  }  # End 'type = train' dataset creation.
  #----------------------------------------------------------------------------
  #----------------------------------------------------------------------------

  if (type == "forecast") {  # Create a dataset for forecasting h steps into the future.

    # Loop over forecast model horizons [i] > features in dataset [j].
    data_out <- lapply(

      if (method == "direct") {  # 1 dataset for each model.

        seq_along(horizons)

      } else if (method == "multi_output") {  # 1 dataset for the 1 model.

        1

      }, function(i) {

      forecast_horizon <- horizons[i]

      # Only create lagged features that allow direct forecasting to max(i). If a single lookback vector is defined,
      # we'll do this filtering outside of the inner loop below.
      if (!is.null(lookback)) {

        if (method == "direct") {

          lookback_over_horizon <- lookback[lookback >= forecast_horizon]

        } else if (method == "multi_output") {

          lookback_over_horizon <- (lookback - 1)[(lookback - 1) >= 0]
        }
      }

      data_x <- lapply_function(1:ncol(data), function(j, future.packages, future.seed) {

        #----------------------------------------------------------------------
        if (!is.null(lookback_control)) {
          # As a convenience to the user, a single-direct-horizon forecast that uses a custom lookback doesn't need to be a nested list.
          if (length(horizons) == 1) {

            if (method == "direct") {

              lookback_over_horizon <- lookback_control[[j]]

            } else if (method == "multi_output") {

              lookback_over_horizon <- (lookback_control[[j]] - 1)[(lookback_control[[j]] - 1) >= 0]
            }

          } else {  # Multiple forecast horizons.

            if (method == "direct") {

              lookback_over_horizon <- lookback_control[[i]][[j]]

            } else if (method == "multi_output") {

              lookback_over_horizon <- (lookback_control[[i]][[j]] - 1)[(lookback_control[[i]][[j]] - 1) >= 0]
            }
          }
        }  # End lookback_control lag adjustment.
        #----------------------------------------------------------------------
        # If there are no feature-level lags suitable for the forecast horizons, return NULL for this feature-level lagged data.frame.
        # However, grouping features will pass through and be added to the output dataset without lags.
        if (length(lookback_over_horizon) > 0 || var_names[j] %in% c(groups, dynamic_features, static_features)) {

          #--------------------------------------------------------------------
          # This list of functions has slightly different lags from type = 'train' to account for the whole
          # future aspect of this data.frame.
          lag_functions <- vector("list", length(lookback_over_horizon))

          if (method == "direct") {

            for (k in seq_along(lag_functions)) {

              lag_functions[[k]] <- function(.) {
                dplyr::lag(unlist(.), lookback_over_horizon[k] - forecast_horizon)
              }

              body(lag_functions[[k]])[[2]][[3]] <- get('lookback_over_horizon')[k] - forecast_horizon  # Change the body of the function to reflect the feature-specific lag.
              names(lag_functions)[k] <- paste0(var_names[j], "_lag_", lookback_over_horizon[k])
            }

          } else if (method == "multi_output") {

            for (k in seq_along(lag_functions)) {

              lag_functions[[k]] <- function(.) {
                dplyr::lag(unlist(.), lookback_over_horizon[k])  # Custom lag for this feature.
              }

              body(lag_functions[[k]])[[2]][[3]] <- get("lookback_over_horizon")[k]  # Change the body of the function to reflect the feature-specific lag.
              names(lag_functions)[k] <- paste0(var_names[j], "_lag_", lookback_over_horizon[k] + 1)
            }
          } # End custom feature lag function creation.
          #--------------------------------------------------------------------

          if (!is.null(groups)) {  # Create lagged features by group.

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

                #data_dt <- dtplyr::lazy_dt(data[, c(groups, static_features), drop = FALSE])
                #data_dt <- data[, c(groups, static_features), drop = FALSE]

                data_x <- data[, c(groups, static_features), drop = FALSE] %>%
                  dplyr::group_by_at(dplyr::vars(groups)) %>%
                  dplyr::mutate("row_number" = 1:dplyr::n(),
                                "max_row_number" = max(.data$row_number, na.rm = TRUE)) %>%
                  dplyr::filter(.data$row_number == .data$max_row_number) %>%
                  dplyr::ungroup() %>%
                  dplyr::select(!!groups, !!static_features, .data$max_row_number) %>%
                  dplyr::as_tibble()

                # Create the same, static dataset for forecasting into the future, the only difference
                # being an index which indicates the forecast horizons.
                data_x <- lapply(forecast_horizon:1, function(k) {
                  data_x$horizon <- k
                  data_x
                })

                # Create 1 forecasting dataset for the grouping and static features.
                data_x <- dplyr::bind_rows(data_x)

                # Used for merging with other feature-level lagged datasets.
                data_x$row_number <- data_x$max_row_number + data_x$horizon

                data_x <- dplyr::select(data_x, .data$row_number, .data$horizon, groups, static_features)

                } else {  # Exit the 'j' loop and return 'NULL' because the group/static features are already computed.

                  data_x <- NULL
                }  # End grouped/static feature dataset creation for grouped data.

              } else if (!var_names[j] %in% c(static_features)) {  # A non-group or non-static-feature lagged feature for grouped data.

              data_x <- dplyr::bind_cols("date" = dates, data[, c(groups, var_names[j])])

              if (!var_names[j] %in% c(dynamic_features)) {  # Lagged, non-dynamic features

                #data_dt <- dtplyr::lazy_dt(data_x)
                #data_dt <- data_x

                data_x <- data_x %>%
                  dplyr::group_by_at(dplyr::vars(groups)) %>%
                  dplyr::mutate("row_number" = 1:dplyr::n()) %>%
                  dplyr::mutate(dplyr::across(dplyr::all_of(!!var_names[j]), lag_functions, .names = "{fn}")) %>%
                  dplyr::mutate("max_row_number" = max(.data$row_number, na.rm = TRUE),
                                "horizon" = .data$max_row_number - .data$row_number + 1) %>%
                  dplyr::filter(.data$horizon <= forecast_horizon) %>%
                  dplyr::mutate("horizon" = rev(.data$horizon),
                                "row_number" = .data$max_row_number + .data$horizon) %>%
                  dplyr::ungroup() %>%
                  dplyr::select(.data$row_number, .data$horizon, groups, names(lag_functions)) %>%
                  dplyr::as_tibble()

              } else {

                #data_dt <- dtplyr::lazy_dt(data_x)
                #data_dt <- data_x

                if (method == "direct") {

                  data_x <- data_x %>%
                    dplyr::group_by_at(dplyr::vars(groups)) %>%
                    dplyr::mutate("row_number" = 1:dplyr::n()) %>%
                    dplyr::mutate("max_row_number" = max(.data$row_number, na.rm = TRUE),
                                  "horizon" = .data$max_row_number - .data$row_number + 1) %>%
                    dplyr::filter(.data$horizon <= forecast_horizon) %>%
                    dplyr::mutate("horizon" = rev(.data$horizon),
                                  "row_number" = .data$max_row_number + .data$horizon) %>%
                    dplyr::ungroup() %>%
                    dplyr::select(.data$row_number, .data$horizon, !!groups) %>%
                    dplyr::as_tibble()

                  data_x[, var_names[j]] <- NA  # This is direct forecasting without predicting the predictors.

                } else if (method == "multi_output") {

                  data_x <- data_x %>%
                    dplyr::group_by_at(dplyr::vars(groups)) %>%
                    dplyr::mutate("row_number" = 1:dplyr::n()) %>%
                    dplyr::mutate("max_row_number" = max(.data$row_number, na.rm = TRUE),
                                  "horizon" = .data$max_row_number - .data$row_number + 1) %>%
                    dplyr::filter(.data$horizon <= forecast_horizon) %>%
                    dplyr::mutate("horizon" = rev(.data$horizon),
                                  "row_number" = .data$max_row_number + .data$horizon) %>%
                    dplyr::ungroup() %>%
                    dplyr::select(.data$row_number, .data$horizon, !!groups, !!var_names[j]) %>%
                    dplyr::as_tibble()
                }
              }

              } else {  # Return 'NULL' for non-group static features as they've already been computed.

                data_x <- NULL
              }
            #------------------------------------------------------------------

          } else {  # No user-defined groupings, compute lagged variables without dplyr::group_by().

            if (!var_names[j] %in% c(dynamic_features)) {  # Lagged, non-dynamic features, static and group features are not present.

              if (method == "direct") {

                data_x <- data[, var_names[j], drop = FALSE] %>%
                  dplyr::mutate("row_number" = !!row_names) %>%
                  dplyr::mutate(dplyr::across(dplyr::all_of(!!var_names[j]), lag_functions, .names = "{fn}")) %>%
                  dplyr::mutate("max_row_number" = !!n_instances,
                                "horizon" = .data$max_row_number - .data$row_number + 1) %>%
                  dplyr::filter(.data$horizon <= forecast_horizon) %>%
                  dplyr::mutate("horizon" = rev(.data$horizon),
                                "row_number" = .data$max_row_number + .data$horizon) %>%
                  dplyr::select(.data$row_number, .data$horizon, names(lag_functions)) %>%
                  dplyr::as_tibble()

              } else if (method == "multi_output") {

                data_x <- data[, var_names[j], drop = FALSE] %>%
                  dplyr::mutate(dplyr::across(dplyr::all_of(!!var_names[j]), lag_functions, .names = "{fn}")) %>%
                  dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
                  dplyr::mutate("row_number" = !!n_instances,  # Hard-coded for merging feature-level data.
                                "horizon" = !!horizons[1]) %>%  # Hard-coded for merging feature-level data.
                  dplyr::select(.data$row_number, .data$horizon, names(lag_functions)) %>%
                  dplyr::as_tibble()
              }

            } else {  # Dynamic features.

              if (method == "direct") {

                data_x <- data.frame("row_number" = n_instances + 1:forecast_horizon, "horizon" = 1:forecast_horizon)
                data_x[, var_names[j]] <- NA  # This is direct forecasting without predicting the predictors.

              } else if (method == "multi_output") {  # Without groups, this value is in the last row of the input data.

                data_x <- data.frame("row_number" = n_instances, "horizon" = horizons[1])
                data_x[, var_names[j]] <- data[n_instances, var_names[j], drop = TRUE]
              }
            }
          }  # End feature-level lag creation across 'lookback_over_horizon'.

        } else {  # There are no user-specified feature lags appropriate for direct forecasting for this feature.

          data_x <- NULL
        }

        data_x
      }, future.packages = "dplyr", future.seed = 1)  # End loop 'j', the creation of lagged features for a given forecast model horizon.
      #------------------------------------------------------------------------
      # Merge all feature-level lags into 1 data.frame.
      data_x <- data_x[!sapply(data_x, is.null)]

      if (is.null(groups)) {

        data_x <- Reduce(function(x, y) {dplyr::full_join(x, y, by = c("row_number", "horizon"))}, data_x)

      } else {

        data_x <- Reduce(function(x, y) {try(dplyr::full_join(x, y, by = c("row_number", "horizon", groups)))}, data_x)
      }

      if (is.null(dates)) {  # Single time series without dates.

        names(data_x)[names(data_x) == "row_number"] <- "index"

        if (method == "multi_output") {
          data_x$index <- paste(n_instances + horizons, collapse = ", ")  # There is only 1 row in the forecast data.frame.
          data_x$horizon <- paste(horizons, collapse = ", ")  # There is only 1 row in the forecast data.frame.
        }

      } else {  # Single or multiple time series with dates.

        data_x <- dplyr::select(data_x, -.data$row_number)  # The index will be a date from a merged template.

        date_of_forecast <- data.frame("horizon" = 1:forecast_horizon,
                                       "index" = seq(max(dates, na.rm = TRUE), by = frequency, length.out = forecast_horizon + 1)[-1])

        data_x <- dplyr::left_join(data_x, date_of_forecast, by = "horizon")

        data_x <- data_x[, c(ncol(data_x), 1:(ncol(data_x) - 1))]

        if (method == "multi_output") {  # There is only 1--per group--row in the forecast data.frame so these columns will be collapsed into a string.

          data_x <- data_x %>%
            dplyr::group_by_at(dplyr::vars(groups)) %>%
            dplyr::filter(dplyr::row_number() == dplyr::n())

          data_x$index <- paste(seq(max(dates, na.rm = TRUE), by = frequency, length.out = max(horizons, na.rm = TRUE) + 1)[-1][horizons], collapse = ", ")
          data_x$horizon <- paste(horizons, collapse = ", ")
        }
      }  # End formatting of the forecast dataset for horizon 'i'.
      #------------------------------------------------------------------------
      attr(data_x, "horizons") <- forecast_horizon
      if (!is.null(lookback)) {
        attr(data_x, "lookback") <- lookback_over_horizon
      } else {
        attr(data_x, "lookback") <- if (length(horizons) == 1) {lookback_control} else {lookback_control[[i]]}
      }
      as.data.frame(data_x)
    })  # End loop 'i' and return 'data_out'.
  }  # End 'type = forecast' dataset creation.
  #----------------------------------------------------------------------------

  if (method == "direct") {
    names(data_out) <- paste0("horizon_", horizons)
  } else if (method == "multi_output") {
    names(data_out) <- paste0("horizon_", paste(horizons, collapse = "_"))
  }

  # Global classes and attributes for the return object.
  attr(data_out, "type") <- type
  attr(data_out, "method") <- method
  attr(data_out, "horizons") <- horizons
  attr(data_out, "outcome_col") <- outcome_col
  attr(data_out, "outcome_cols") <- if (method == "direct") {outcome_col} else if (method == "multi_output") {outcome_col:(length(horizons))}
  attr(data_out, "outcome_name") <- outcome_name
  attr(data_out, "outcome_names") <- if (method == "direct") {outcome_name} else if (method == "multi_output") {names(data_y)}
  attr(data_out, "outcome_levels") <- outcome_levels
  attr(data_out, "predictor_names") <- var_names
  attr(data_out, "dynamic_features") <- dynamic_features
  attr(data_out, "static_features") <- static_features

  if (is.null(groups)) {
    if (isFALSE(keep_rows) && method == "direct") {
      attr(data_out, "row_indices") <- row_names[-(1:lookback_max)]
    } else {
      attr(data_out, "row_indices") <- row_names
    }
    if (is.null(dates)) {
      if (isFALSE(keep_rows) && method == "direct") {
        attr(data_out, "data_start") <- lookback_max + 1  # Removes NAs at the beginning of the dataset
      } else {
        attr(data_out, "data_start") <- min(row_names, na.rm = TRUE)  # Keep NAs at the beginning of the dataset
      }
      attr(data_out, "data_stop") <- max(row_names, na.rm = TRUE)
    } else {  # Non-grouped time series with dates.
      if (isFALSE(keep_rows) && method == "direct") {
        attr(data_out, "date_indices") <- dates[-(1:lookback_max)]
      } else {
        attr(data_out, "date_indices") <- dates
      }
      attr(data_out, "frequency") <- frequency
      if (isFALSE(keep_rows) && method == "direct") {
        attr(data_out, "data_start") <- min(dates[lookback_max + 1], na.rm = TRUE)  # Removes NAs at the beginning of the dataset
      } else {
        attr(data_out, "data_start") <- min(dates, na.rm = TRUE)  # Keep NAs at the beginning of the dataset
      }
      attr(data_out, "data_stop") <- max(dates, na.rm = TRUE)
    }
  } else {  # Grouped data requires a 'dates' argument.
    attr(data_out, "row_indices") <- row_names  # Don't remove intial rows because the data is grouped and will have lots of NAs throughout.
    attr(data_out, "date_indices") <- dates
    attr(data_out, "frequency") <- frequency
    attr(data_out, "data_start") <- min(dates, na.rm = TRUE)
    attr(data_out, "data_stop") <- max(dates, na.rm = TRUE)
  }
  attr(data_out, "groups") <- groups

  if (method == "multi_output") {
    attr(data_out, "outcome") <- data[, outcome_col, drop = FALSE]
  }

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
#' @param object An object of class 'lagged_df' from \code{create_lagged_df()}.
#' @param ... Not used.
#' @return A printed summary of the contents of the lagged_df object.
#' @export
summary.lagged_df <- function(object, ...) {

  data <- object
  outcome_names <- attributes(data)$outcome_names

  n_predictors_at_each_horizon <- unlist(lapply(data, function(x) {ncol(x) - length(outcome_names)}))
  horizons <- attributes(data)$horizons

  cat(paste0(
    "Number of forecast horizons: ", length(horizons), " \n",
    "Number of models to train: ", length(data), " \n",
    "Data start index: ", attributes(data)$data_start, " \n",
    "Data stop index: ", attributes(data)$data_stop, " \n",
    "Number of predictors in input data: ", length(attributes(data)$predictor_names), " \n",
    "Minimum number of predictors with lags: ", min(n_predictors_at_each_horizon, na.rm = TRUE), " at forecast horizons ", horizons[which.min(n_predictors_at_each_horizon)], " \n",
    "Maximum number of predictors with lags: ", max(n_predictors_at_each_horizon, na.rm = TRUE), " at forecast horizons ", horizons[which.max(n_predictors_at_each_horizon)], " \n"
  ))
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#' Plot datasets with lagged features
#'
#' Plot datasets with lagged features to view ther direct forecasting setup across horizons.
#'
#' @param x An object of class 'lagged_df' from \code{create_lagged_df()}.
#' @param ... Not used.
#' @return A single plot of class 'ggplot' if \code{lookback} was specified in \code{create_lagged_df()};
#' a list of plots, one per feature, of class 'ggplot' if \code{lookback_control} was specified.
#' @example /R/examples/example_plot_lagged_df.R
#' @export
plot.lagged_df <- function(x, ...) { # nocov start

  if (isTRUE(attributes(x)$skeleton)) {
    stop("Lagged feature plots are not available for skeleton objects.")
  }

  horizons <- attributes(x)$horizons

  # Within a given horizons, an indicator for different lag vectors for each feature.
  if (methods::is(attributes(x[[1]])$lookback, "list")) {
    lookback_per_predictor <- TRUE
  } else {
    lookback_per_predictor <- FALSE
  }

  groups <- attributes(x)$groups
  dynamic_features <- attributes(x)$dynamic_features
  static_features <- attributes(x)$static_features

  # Grouping features won't be plotted because they aren't lagged.
  predictor_names <- attributes(x)$predictor_names
  dont_plot_these_predictors <- which(predictor_names %in% c(groups, dynamic_features, static_features))
  predictor_names <- predictor_names[!predictor_names %in% c(groups, dynamic_features, static_features)]

  n_predictors <- length(predictor_names)

  lookback <- lapply(x, function(data) {attributes(data)$lookback})
  lookback_max <- max(unlist(lookback), na.rm = TRUE)

  if (lookback_per_predictor) {

    # seq_along horizons > predictor.
    data_horizon <- lapply(seq_along(lookback), function(i) {

      data_predictor <- lapply(seq_along(lookback[[i]]), function(j) {

        if (!j %in% dont_plot_these_predictors) {

          lookback_predictor <- lookback[[i]][[j]]

          if (all(!is.na(lookback_predictor), length(lookback_predictor) > 0)) {
            data_predictor <- expand.grid("horizons" = horizons[i], "time" = lookback_predictor)
            data_predictor$predictor_number <- j

          } else {  # Nothing to plot, no feature lags appropriate for this horizons.

            data_predictor <- expand.grid("horizons" = NA, "time" = NA)
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

    data_plot$predictor_number <- dplyr::dense_rank(data_plot$predictor_number)

  } else {

    data_plot <- purrr::map2(horizons, lookback, function(x, y) {expand.grid("horizons" = x, "time" = y)})
    data_plot <- dplyr::bind_rows(data_plot)
  }

  # Set the scale of the x-axis by multiplying time by -1, placing the features to the left of the
  # zero line--i.e., in the past.
  data_plot$time <- -1 * data_plot$time

  # Plot fill and legend contents.
  data_plot$feature <- "Feature"  # Feature is present in plot.

  # Make a grid for plotting the presence of lagged features, no lagged features, and forecast horizons.
  data_grid_past <- expand.grid("horizons" = min(horizons):max(horizons),
                                "time" = (-1 * lookback_max):-1)
  data_grid_future <- expand.grid("horizons" = min(horizons):max(horizons),
                                  "time" = 1:max(horizons))
  # For forecast horizons plot fill, remove the rows in the data that exceed the horizons.
  data_grid_future <- data_grid_future[with(data_grid_future, time <= horizons), ]
  data_grid <- dplyr::bind_rows(data_grid_past, data_grid_future)

  data_plot <- dplyr::left_join(data_grid, data_plot, by = c("horizons", "time"))

  # Plot fill and legend contents.
  data_plot$feature[is.na(data_plot$feature) & data_plot$time <= 0] <- "No feature"
  data_plot$feature[is.na(data_plot$feature)] <- "Forecast"

  # Filter to remove user-specified unused horizons from the plot grid.
  data_plot <- data_plot[data_plot$horizons %in% horizons, ]

  if (!lookback_per_predictor) {

    data_plot$horizons <- factor(data_plot$horizons, levels = sort(unique(as.numeric(data_plot$horizons))), ordered = TRUE)

    p <- ggplot(data_plot, aes(x = .data$time, y = .data$horizons, fill = .data$feature))
    p <- p + geom_tile(color = "gray85")
    p <- p + scale_fill_viridis_d()
    p <- p + geom_vline(xintercept = 0, size = 2)
    # With a continuous scale, the limits need to extend 1 unit beyond the min and max to capture the bookend geom_tile-s.
    p <- p + scale_x_continuous(limits = c(eval(parse(text = (lookback_max * -1) - 1)), max(data_plot$time, na.rm = TRUE) + 1))
    p <- p + theme_bw()
    p <- p + xlab("Time (0 is the current time)") + ylab("Forecast horizons") +
      labs(fill = NULL) + ggtitle("Map of Feature Lags for a Single Feature")
    return(p)

  } else {

    predictor_indices <- unique(data_plot$predictor_number)[!is.na(unique(data_plot$predictor_number))]

    lapply(predictor_indices, function(i) {

      data_plot_predictor_1 <- dplyr::filter(data_plot, .data$feature == "Feature" & .data$predictor_number == i)
      data_plot_predictor_1$feature_match <- paste0(data_plot_predictor_1$horizons, "-", data_plot_predictor_1$time)
      data_plot_predictor_2 <- data_grid_past
      data_plot_predictor_2$feature <- "No feature"
      data_plot_predictor_2$feature_match <- paste0(data_plot_predictor_2$horizons, "-", data_plot_predictor_2$time)
      data_plot_predictor_2 <- data_plot_predictor_2[!(data_plot_predictor_2$feature_match %in% data_plot_predictor_1$feature_match), ]
      data_plot_predictor_3 <- dplyr::filter(data_plot, .data$feature == "Forecast")
      data_plot_predictor <- dplyr::bind_rows(data_plot_predictor_1, data_plot_predictor_2, data_plot_predictor_3)

      data_plot_predictor <- data_plot_predictor[as.numeric(data_plot_predictor$horizons) %in% horizons, ]
      data_plot_predictor$horizons <- factor(data_plot_predictor$horizons, levels = sort(unique(as.numeric(data_plot_predictor$horizons))), ordered = TRUE)

      p <- ggplot(data_plot_predictor, aes(x = .data$time, y = .data$horizons, fill = .data$feature))
      p <- p + geom_tile(color = "gray85")
      p <- p + scale_fill_viridis_d()
      p <- p + geom_vline(xintercept = 0, size = 2)
      # With a continuous scale, the limits need to extend 1 unit beyond the min and max to capture the bookend geom_tile-s.
      p <- p + scale_x_continuous(limits = c(eval(parse(text = (lookback_max * -1) - 1)), max(data_plot$time, na.rm = TRUE) + 1))
      p <- p + theme_bw()
      p <- p + xlab("Time (0 is the current time)") + ylab("Forecast horizons") +
        labs(fill = NULL) + ggtitle(paste0("Map of Feature Lags: ", predictor_names[i]))
    })
  }
} # nocov end
