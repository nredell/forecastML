#' Reconcile multiple temporal or hierarchical forecasts
#'
#' The purpose of forecast reconciliation is to produce a single coherent forecast
#' from multiple forecasts produced at (a) different time horizons (e.g., monthly and quarterly)
#' and/or (b) different levels of aggregation (e.g., classroom, school, and school district).
#' After forecast reconciliation, the bottom-level or most disaggregated forecast can simply
#' be summed up to produce all higher-level forecasts.
#'
#' @param forecasts A list of 2 or more dataframes with forecasts. Each dataframe must have
#' a date column named \code{index} of class \code{Date} or \code{POSIXt} and a forecast column named
#' \code{outcome} of class \code{numeric}.
#' Forecasts should be sorted from oldest (top) to newest (bottom).
#' @param frequency A character vector of \code{length(forecasts)} that identifies the date/time frequency
#' of the forecast. Each string should work with \code{base::seq.Date(..., by = "frequency")} or
#' \code{base::seq.POSIXt(..., by = "frequency")} e.g., '1 hour', '1 month', '7 days', '10 years' etc.
#' @param index A string giving the column name of the date column which should be common across \code{forecasts}.
#' @param outcome A string giving the column name of the forecast which should be common across \code{forecasts}.
#' @param method One of \code{c("temporal", "group")}. See the Implementation section for details.
#' @param keys Optional. For forecast reconciliation across groups, a \code{unique()} vector of column names listing all of the keys that
#' identify a distinct time series across the datasets in \code{forecasts}. If not specified, all columns that are not
#' in \code{index} or \code{outcome} are treated as grouping keys for each dataset in \code{forecasts}.
#' @param keep_all Boolean. For \code{method = "temporal"}. If \code{TRUE}, reconciled forecasts at all levels are returned.
#' If \code{FALSE}, only the bottom-level or most disaggregated forecast is returned which can be manually aggregated as needed.
#' @param keep_non_reconciled Boolean. For \code{method = "temporal"}. If \code{TRUE}, any additional higher frequency forecasts
#' that fell outside of the date range of the lowest frequency forecast are returned with their same forecast value from \code{forecasts}.
#' @return A \code{data.frame} of reconciled forecasts.
#'
#' @section Implementation:
#'
#'     \itemize{
#'       \item \bold{method = 'temporal'}: Forecasts are reconciled across forecast horizons.
#'         \itemize{
#'           \item Structural scaling with weights from temporal hierarchies from Athanasopoulos et al. (2017).
#'           \item To produce correct forecast reconciliations, all forecasts at the lowest/disaggregated
#'           level should be present for all horizons contained in the forecasts with the higher levels of aggregation
#'           (e.g., 24 monthly forecasts for 2 annual forecasts or 21 daily forecasts for 3 weekly forecasts).
#'           }
#'
#'       \item \bold{method = 'group'}: Forecasts are reconciled across groups independently at each forecast horizon.
#'         \itemize{
#'           \item Structural scaling from Hyndman et al. (2011).
#'           \item A key column is not needed for the forecast at the highest level of aggregation.
#'           \item Having input forecasts at each level of aggregation is not a requirement.
#'           For example, forecasts by nation, state, and city could be reconciled with only 2 input
#'           forecasts: 1 for nation (highest aggregation) and 1 for the combination of nation by state by
#'           city (lowest/no aggregation) without the 2 intermediate-level forecasts at the state and city levels.
#'           }
#'       }
#'
#' @section References:
#'
#' Athanasopoulos, G., Hyndman, R. J., Kourentzes, N., & Petropoulos, F. (2017).
#' Forecasting with temporal hierarchies. European Journal of Operational Research, 262(1), 60-74.
#' \url{https://robjhyndman.com/papers/temporalhierarchies.pdf}
#'
#' Hyndman, R. J., Ahmed, R. A., Athanasopoulos, G., & Shang, H. L. (2011).
#' Optimal combination forecasts for hierarchical time series. Computational statistics & data analysis, 55(9), 2579-2589.
#' \url{http://robjhyndman.com/papers/hierarchical}
#'
#' @example /R/examples/example_reconcile_forecasts.R
#' @export
reconcile_forecasts <- function(forecasts, frequency, index, outcome, keys = NULL, method, keep_all = TRUE, keep_non_reconciled = FALSE) {

  n_forecasts <- length(forecasts)
  forecast_names <- names(forecasts)

  if (n_forecasts < 2) {
    stop("Forecast reconciliation requires a list of 2 or more forecasts.")
  }

  if (missing(method)) {
    stop("Select one of 'temporal' or 'group'.")
  }

  if (method == "temporal") {

    if (n_forecasts != length(frequency)) {
      stop("The number of forecast horizon frequencies does not match the number of forecasts.")
    }

  } else if (method == "group") {

    if (length(frequency) != 1) {
      stop("Grouped forecast reconciliation only works with the same forecast horizon frequency across all forecasts.")
    }
  }

  if (length(index) != 1) {
    stop("'index' should be a single column name identifying the dates in each dataset.")
  }

  if (length(outcome) != 1) {
    stop("'outcome' should be a single column name identifying the forecasts in each dataset.")
  }

  outcome_in_dataset <- sapply(forecasts, function(x) {
    outcome %in% names(x)
  })

  if (!all(outcome_in_dataset)) {
    stop(paste0("'", outcome, "' is not a column name in all input forecasts; rename the forecasts to have a common name."))
  }

  index_in_dataset <- sapply(forecasts, function(x) {
    index %in% names(x)
  })

  if (!all(index_in_dataset)) {
    stop("'", index, "' is not a column name in all input forecasts; rename the forecast date column to have a common name.")
  }
  #----------------------------------------------------------------------------
  # Filter the datasets to keep only the 'outcome', 'index', and 'key' columns.
  if (is.null(keys)) {

    keys <- unique(unlist(lapply(forecasts, function(x) {names(x)[!names(x) %in% c(index, outcome)]})))

    if (length(keys) == 0 && method == "group") {
      stop("Grouped forecast reconciliation requires that at least 1 input forecast has a key column identifying unique time series.")
    }
  }

  if (method == "temporal") {

    forecasts <- lapply(forecasts, function(forecast) {

      forecast[, c(index, outcome)]
    })

  } else if (method == "group") {

    forecasts <- lapply(forecasts, function(forecast) {

      keys_at_this_level <- keys[keys %in% names(forecast)]

      if (length(keys_at_this_level) > 0) {

        forecast[, c(index, keys_at_this_level, outcome)]

      } else {

        forecast[, c(index, outcome)]
      }
    })
  }
  #----------------------------------------------------------------------------
  #----------------------------------------------------------------------------
  # To-do: Remove nested if statements for extensibility.
  if (method == "temporal") {

    #----------------------------------------------------------------------------
    # Find the frequency of each forecast. The purpose is to re-order the forecasts
    # from the highest to lowest forecast frequency (e.g., monthly > quarterly > annually).
    # The end result doesn't need to be exact with respect to phenomena like leap years
    # because this is simply for ordering the input forecasts.
    day_diff <- sapply(seq_along(forecasts), function(i) {

      # Normalize all date and date-time differences to days. This will always return day
      # differences for "Date" classes; however, "POSIXt" classes may not respect the
      # "units" argument and are coerced to numeric days.
      date_diff <- diff(seq(min(forecasts[[i]][, index, drop = TRUE]), by = frequency[i], length.out = 2), units = "days")

      units <- attributes(date_diff)$units

      date_diff <- as.numeric(date_diff)

      if (units == "hours") {

        date_diff <- date_diff / 24

      } else if (units == "mins") {

        date_diff <- date_diff / 3600

      } else if (units == "secs") {

        date_diff <- date_diff / 86400
      }

      date_diff
    })

    # Ordered from highest to lowest forecast horizon frequency (e.g., monthly > annually).
    forecast_order <- order(day_diff)

    forecasts <- forecasts[forecast_order]
    frequency <- frequency[forecast_order]
    #----------------------------------------------------------------------------
    # Get information to work with both dates and datetimes.
    is_datetime <- unlist(lapply(forecasts, function(x) {
      methods::is(x[, index, drop = TRUE], "POSIXt")
    }))

    # If there is a mix of dates and datetimes, coerce all indices into datetimes.
    if (length(unique(is_datetime)) > 1) {

      timezone <- unlist(lapply(which(is_datetime), function(i) {

        timezone <- attributes(forecasts[[i]][, index, drop = TRUE])$tzone

        if (timezone == "") {

          timezone <- "UTC"

        } else {

          timezone
        }
      }))

      timezone <- unique(timezone)[1]

      forecasts <- lapply(forecasts, function(x) {

        if (methods::is(x[, index, drop = TRUE], "POSIXt")) {

          attr(x[, index], "tzone") <- timezone  # as.POSIXct() won't change the timezone attribute for POSIXt classes.

        } else {

          x[, index] <- as.POSIXct(x[, index, drop = TRUE])
          attr(x[, index], "tzone") <- timezone
        }

        x
      })
    }
    #----------------------------------------------------------------------------
    # Start and stop dates for the forecast periods for the lowest forecast frequency.
    index_lf <- forecasts[[n_forecasts]][, index, drop = TRUE]
    index_lf_start <- index_lf

    index_lf_stop <- lapply(seq_along(index_lf_start), function(i) {

      # The stop date/time is the next date/time on the calendar at this frequency.
      seq(index_lf_start[i], by = frequency[n_forecasts], length.out = 2)[-1]
    })

    index_lf_stop <- do.call("c", index_lf_stop)
    #----------------------------------------------------------------------------
    # Remove and store any higher frequency forecasts that don't fall within the start
    # and stop dates of the lowest frequency forecast which are the reconciliation bookends
    # so to speak. These out-of-bounds forecasts can be returned but will not be reconciled.
    forecasts_out_of_bounds <- vector("list", n_forecasts)
    for (i in seq_len(n_forecasts)) {

      early_forecasts <- which(forecasts[[i]][, index, drop = TRUE] < min(index_lf_start))
      late_forecasts <- which(forecasts[[i]][, index, drop = TRUE] >= max(index_lf_stop))

      forecasts_out_of_bounds[[i]] <- forecasts[[i]][c(early_forecasts, late_forecasts), ]  # Keep the unreconcilable forecasts.

      if (length(early_forecasts) || length(late_forecasts)) {

        forecasts[[i]] <- forecasts[[i]][-c(early_forecasts, late_forecasts), , drop = FALSE]  # Remove the unreconcilable forecasts.
      }
    }
    #----------------------------------------------------------------------------
    # Start and stop dates for the forecast periods for the highest forecast frequency.
    # If the highest frequency forecast is a datetime and the lowest frequency forecast
    # is a date, set both to datetimes for easier indexing.
    if (length(unique(is_datetime)) > 1) {

      index_start <- min(forecasts[[1]][, index, drop = TRUE])
      index_stop <- max(index_lf_stop)
      attr(index_start, "tzone") <- timezone
      attr(index_stop, "tzone") <- timezone

    } else {

      index_start <- min(forecasts[[1]][, index, drop = TRUE])
      index_stop <- max(index_lf_stop)
    }

    index_hf <- seq(index_start, index_stop, by = frequency[1])

    # Throw an error if the highest frequency forecast has too few forecasts based on the
    # start and stop dates of the lowest frequency forecast--our bookends.
    # To-do: Expand to check intermediate levels of the hierarchy.
    n_forecasts_missing_at_start <- length(seq(index_lf_start[1], index_hf[1], by = frequency[1])) - 1

    n_forecasts_missing_at_end <- length(seq(max(forecasts[[1]][, index, drop = TRUE]), max(index_stop) - 1, by = frequency[1])) - 1

    if (any(c(n_forecasts_missing_at_start > 0, n_forecasts_missing_at_end > 0))) {
      stop(paste0("For the '", frequency[1], "' forecast, ", n_forecasts_missing_at_start, " forecasts were missing at the start of the forecast date range and ", n_forecasts_missing_at_end,  " forecasts were missing at the end of the forecast date range.
    The forecast date range--given by the lowest frequency forecast--is [", min(index_lf_start), ", ", max(index_lf_stop), ")."))
    }
    #----------------------------------------------------------------------------
    # The anticipated forecast dates given the forecast start and stop dates for
    # the lowest frequency forecast and the frequencies of each forecast in the hierarchy.
    # The return value is a nested list of indices that specify the number of 1s that
    # should be added to each row of the summation matrix.
    forecast_dates <- lapply(1:n_forecasts, function(i) {

      index <- forecasts[[i]][, index, drop = TRUE]
      index_start <- index

      index_stop <- lapply(seq_along(index_start), function(j) {

        seq(index_start[j], by = frequency[i], length.out = 2)[-1]
      })

      index_stop <- do.call("c", index_stop)

      if (i == 1) {  # Highest frequency forecast.

        date_indices <- lapply(seq_along(index_lf_start), function(j) {

          index <- dplyr::intersect(which(index_hf >= index_lf_start[j]), which(index_hf < index_lf_stop[j]))
          length(index)
        })

      } else {  # Forecasts with lower frequencies.

        date_indices <- lapply(seq_along(index_start), function(j) {

          index <- dplyr::intersect(which(index_hf >= index_start[j]), which(index_hf < index_stop[j]))
          length(index)
        })
      }

      date_indices <- unlist(date_indices)
    })

    # Number of columns in summation matrix. This is equal to the number of
    # time steps for the highest frequency forecast horizon. This format supports
    # asymmetric hierarchies like 29 and 30 days in a daily/monthly forecast
    # reconciliation.
    highest_freq <- sum(forecast_dates[[1]])
    #----------------------------------------------------------------------------
    # Stacked forecasts.
    # Rows: From lowest (top) to highest (bottom) frequency; from oldest (top) to newest (bottom) dates.
    # Columns: The number of repeated forecast cycles based on the number of lowest frequency forecasts.
    forecast_matrix <- as.data.frame(dplyr::bind_rows(rev(forecasts)))
    forecast_matrix <- matrix(forecast_matrix[, outcome], ncol = 1)
    #----------------------------------------------------------------------------
    # Summation matrix.
    agg_matrices <- lapply(seq_along(forecast_dates)[-n_forecasts], function(i) {

      agg_matrix <- matrix(0, nrow = length(forecast_dates[[i]]), ncol = highest_freq)

      col_indices_stop <- cumsum(forecast_dates[[i]])
      col_indices_start <- c(1, col_indices_stop[-length(col_indices_stop)] + 1)

      col_indices <- purrr::map2(col_indices_start, col_indices_stop,  `:`)

      for (i in seq_along(forecast_dates[[i]])) {

        agg_matrix[i, col_indices[[i]]] <- 1
      }

      agg_matrix
    })

    agg_matrix <- do.call(rbind, agg_matrices)

    bottom_matrix <- diag(highest_freq)

    sum_matrix <- rbind(agg_matrix, bottom_matrix)
    #----------------------------------------------------------------------------
    # Structural weight matrix.
    sum_matrix_weights <- c(unlist(forecast_dates[-n_forecasts]), rep(1, highest_freq))

    weight_matrix <- matrix(0, ncol = length(sum_matrix_weights), nrow = length(sum_matrix_weights))
    diag(weight_matrix) <- sum_matrix_weights
    #----------------------------------------------------------------------------
    # Weighted least squares regression for reconciliation. The output is in the same
    # format as the input 'forecast_matrix'.
    data_forecasts_reconciled <- forecastML_reconcile(forecast_matrix, weight_matrix, sum_matrix)
    #----------------------------------------------------------------------------
    # Format forecast outputs to match the forecast inputs.
    n_horizons <- rev(sapply(forecasts, nrow))
    row_indices <- lapply(cumsum(n_horizons), function(x) {seq(1, x, by = 1)})

    row_indices[seq_along(row_indices)[-1]] <- lapply(seq_along(row_indices)[-1], function(i) {

      row_indices[[i]][!(row_indices[[i]] %in% row_indices[[i - 1]])]
    })

    row_indices <- rev(row_indices)

    data_out <- lapply(1:n_forecasts, function(i) {

      data_out <- data.frame("index" = forecasts[[i]][, index, drop = TRUE],
                             "forecast" = data_forecasts_reconciled[row_indices[[i]], ])

      names(data_out) <- c(index, outcome)

      if (keep_non_reconciled) {

        data_out <- dplyr::bind_rows(data_out, forecasts_out_of_bounds[[i]])

        data_out <- as.data.frame(data_out[order(data_out[, index, drop = TRUE]), ], row.names = 1:nrow(data_out))
      }

      data_out
    })

    names(data_out) <- frequency

    if (!keep_all) {  # Keep the bottom-level forecasts.

      data_out <- data_out[[1]]
    }

    # End method == "temporal".

    #--------------------------------------------------------------------------
    #--------------------------------------------------------------------------

  } else if (method == "group") {

    frequency <- unlist(frequency)

    #------------------------------------------------------------------------------
    # Sort the input forecasts from the highest level of aggregation (level 0) to the
    # lowest level of aggregation (level N).
    n_keys_in_level <- lapply(forecasts, function(x) {length(keys[keys %in% names(x)])})

    n_time_series_in_level <- lapply(forecasts, function(x) {

      keys <- keys[keys %in% names(x)]

      if (length(keys) > 0) {

        nrow(dplyr::distinct(x, !!!rlang::syms(keys)))

      } else {

        1
      }
    })

    forecast_order <- order(unlist(n_keys_in_level), unlist(n_time_series_in_level))

    forecasts <- forecasts[forecast_order]

    forecast_keys <- lapply(forecasts, function(x) {keys[keys %in% names(x)]})
    #------------------------------------------------------------------------------
    # Auto-fill 0s for any time series that may be missing a forecast at a given forecast
    # horizon/date. This flexibility supports different forecast horizons for different input
    # time series--e.g., a discontinued product that only receives a short-term forecast but
    # still contributes to the total/aggregated forecast during the relevant forecast horizons.
    # It also standardizes the input and allows the same summation matrix to be used for reconciliation
    # at each forecast horizon. To-do: Make this an optional and support a unique summation matrix
    # at each forecast horizon.
    forecast_date_range <- lapply(forecasts, function(x) {

      c(min(x[[index]]), max(x[[index]]))
    })

    forecast_date_range <- do.call("c", forecast_date_range)

    forecast_start <- min(forecast_date_range)  # Across all forecasts.
    forecast_stop <- max(forecast_date_range)  # Across all forecasts.

    date_template <- data.frame(seq(forecast_start, forecast_stop, by = frequency))
    names(date_template) <- index

    forecasts <- lapply(seq_along(forecasts), function(i) {

      if (length(forecast_keys[[i]]) > 0) {

        date_key_template <- append(list(date_template[[index]]), lapply(forecast_keys[[i]], function(key) {unique(forecasts[[i]][[key]])}))

        date_key_template <- do.call("expand.grid", date_key_template)

        names(date_key_template) <- c(index, forecast_keys[[i]])

        forecasts[[i]] <- dplyr::left_join(date_key_template, forecasts[[i]], by = c(index, forecast_keys[[i]]))

        forecasts[[i]][[outcome]][is.na(forecasts[[i]][[outcome]])] <- 0
      }

      forecasts[[i]]
    })
    #------------------------------------------------------------------------------
    # Look up the forecast stack from the lowest to the highest aggregation and sort
    # the time series for the summation matrix.
    forecasts <- lapply(seq_along(forecasts), function(i) {

      if (i == 1) {  # Highest level or most aggregated.

        forecasts[[i]] <- dplyr::arrange(forecasts[[i]], !!!rlang::syms(forecast_keys[[i]]), !!rlang::sym(index))

      } else {  # Sort forecasts by keys at the highest level >> lower levels >> keys at the given level.

        keys <- unique(c(unlist(forecast_keys[1:(i - 1)]), forecast_keys[[i]]))

        forecast_keys[[i]] <- forecast_keys[[i]][forecast_keys[[i]] %in% keys]

        forecasts[[i]] <- dplyr::arrange(forecasts[[i]], !!!rlang::syms(forecast_keys[[i]]), !!rlang::sym(index))
      }
    })
    #------------------------------------------------------------------------------
    # Create the summation matrix.
    keys_at_lowest_level <- dplyr::distinct(forecasts[[n_forecasts]], !!!rlang::syms(forecast_keys[[n_forecasts]]))

    n_keys_at_lowest_level <- nrow(keys_at_lowest_level)

    sum_matrix <- lapply(seq_along(forecasts), function(i) {

      if (i == n_forecasts) {  # Lowest level.

        row_matrix <- diag(nrow(keys_at_lowest_level))  # Function return.

      } else if (any(forecast_keys[[i]] %in% forecast_keys[[n_forecasts]])) {

        # If a higher level of aggregation has the same key(s) as the lowest level,
        # produce a row matrix for each key.

        keys <- forecast_keys[[i]][forecast_keys[[i]] %in% forecast_keys[[n_forecasts]]]

        row_matrices <- lapply(keys, function(key) {

          # A vector giving the number of forecasts at the lowest level that feed into
          # this level of aggregation. Each key adds a new row to the sum matrix for each
          # unique value of the given key.
          key <- table(keys_at_lowest_level[[key]])

          col_stop <- cumsum(key)  # Row matrix stop indices for the 1s.
          col_start <- c(1, col_stop[-length(col_stop)] + 1)  # Row matrix start indices for the 1s.

          # For each unique value of this key--which represents a unique time series--, create 1
          # row for the sum matrix.
          row_matrix <- lapply(seq_along(key), function(i) {

            row_matrix <- matrix(rep(0, n_keys_at_lowest_level), nrow = 1)  # Initialize row with 0s.
            row_matrix[1, col_start[i]:col_stop[i]] <- 1  # Add 1s by position.
            row_matrix
          })

          row_matrix <- do.call("rbind", row_matrix)
        })

        row_matrix <- do.call("rbind", row_matrices)  # Function return.

      } else {  # The total forecast at the highest level of the hierarchy, if given, is a row of 1s.

        row_matrix <- matrix(rep(1, n_keys_at_lowest_level), nrow = 1)  # Function return.
      }
    })

    sum_matrix <- do.call("rbind", sum_matrix)
    #------------------------------------------------------------------------------
    # 1 1-column matrix of forecasts for each forecast horizon/date. The forecasts
    # are reconciled independently at each horizon.
    forecast_dates <- lapply(forecasts, function(x) {

      unique(x[[index]])
    })

    forecast_dates <- unique(do.call("c", forecast_dates))

    forecast_matrices <- lapply(seq_along(forecast_dates), function(i) {

      data_out <- lapply(seq_along(forecasts), function(j) {

        forecasts[[j]] %>%
          dplyr::filter(!!rlang::sym(index) %in% !!forecast_dates[i]) %>%
          dplyr::select(!!outcome)
      })

      data_out <- as.matrix(dplyr::bind_rows(data_out), ncol = 1)
    })

    names(forecast_matrices) <- forecast_dates
    #------------------------------------------------------------------------------
    # Create the weight matrix.
    sum_matrix_weights <- apply(sum_matrix, 1, sum)

    weight_matrix <- matrix(0, ncol = length(sum_matrix_weights), nrow = length(sum_matrix_weights))

    diag(weight_matrix) <- sum_matrix_weights
    #----------------------------------------------------------------------------
    # Reconcile forecasts with WLS regression.
    data_forecasts_reconciled <- lapply(seq_along(forecast_matrices), function(i) {

      forecastML_reconcile(forecast_matrices[[i]], weight_matrix, sum_matrix)
    })
    #----------------------------------------------------------------------------
    # Replace the input forecasts with the reconciled forecasts. The first step is
    # to replace the outcome values which are still grouped by forecast horizon/date.
    data_forecasts_reconciled <- lapply(seq_along(forecast_dates), function(i) {

      #----------------------------------------------------------------------------
      # Get the row indices corresponding to the various forecast aggregation levels in
      # the return 1-column matrix of forecasts from forecastML_reconcile().
      n_forecasts_at_agg_level <- lapply(seq_along(forecasts), function(j) {

        nrow(
          forecasts[[j]] %>%
            dplyr::filter(!!rlang::sym(index) %in% !!forecast_dates[i])
        )
      })

      n_forecasts_at_agg_level <- unlist(n_forecasts_at_agg_level)

      row_stop <- cumsum(n_forecasts_at_agg_level)
      row_start <- c(1, row_stop[-length(row_stop)] + 1)
      #----------------------------------------------------------------------------
      # For the forecasts reconciled across groups for the date indexed by 'i', insert the
      # reconciled forecast into the outcome column at date[i] in the input forecasts.
      data_out <- lapply(seq_along(forecasts), function(k) {

        data_out <- forecasts[[k]] %>%
          dplyr::filter(!!rlang::sym(index) %in% !!forecast_dates[i])

        data_out[, outcome] <- data_forecasts_reconciled[[i]][row_start[k]:row_stop[k]]

        data_out
      })

      keys <- unlist(lapply(forecast_keys, paste, collapse = "_"))
      keys <- sapply(keys, function(key) {if (key == "") {"no_key"} else {key}})

      names(data_out) <- keys

      data_out
    })

    data_forecasts_reconciled <- unlist(data_forecasts_reconciled, recursive = FALSE)
    #------------------------------------------------------------------------------
    # The second step is to combine the date- and key-based lists into the input
    # forecast format.
    list_keys <- unique(names(data_forecasts_reconciled))

    data_out <- lapply(seq_along(list_keys), function(i) {

      data_out <- dplyr::bind_rows(data_forecasts_reconciled[which(names(data_forecasts_reconciled) %in% list_keys[i])])

      if (length(forecast_keys[[i]]) > 0) {

        data_out <- dplyr::arrange(data_out, !!!rlang::syms(forecast_keys[[i]]), !!rlang::sym(index))

      } else {

        data_out <- dplyr::arrange(data_out, !!rlang::sym(index))
      }
    })

    if (!is.null(forecast_names)) {

      names(data_out) <- forecast_names[forecast_order]
    }
  }  # End method == "group".
  #----------------------------------------------------------------------------
  #----------------------------------------------------------------------------

  return(data_out)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Not exported: Weighted least squares regression for reconciliation. The output is
# in the same format as the input 'forecast_matrix'.
forecastML_reconcile <- function(forecast_matrix, weight_matrix, sum_matrix) {

  n_time_series <- nrow(sum_matrix)
  n_time_series_bottom <- ncol(sum_matrix)
  n_aggregations <- n_time_series - n_time_series_bottom

  matrix_upper_tri_l <- diag(n_aggregations)
  matrix_upper_tri_r <- sum_matrix[1:n_aggregations, , drop = FALSE]
  matrix_upper_tri_r[matrix_upper_tri_r == 1] <- -1
  matrix_upper_tri <- cbind(matrix_upper_tri_l, matrix_upper_tri_r)

  matrix_j_l <- matrix(0, ncol = n_aggregations, nrow = n_time_series_bottom)
  matrix_j_r <- diag(n_time_series_bottom)
  matrix_j <- cbind(matrix_j_l, matrix_j_r)

  matrix_rhs_lower <- matrix_upper_tri %*% forecast_matrix

  matrix_lhs_lower <- matrix_upper_tri %*% weight_matrix %*% t(matrix_upper_tri)

  linear_solution <- base::solve(matrix_lhs_lower, matrix_rhs_lower)

  linear_solution <- matrix_j %*% forecast_matrix - (matrix_j %*% weight_matrix %*% t(matrix_upper_tri) %*% linear_solution)

  forecasts_reconciled <- sum_matrix %*% linear_solution

  return(forecasts_reconciled)
}
