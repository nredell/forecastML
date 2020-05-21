#' Reconcile multiple temporal or hierarchical forecasts
#'
#' The purpose of forecast reconciliation is to produce a single coherent forecast
#' from multiple forecasts produced at (a) different time horizons (e.g., monthly and quarterly)
#' and/or (b) different levels of aggregation (e.g., classroom, school, and school district).
#' After forecast reconciliation, the bottom-level or most disaggregated forecast can simply
#' be summed up to produce all higher-level forecasts. At present, only temporal forecasts
#' are supported for a single time series.
#'
#' @param forecasts A list of 2 or more dataframes with forecasts. Each dataframe must have
#' a column named \code{index} of class \code{Date} or \code{POSIXt}. The column name of the
#' forecast is defined in the \code{outcome} argument and must be the same across dataframes.
#' Forecasts should be sorted from oldest (top) to newest (bottom). To produce correct forecast
#' reconciliations, all forecasts at the lowest/disaggregated level should be present for all horizons
#' contained in the forecasts with the higher levels of aggregation
#' (e.g., 24 monthly forecasts for 2 annual forecasts or 21 daily forecasts for 3 weekly forecasts).
#' @param frequency A character vector of \code{length(forecasts)} that identifies the date/time frequency
#' of the forecast. Each string should work with \code{base::seq.Date(..., by = "frequency")} or
#' \code{base::seq.POSIXt(..., by = "frequency")} e.g., '1 hour', '1 month', '7 days', '10 years' etc.
#' @param index A string giving the column name of the date column which should be common across \code{forecasts}.
#' @param outcome A string giving the column name of the forecast which should be common across \code{forecasts}.
#' @param method One of \code{c("temporal", "hierarchical", "cross_temporal")}.
#' @param keep_all Boolean. If \code{TRUE}, reconciled forecasts at all levels are returned. If \code{FALSE},
#' only the bottom-level or most disaggregated forecast is returned which can be manually aggregated as needed.
#' @param keep_non_reconciled Boolean. If \code{TRUE}, any additional higher frequency forecasts that fell outside of the
#' date range of the lowest frequency forecast are returned with their same forecast value from \code{forecasts}.
#' @return A \code{data.frame} of reconciled forecasts at either (a) the most disaggregated level or (b) all levels given in \code{forecasts}.
#'
#' @section Implementation:
#'
#'     \itemize{
#'       \item \code{method = 'temporal'}: Forecasts are reconciled across forecast horizons.
#'       }
#'
#'     \itemize{
#'       \item \bold{Combination type}: Structural scaling with weights from temporal hierarchies Athanasopoulos et al. (2017).
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
reconcile_forecasts <- function(forecasts, frequency, index, outcome, method, keep_all = TRUE, keep_non_reconciled = FALSE) {

  n_forecasts <- length(forecasts)

  if (n_forecasts < 2) {
    stop("Forecast reconciliation requires a list of 2 or more forecasts.")
  }

  if (n_forecasts != length(frequency)) {
    stop("The number of forecast horizon frequencies does not match the number of forecasts.")
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

  if (method != "temporal") {
    stop("reconcile_forecasts() currently only supports temporal forecast reconciliation.")
  }
  #----------------------------------------------------------------------------
  # Filter the datasets to keep only the 'outcome' and 'index' columns.
  forecasts <- lapply(forecasts, function(x) {

    x[, c(index, outcome), drop = FALSE]
  })
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
    methods::is(x$index, "POSIXt")
  }))

  # If there is a mix of dates and datetimes, coerce all indices into datetimes.
  if (length(unique(is_datetime)) > 1) {

    timezone <- unlist(lapply(which(is_datetime), function(i) {

      timezone <- attributes(forecasts[[i]]$index)$tzone

      if (timezone == "") {

        timezone <- "UTC"

      } else {

        timezone
      }
    }))

    timezone <- unique(timezone)[1]

    forecasts <- lapply(forecasts, function(x) {

      if (methods::is(x$index, "POSIXt")) {

        attr(x$index, "tzone") <- timezone  # as.POSIXct() won't change the timezone attribute for POSIXt classes.

      } else {

        x$index <- as.POSIXct(x$index)
        attr(x$index, "tzone") <- timezone
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
  # so to speak. These out-of-bounds forecasts will be returned but will not be reconciled.
  forecasts_out_of_bounds <- vector("list", n_forecasts)
  for (i in seq_along(n_forecasts)) {

    early_forecasts <- which(forecasts[[i]][, index, drop = TRUE] < min(index_lf_start))
    late_forecasts <- which(forecasts[[i]][, index, drop = TRUE] > max(index_lf_stop))

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

  n_forecasts_missing_at_end <- length(seq(max(forecasts[[i]][, index, drop = TRUE]), max(index_stop) - 1, by = frequency[1])) - 1

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

  weights <- matrix(0, ncol = length(sum_matrix_weights), nrow = length(sum_matrix_weights))
  diag(weights) <- sum_matrix_weights
  #----------------------------------------------------------------------------
  # Matrix multiplication. To-do: add formulas' references from paper.
  number_time_series <- nrow(sum_matrix)
  number_time_series_bottom <- ncol(sum_matrix)  # Same as highest_freq
  n_aggregations <- number_time_series - number_time_series_bottom

  matrix_upper_tri_l <- diag(n_aggregations)
  matrix_upper_tri_r <- sum_matrix[1:n_aggregations, , drop = FALSE]
  matrix_upper_tri_r[matrix_upper_tri_r == 1] <- -1
  matrix_upper_tri <- cbind(matrix_upper_tri_l, matrix_upper_tri_r)

  matrix_j_l <- matrix(0, ncol = n_aggregations, nrow = number_time_series_bottom)
  matrix_j_r <- diag(number_time_series_bottom)
  matrix_j <- cbind(matrix_j_l, matrix_j_r)

  matrix_rhs_lower <- matrix_upper_tri %*% forecast_matrix

  matrix_lhs_lower <- matrix_upper_tri %*% weights %*% t(matrix_upper_tri)

  linear_solution <- base::solve(matrix_lhs_lower, matrix_rhs_lower)

  linear_solution <- matrix_j %*% forecast_matrix - (matrix_j %*% weights %*% t(matrix_upper_tri) %*% linear_solution)

  data_forecasts_reconciled <- sum_matrix %*% linear_solution
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

  return(data_out)
}
