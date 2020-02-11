# nocov start
.onLoad <- function(...) {
  requireNamespace("dplyr")
}

#------------------------------------------------------------------------------
# Error functions in return_error.R.
# For all error function args: x = 'residual', y = 'actual', and z = 'prediction'.
forecastML_mae <- function(x, ...) {
  error_var <- base::mean(base::abs(x), na.rm = TRUE)
  error_var <- if (is.infinite(error_var) || is.nan(error_var)) {NA} else {error_var}
}

forecastML_mape <- function(x, y, ...) {
  error_var <- base::mean(base::abs(x) / base::abs(y), na.rm = TRUE) * 100
  error_var <- if (is.infinite(error_var) || is.nan(error_var)) {NA} else {error_var}
}

forecastML_mdape <- function(x, y, ...) {
  error_var <- stats::median(base::abs(x) / base::abs(y), na.rm = TRUE) * 100
  error_var <- if (is.infinite(error_var) || is.nan(error_var)) {NA} else {error_var}
}

forecastML_smape <- function(x, y, z, ...) {
  error_var <- base::mean(2 * base::abs(x) / (base::abs(y) + base::abs(z)), na.rm = TRUE) * 100
  error_var <- if (is.infinite(error_var) || is.nan(error_var)) {NA} else {error_var}
}
#------------------------------------------------------------------------------
# Function for ggplot2 faceting in train_model.R and return_error.R. The input is (1) a formula
# with any of 'horizon', 'model', 'group', or '.' and (2) a string identifying the grouping column,
# if any, in the input dataset. The output is a list containing (1) a formula where any
# groups in the modeling dataset are substituted for "~ group" and (2) facet names
# for use with ggplot2 geom objects
forecastML_facet_plot <- function(facet, groups) {

  facet_names <- all.vars(facet)

  if (isTRUE(any(facet_names %in% "."))) {
    facet_names <- facet_names[!facet_names %in% "."]
  }

  if (!is.null(facet) && !all(facet_names %in% c("horizon", "model", "group"))) {
    stop("One or more of the plot facets is not in 'horizon', 'model', or 'group'.")
  }

  # Adjust the formula, substituting the group name from the data into the 'facet' input formula.
  if ("group" %in% facet_names) {

    rhs <- try(labels(stats::terms(facet)))

    if (methods::is(rhs, "try-error")) {
      rhs <- "."
    }

    lhs <- facet_names[!facet_names %in% rhs]

    lhs[lhs %in% "group"] <- groups
    rhs[rhs %in% "group"] <- groups

    facet <- as.formula(paste(paste(lhs, collapse = "+"), "~", paste(rhs, collapse = "+")))

    facet_names[facet_names %in% "group"] <- groups
  }
  return(list(facet, facet_names))
}
#------------------------------------------------------------------------------
# Used in lagged_df.R in create_lagged_df() for removing feature-specific feature lag indices when
# they do not support direct forecasting to a given forecast horizon.
forecastML_filter_lookback_control <- function(lookback_control, horizons, groups, group_cols,
                                               static_feature_cols, dynamic_feature_cols) {

  if (length(horizons) == 1) {  # A single-horizon, non-nested lookback_control of feature lags.

    lookback_control <- lapply(seq_along(lookback_control), function(i) {

      if (is.null(groups)) {  # Single time series

        if (i %in% dynamic_feature_cols) {

          lookback_control[[i]] <- 0

        } else {

          lookback_control[[i]][lookback_control[[i]] >= horizons]
        }

      } else {  # Multiple time series

        if (i %in% c(group_cols, static_feature_cols, dynamic_feature_cols)) {

          lookback_control[[i]] <- 0

        } else {

          if (!is.null(lookback_control[[i]])) {

            lookback_control[[i]] <- lookback_control[[i]][lookback_control[[i]] >= horizons]
          }
        }
        lookback_control[[i]]
      }
    })  # Impossible lags for lagged features have been removed.

  } else if (length(horizons) > 1) {  # A multiple-horizon, nested lookback_control of feature lags.

    lookback_control <- lapply(seq_along(lookback_control), function(i) {
      lapply(seq_along(lookback_control[[i]]), function(j) {

        if (is.null(groups)) {  # Single time series.

          if (j %in% dynamic_feature_cols) {

            lookback_control[[i]][[j]] <- 0

          } else {

            lookback_control[[i]][[j]][lookback_control[[i]][[j]] >= horizons[i]]
          }

        } else {  # Multiple time series.

          if (j %in% c(group_cols, static_feature_cols, dynamic_feature_cols)) {

            lookback_control[[i]][[j]] <- 0

          } else {

            if (!is.null(lookback_control[[i]][[j]])) {

              lookback_control[[i]][[j]] <- lookback_control[[i]][[j]][lookback_control[[i]][[j]] >= horizons[i]]
            }
          }
          lookback_control[[i]][[j]]
        }
      })
    })  # Impossible lags for lagged features have been removed.
  }  # Impossible lags in 'lookback_control' have been removed.
}

# nocov end
