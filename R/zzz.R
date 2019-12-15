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
# nocov end
