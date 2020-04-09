#' Remove the features from a lagged training dataset to reduce memory consumption
#'
#' \code{create_skeleton()} strips the feature data from a \code{create_lagged_df()} object
#' but keeps the outcome column(s), any grouping columns, and meta-data which allows the resulting
#' \code{lagged_df} to be used downstream in the \code{forecastML} pipeline. The main benefit is
#' that the custom modeling function passed in \code{train_model()} can read data directly from the
#' disk or a database when the dataset is too large to fit into memory.
#'
#' @param lagged_df An object of class 'lagged_df' from \code{create_lagged_df(..., type = 'train')}.
#' @return An S3 object of class 'lagged_df' or 'grouped_lagged_df': A list of data.frames with the
#' outcome column(s) and any grouping columns but with all other features removed.
#' A special attribute \code{skeleton = TRUE} is added.
#'
#' @section Methods and related functions:
#'
#' The output of \code{create_skeleton} can be passed into
#'
#' \itemize{
#'   \item \code{\link{create_windows}}
#' }
#' @export
create_skeleton <- function(lagged_df) {

  if (missing(lagged_df) || !methods::is(lagged_df, "lagged_df")) {
    stop("The 'lagged_df' argument takes an object of class 'lagged_df' as input. Run create_lagged_df() first.")
  }

  if (attributes(lagged_df)$type == "forecast") {
    stop("Skeleton objects are only used for model training, not forecasting.")
  }

  outcome_names <- attributes(lagged_df)$outcome_names
  groups <- attributes(lagged_df)$groups

  data <- lapply(lagged_df, function(x) {

    if (is.null(groups)) {

      data <- x[, c(outcome_names)]

    } else {

      data <- x[, c(outcome_names, groups)]
    }

    attr(data, "horizons") <- attributes(x)$horizons
    data
  })

  attributes(data) <- attributes(lagged_df)
  attr(data, "skeleton") <- TRUE

  return(data)
}
