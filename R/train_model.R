#' Train a model across horizons and validation datasets
#'
#' Train a user-defined forecast model for each horizon, h, and across the validation
#' datasets, d. A total of h * d models are trained--more if the user-defined modeling function
#' performs any inner-loop cross-validation. These models can, however, be trained in parallel
#' with the \code{future} package.
#'
#' @param lagged_df An object of class 'lagged_df' from \code{\link{create_lagged_df}}.
#' @param windows An object of class 'windows' from \code{\link{create_windows}}.
#' @param model_function A user-defined wrapper function for model training that takes 2
#' positional arguments--(1) a data.frame made with \code{create_lagged_df()} and
#' (2) the column index of the modeled outcome--and returns a model object which is used
#' as input in the user-defined prediction function (see example).
#' @param model_name A name for the model. Required.
#' @param use_future Boolean. If \code{TRUE}, the \code{future} package is used for training models in parallel.
#' The model will train in parallel across either (1) model forecast horizons or (b) validation windows,
#' whichever is longer (i.e., \code{length(create_lagged_df())} or \code{nrow(create_windows())}). The user
#' should run \code{future::plan(future::multiprocess)} or similar prior to this function to train these models
#' in parallel.
#' @return An S3 object of class 'forecast_model': A nested list of trained models. Models can be accessed with
#' \code{my_trained_model$horizon_h$window_w$model} where 'h' gives the forecast horizon and 'w' gives
#' the validation dataset window number from \code{create_windows}.
#'
#' @section Methods and related functions:
#'
#' The output of of \code{train_model} can be passed into
#'
#' \itemize{
#'   \item \code{\link{return_error}}
#'   \item \code{\link{return_hyper}}
#' }
#'
#' and has the following generic S3 methods
#'
#' \itemize{
#'   \item \code{\link[=predict.forecast_model]{predict}}
#'   \item \code{\link[=plot.training_results]{plot}} (from \code{predict.forecast_model(data = create_lagged_df(..., type = "train"))})
#'   \item \code{\link[=plot.forecast_results]{plot}} (from \code{predict.forecast_model(data = create_lagged_df(..., type = "forecast"))})
#' }
#' @example /R/examples/example_train_model.R
#' @export
train_model <- function(lagged_df, windows, model_function, model_name, use_future = FALSE) {

  data <- lagged_df

  if(!methods::is(data, "lagged_df")) {
    stop("The 'data' argument takes an object of class 'lagged_df' as input. Run create_lagged_df() first.")
  }

  if(!methods::is(windows, "windows")) {
    stop("The 'windows' argument takes an object of class 'windows' as input. Run create_windows() first.")
  }

  if (is.null(model_name)) {
    stop("Enter a model name for the 'model_name' argument.")
  }

  outcome_cols <- attributes(data)$outcome_cols
  outcome_names <- attributes(data)$outcome_names
  row_indices <- attributes(data)$row_indices
  date_indices <- attributes(data)$date_indices
  frequency <- attributes(data)$frequency
  horizons <- attributes(data)$horizons
  data_stop <- attributes(data)$data_stop
  n_outcomes <- length(outcome_cols)
  groups <- attributes(data)$groups
  valid_indices_date <- NULL

  window_indices <- windows

  #----------------------------------------------------------------------------
  # The default future behavior is to parallelize the model training over the longer dimension: (a) number of
  # forecast horizons or (b) number of validation windows. This is due to a current limitation
  # in the future package on changing object size limitations for nested futures where
  # "options(globals.maxSize.default = Inf)" isn't recognized.
  if (isTRUE(use_future)) {

    if (length(horizons) > nrow(windows)) {

      lapply_across_horizons <- future.apply::future_lapply
      lapply_across_val_windows <- base::lapply

    } else {

      lapply_across_horizons <- base::lapply
      lapply_across_val_windows <- future.apply::future_lapply
    }

  } else {

    lapply_across_horizons <- base::lapply
    lapply_across_val_windows <- base::lapply
  }
  #----------------------------------------------------------------------------

  # Seq along model forecast horizon > cross-validation windows.
  data_out <- lapply_across_horizons(data, function(data) {  # model forecast horizon.

    model_plus_valid_data <- lapply_across_val_windows(1:nrow(window_indices), function(i) {  # validation windows within model forecast horizon.

      window_length <- window_indices[i, "window_length"]

      if (is.null(date_indices)) {

        valid_indices <- window_indices[i, "start"]:window_indices[i, "stop"]

      } else {

        valid_indices <- which(date_indices >= window_indices[i, "start"] & date_indices <= window_indices[i, "stop"])
        valid_indices_date <- date_indices[date_indices >= window_indices[i, "start"] & date_indices <= window_indices[i, "stop"]]
      }

      # A window length of 0 removes the nested cross-validation and trains on all input data in lagged_df.
      if (window_length == 0) {

        data_train <- data

      } else {

        data_train <- data[!row_indices %in% valid_indices, , drop = FALSE]
      }

      # Model training.
      model <- model_function(data_train, outcome_cols)

      model_plus_valid_data  <- list("model" = model, "window" = window_length,
                                     "valid_indices" = valid_indices, "date_indices" = valid_indices_date)

      model_plus_valid_data
    })  # End model training across nested cross-validation windows for the horizon in "data".

    names(model_plus_valid_data) <- paste0("window_", 1:nrow(window_indices))
    attr(model_plus_valid_data, "horizon") <- attributes(data)$horizon
    model_plus_valid_data
  })  # End training across horizons.

  attr(data_out, "model_name") <- model_name
  attr(data_out, "outcome_cols") <- outcome_cols
  attr(data_out, "outcome_names") <- outcome_names
  attr(data_out, "row_indices") <- row_indices
  attr(data_out, "date_indices") <- date_indices
  attr(data_out, "frequency") <- frequency
  attr(data_out, "data_stop") <- data_stop
  attr(data_out, "horizons") <- horizons
  attr(data_out, "groups") <- groups

  class(data_out) <- c("forecast_model", class(data_out))

  return(data_out)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#' Predict on validation datasets or forecast
#'
#' Predict with a 'forecast_model' object from \code{train_model()}. If \code{data = create_lagged_df(..., type = "train")},
#' predictions are returned for the outer-loop nested cross-validation datasets.
#' If \code{data} is an object of class 'lagged_df' from \code{create_lagged_df(..., type = "forecast")},
#' predictions are returned for the horizons specified in \code{create_lagged_df()}.
#'
#' @param ... One or more trained models from \code{train_model()}.
#' @param prediction_function A list of user-defined prediction functions with length equal to
#' the number of models supplied in \code{...}. The prediction functions
#' take 2 required positional arguments--(1) a 'forecast_model' object from \code{train_model()} and (2) a
#' data.frame of model features from \code{create_lagged_df()}--and return a 1- or 3-column data.frame
#' of model predictions. If the prediction function returns a 1-column data.frame, point forecasts are assumed.
#' If the prediction function returns a 3-column data.frame, lower and upper forecast bounds are assumed (the
#' order of the 3 columns does not matter). See the example below for details.
#' @param data If \code{data} is a training dataset from \code{create_lagged_df(..., type = "train")}, validation dataset
#' predictions are returned; else, if \code{data} is a forecasting dataset from \code{create_lagged_df(..., type = "forecast")},
#' forecasts from horizons 1:h are returned.
#' @return If \code{data = create_lagged_df(..., type = "forecast")}, an S3 object of class 'training_results'. If
#' \code{data = create_lagged_df(..., type = "forecast")}, an S3 object of class 'forecast_results'.
#'
#'   \strong{Columns in returned 'training_results' data.frame:}
#'     \itemize{
#'       \item \code{model}: User-supplied model name in \code{train_model()}.
#'       \item \code{horizon}: Forecast horizons, 1:h, measured in dataset rows.
#'       \item \code{window_length}: Validation window length measured in dataset rows.
#'       \item \code{valid_indices}: Validation dataset row names from \code{attributes(create_lagged_df())$row_indices}.
#'       \item \code{date_indices}: If given, validation dataset date indices from \code{attributes(create_lagged_df())$date_indices}.
#'       \item \code{"groups"}: If given, the user_supplied groups in \code{create_lagged_df()}.
#'       \item \code{"outcome_name"}: The target being forecasted.
#'       \item \code{"outcome_name"_pred}: The model predictions.
#'       \item \code{"outcome_name"_pred_lower}: If given, the lower prediction bounds returned by the user-supplied prediction function.
#'       \item \code{"outcome_name"_pred_upper}: If given, the upper prediction bounds returned by the user-supplied prediction function.
#'    }
#'
#'    \strong{Columns in returned 'forecast_results' data.frame:}
#'     \itemize{
#'       \item \code{model}: User-supplied model name in \code{train_model()}.
#'       \item \code{model_forecast_horizon}: The direct-forecasting time horizon that the model was trained on.
#'       \item \code{horizon}: Forecast horizons, 1:h, measured in dataset rows.
#'       \item \code{window_length}: Validation window length measured in dataset rows.
#'       \item \code{window_number}: Validation dataset number.
#'       \item \code{forecast_period}: The forecast period in row indices or dates. The forecast period starts at either \code{attributes(create_lagged_df())$data_stop + 1} for row indices or \code{attributes(create_lagged_df())$data_stop + 1 * frequency} for date indices.
#'       \item \code{"groups"}: If given, the user_supplied groups in \code{create_lagged_df()}.
#'       \item \code{"outcome_name"}: The target being forecasted.
#'       \item \code{"outcome_name"_pred}: The model forecasts.
#'       \item \code{"outcome_name"_pred_lower}: If given, the lower forecast bounds returned by the user-supplied prediction function.
#'       \item \code{"outcome_name"_pred_upper}: If given, the upper forecast bounds returned by
#'       the user-supplied prediction function.
#'    }
#'
#' @example /R/examples/example_predict_train_model.R
#' @export
predict.forecast_model <- function(..., prediction_function = list(NULL), data = NULL) {

  model_list <- list(...)

  type <- attributes(data)$type

  if(!all(unlist(lapply(model_list, function(x) {class(x)[1]})) %in% "forecast_model")) {
    stop("The 'model_results' argument takes a list of objects of class 'forecast_model' as input. Run train_model() first.")
  }

  if(length(model_list) != length(prediction_function)) {
    stop("The number of prediction functions does not equal the number of forecast models.")
  }

  if(!type %in% c("train", "forecast")) {
    stop("The 'data' argument takes an object of class 'lagged_df' from create_lagged_df().")
  }

  outcome_cols <- attributes(model_list[[1]])$outcome_cols
  outcome_names <- attributes(model_list[[1]])$outcome_names
  row_indices <- attributes(model_list[[1]])$row_indices
  date_indices <- attributes(model_list[[1]])$date_indices
  frequency <- attributes(model_list[[1]])$frequency

  if (type == "train") {

    data_stop <- attributes(model_list[[1]])$data_stop

  } else {

    data_stop <- attributes(data)$data_stop
  }

  horizons <- attributes(model_list[[1]])$horizons
  groups <- attributes(model_list[[1]])$groups

  # Seq along model > forecast model horizon > validation window number.
  data_model <- lapply(seq_along(model_list), function(i) {

    prediction_fun <- prediction_function[[i]]

    data_horizon <- lapply(seq_along(model_list[[i]]), function(j) {

      data_win_num <- lapply(seq_along(model_list[[i]][[j]]), function(k) {

        data_results <- model_list[[i]][[j]][[k]]

        # Predict on training data or the forecast dataset?
        if (type == "train") {  # Nested cross-validation.

          x_valid <- data[[j]][row_indices %in% data_results$valid_indices, -(outcome_cols), drop = FALSE]
          y_valid <- data[[j]][row_indices %in% data_results$valid_indices, outcome_cols, drop = FALSE]  # Actuals in function return.

          data_pred <- prediction_fun(data_results$model, x_valid)  # Nested cross-validation.

          if (!is.null(groups)) {

            data_groups <- x_valid[, groups, drop = FALSE]  # save out group identifiers.
          }

        } else {  # Forecast.

          forecast_period <- data[[j]][, "index", drop = FALSE]
          names(forecast_period) <- "forecast_period"
          forecast_horizons <- data[[j]][, "horizon", drop = FALSE]
          data_for_forecast <- data[[j]][, !names(data[[j]]) %in% c("index", "horizon"), drop = FALSE]  # Remove ID columns for predict().

          data_pred <- prediction_fun(data_results$model, data_for_forecast)  # User-defined prediction function.

          if (!is.null(groups)) {

            data_groups <- data_for_forecast[, groups, drop = FALSE]
          }
        }

        if (!ncol(data_pred) %in% c(1, 3)) {
          stop("The user-defined prediction function needs to return 1- or 3-column data.frame of model predictions.")
        }

        if (ncol(data_pred) == 1) {

          names(data_pred) <- paste0(outcome_names, "_pred")

        } else {

          # Find the lower, point, and upper forecasts and order the columns accordingly.
          data_pred <- data_pred[order(unlist(lapply(data_pred, mean, na.rm = TRUE)))]

          names(data_pred) <- c(paste0(outcome_names, "_pred_lower"), paste0(outcome_names, "_pred"), paste0(outcome_names, "_pred_upper"))

          # Re-order so that the point forecast is first.
          data_pred <- data_pred[, c(2, 1, 3)]
        }

        model_name <- attributes(model_list[[i]])$model_name

        if (type == "train") {  # Nested cross-validation.

          data_temp <- data.frame("model" = model_name,
                                  "horizon" = attributes(model_list[[i]][[j]])$horizon,
                                  "window_length" = data_results$window,
                                  "window_number" = k,
                                  "valid_indices" = data_results$valid_indices)

          data_temp$date_indices <- data_results$date_indices

          if (is.null(groups)) {

            data_temp <- cbind(data_temp, y_valid, data_pred)

          } else {

            data_temp <- cbind(data_temp, data_groups, y_valid, data_pred)
          }

        } else {  # Forecast.

          data_temp <- data.frame("model" = model_name,
                                  "model_forecast_horizon" = horizons[j],
                                  "horizon" = forecast_horizons,
                                  "window_length" = data_results$window,
                                  "window_number" = k,
                                  "forecast_period" = forecast_period)

          if (is.null(groups)) {

            data_temp <- cbind(data_temp, data_pred)

          } else {

            data_temp <- cbind(data_temp, data_groups, data_pred)
          }
        }  # End forecast results.

        data_temp$model <- as.character(data_temp$model)
        data_temp
      })  # End cross-validation window predictions.
      data_win_num <- dplyr::bind_rows(data_win_num)
    })  # End horizon-level predictions.
    data_horizon <- dplyr::bind_rows(data_horizon)
  })  # End model-level predictions.
  data_out <- dplyr::bind_rows(data_model)

  data_out <- as.data.frame(data_out)

  attr(data_out, "outcome_cols") <- outcome_cols
  attr(data_out, "outcome_names") <- outcome_names
  attr(data_out, "row_indices") <- row_indices
  attr(data_out, "date_indices") <- date_indices
  attr(data_out, "frequency") <- frequency
  attr(data_out, "data_stop") <- data_stop
  attr(data_out, "groups") <- groups

  if (type == "train") {
    class(data_out) <- c("training_results", "forecast_model", class(data_out))
  } else {
    class(data_out) <- c("forecast_results", "forecast_model", class(data_out))
  }

  return(data_out)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#' Plot an object of class training_results
#'
#' Several diagnostic plots can be returned to assess the quality of the forecasts
#' based on predictions on the validation datasets.
#'
#' @param x An object of class 'training_results' from \code{predict.forecast_mode()l}.
#' @param type Plot type, default is "prediction", for hold-out sample predictions.
#' @param models Optional. Filter results by user-defined model name from \code{train_model()}.
#' @param horizons Optional. A numeric vector of horizons to filter results by horizon.
#' @param windows Optional. A numeric vector of windows to filter results by validation window number.
#' @param valid_indices Optional. A numeric or date vector to filter results by validation row indices or dates.
#' @param group_filter Optional. A string for filtering plot results for grouped time-series
#' (e.g., \code{"group_col_1 == 'A'"}). The results are passed to \code{dplyr::filter()} internally.
#' @param ... Arguments passed to \code{base::plot()}
#' @return Diagnostic plots of class 'ggplot'.
#' @export
plot.training_results <- function(x,
                                  type = c("prediction", "residual", "forecast_stability", "forecast_variability"),
                                  models = NULL, horizons = NULL,
                                  windows = NULL, valid_indices = NULL, group_filter = NULL, ...) {

  data <- x

  type <- type[1]

  if (!methods::is(data, "training_results")) {
    stop("The 'data' argument takes an object of class 'training_results' as input. Run predict() on a 'forecast_model' object first.")
  }

  if (type == "forecast_stability") {
    if (!xor(is.null(windows), is.null(valid_indices))) {
      stop("Select either (a) one or more validation windows, 'windows', or (b) a range of dataset rows, 'valid_indices', to reduce plot size.")
    }
  }

  if (!is.null(attributes(data)$group) & !type %in% c("prediction", "residual")) {
    stop("Only 'prediction' and 'residual' plots are currently available for grouped models")

  }
  #----------------------------------------------------------------------------

  outcome_cols <- attributes(data)$outcome_cols
  outcome_names <- attributes(data)$outcome_names
  date_indices <- attributes(data)$date_indices
  frequency <- attributes(data)$frequency
  groups <- attributes(data)$group
  n_outcomes <- length(outcome_cols)

  forecast_stability_plot_windows <- windows

  data$residual <- data[, outcome_names] - data[, paste0(outcome_names, "_pred")]

  forecast_horizons <- sort(unique(data$horizon))

  models <- if (is.null(models)) {unique(data$model)} else {models}
  horizons <- if (is.null(horizons)) {unique(data$horizon)} else {horizons}
  windows <- if (is.null(windows)) {unique(data$window_number)} else {windows}
  valid_indices <- if (is.null(valid_indices)) {unique(data$valid_indices)} else {valid_indices}

  data_plot <- data

  data_plot <- data_plot[data_plot$model %in% models & data_plot$horizon %in% horizons &
                         data_plot$window_number %in% windows, ]

  if (methods::is(valid_indices, "Date")) {

    data_plot <- data_plot[data_plot$date_indices %in% valid_indices, ]  # Filter plots by dates.
    data_plot$index <- data_plot$date_indices

  } else {

    data_plot <- data_plot[data_plot$valid_indices %in% valid_indices, ]  # Filter plots by row indices.

    if (!is.null(date_indices)) {

      data_plot$index <- data_plot$date_indices

    } else {

      data_plot$index <- data_plot$valid_indices

    }
  }

  if (!is.null(group_filter)) {

    data_plot <- dplyr::filter(data_plot, eval(parse(text = group_filter)))
  }
  #----------------------------------------------------------------------------
  # Create different line segments in ggplot with `color = ggplot_color_group`.
  data_plot$ggplot_color_group <- apply(data_plot[,  c("model", groups), drop = FALSE], 1, function(x) {paste(x, collapse = "-")})

  data_plot$ggplot_color_group <- ordered(data_plot$ggplot_color_group, levels = unique(data_plot$ggplot_color_group))
  #----------------------------------------------------------------------------
  # Fill in date gaps with NAs so ggplot doesn't connect line segments where there were no entries recorded.
  if (!is.null(groups)) {

    data_plot_template <- expand.grid("index" = seq(min(date_indices, na.rm = TRUE), max(date_indices, na.rm = TRUE), by = frequency),
                                      "ggplot_color_group" = unique(data_plot$ggplot_color_group),
                                      "horizon" = horizons,
                                      stringsAsFactors = FALSE)

    data_plot <- dplyr::left_join(data_plot_template, data_plot, by = c("index", "horizon", "ggplot_color_group"))

    # Create a dataset of points for those instances where there the outcomes are NA before and after a given instance.
    # Points are needed because ggplot will not plot a 1-instance geom_line().
    data_plot_point <- data_plot %>%
      dplyr::group_by(.data$ggplot_color_group) %>%
      dplyr::mutate("lag" = dplyr::lag(eval(parse(text = outcome_names)), 1),
                    "lead" = dplyr::lead(eval(parse(text = outcome_names)), 1)) %>%
      dplyr::filter(is.na(.data$lag) & is.na(.data$lead))

    data_plot_point$ggplot_color_group <- factor(data_plot_point$ggplot_color_group, ordered = TRUE, levels(data_plot$ggplot_color_group))

    data_plot <- data_plot[data_plot$date_indices %in% date_indices[valid_indices], ]
    data_plot_point <- data_plot_point[data_plot_point$date_indices %in% date_indices[valid_indices], ]

  }
  #----------------------------------------------------------------------------

  if (type %in% c("prediction", "residual")) {

    # Melt the data for plotting.
    data_plot <- tidyr::gather(data_plot, "outcome", "value",
                               -!!names(data_plot)[!names(data_plot) %in% c(outcome_names, paste0(outcome_names, "_pred"))])

    # If date indices exist, plot with them.
    if (!is.null(date_indices)) {
      data_plot$index <- data_plot$date_indices
    }

    if (type == "prediction") {

      p <- ggplot(data_plot[data_plot$outcome != outcome_names, ],
                  aes(x = .data$index, y = .data$value,
                      group = .data$ggplot_color_group, color = .data$ggplot_color_group))

      p <- p + geom_line(size = 1.05, linetype = 1)

      # If the plotting data.frame has bother lower and upper forecasts plot these bounds.
      if (c(all(any(grepl("_pred_lower", names(data_plot))), any(grepl("_pred_upper", names(data_plot)))))) {

        p <- p + geom_ribbon(data = data_plot[data_plot$outcome == outcome_names, ],
                             aes(x = .data$index, ymin = eval(parse(text = paste0(outcome_names, "_pred_lower"))),
                                 ymax = eval(parse(text = paste0(outcome_names, "_pred_upper"))),
                                 fill = .data$ggplot_color_group, color = NULL), alpha = .25, show.legend = FALSE)
      }

      if (is.null(groups)) {

        p <- p + geom_line(data = data_plot[data_plot$outcome == outcome_names, ],
                           aes(x = .data$index, y = .data$value), color = "grey50")

      } else {

        p <- p + geom_line(data = data_plot[data_plot$outcome == outcome_names, ],
                           aes(x = .data$index, y = .data$value,
                               group = .data$ggplot_color_group,
                               color = .data$ggplot_color_group), linetype = 2)
      }

    } else if (type == "residual") {

      p <- ggplot(data_plot[data_plot$outcome != outcome_names, ],
                  aes(x = .data$index, y = .data$residual,
                      group = .data$ggplot_color_group, color = .data$ggplot_color_group))
      p <- p + geom_line(size = 1.05, linetype = 1)
      p <- p + geom_hline(yintercept = 0)
    }

    p <- p + scale_color_viridis_d()
    p <- p + facet_grid(horizon ~ ., drop = TRUE)
    p <- p + theme_bw()
      if (type == "prediction") {
        p <- p + xlab("Dataset index/row") + ylab("Outcome") + labs(color = "Model") +
        ggtitle("Forecasts vs. Actuals Through Time - Faceted by horizon")
      } else if (type == "residual") {
        p <- p + xlab("Dataset index/row") + ylab("Residual") + labs(color = "Model") +
        ggtitle("Forecast Error Through Time - Faceted by forecast horizon",
                subtitle = "Dashed lines and empty points are actuals")
      }
    return(p)
  }
  #----------------------------------------------------------------------------

  if (type %in% c("forecast_stability")) {

    data_plot$forecast_origin <- with(data_plot, valid_indices - horizon)

    # data_plot$group <- with(data_plot, paste0(window_length, "_", valid_indices))
    data_plot$group <- with(data_plot, paste0(valid_indices))
    data_plot$group <- ordered(data_plot$group)

    # Plotting the original time-series in each facet. Because the plot is faceted by valid_indices, we'll do a bit of a hack here to create
    # the same line plot for each facet.
    data_outcome <- data_plot %>%
      dplyr::select(valid_indices, !!outcome_names) %>%
      dplyr::distinct(valid_indices, .keep_all = TRUE)
    data_outcome$index <- data_outcome$valid_indices
    data_outcome$valid_indices <- NULL  # remove to avoid confusion in facet_wrap()

    data_outcome <- data_outcome[rep(1:nrow(data_outcome), length(unique(data_plot$valid_indices))), ]

      p <- ggplot()
      if (max(data_plot$horizon) != 1) {
        p <- p + geom_line(data = data_plot, aes(x = .data$forecast_origin,
                                                 y = eval(parse(text = paste0(outcome_names, "_pred"))),
                                                 color = factor(.data$model)), size = 1, linetype = 1, show.legend = FALSE)
      }
      p <- p + geom_point(data = data_plot, aes(x = .data$forecast_origin,
                                                y = eval(parse(text = paste0(outcome_names, "_pred"))),
                                                color = factor(.data$model)))
      p <- p + geom_point(data = data_plot, aes(x = .data$valid_indices,
                                                y = eval(parse(text = outcome_names)), fill = "Actual"))
      p <- p + scale_color_viridis_d()
      p <- p + facet_wrap(~ valid_indices)

      p <- p + geom_line(data = data_outcome, aes(x = .data$index,
                                                  y = eval(parse(text = outcome_names))), color = "gray50")
      p <- p + theme_bw()
      p <- p + xlab("Dataset index/row") + ylab("Outcome") + labs(color = "Model") + labs(fill = NULL) +
        ggtitle("Rolling Origin Forecast Stability - Faceted by dataset index/row")
    return(p)
  }
  #----------------------------------------------------------------------------

  if (type %in% c("forecast_variability")) {

    data_plot_summary <- data_plot %>%
      dplyr::group_by(.data$model, .data$valid_indices,
                      .data$window_length, .data$window_number) %>%
      dplyr::summarise("cov" = base::abs(stats::sd(eval(parse(text = paste0(outcome_names, "_pred"))), na.rm = TRUE) / mean(eval(parse(text = paste0(outcome_names, "_pred"))), na.rm = TRUE))) %>%
      dplyr::distinct(.data$model, .data$valid_indices,
                      .data$window_length, .keep_all = TRUE)
    data_plot_summary$group <- with(data_plot_summary, paste0(window_length))
    data_plot_summary$group <- ordered(data_plot_summary$group)

    data_outcome <- data_plot_summary
    data_outcome$window_number <- NULL
    data_outcome <- dplyr::distinct(data_outcome, .data$valid_indices,
                                    .data$window_length, .keep_all = TRUE)

    data_outcome <- dplyr::left_join(data_outcome, data_plot, by = c("model", "valid_indices", "window_length"))
    data_outcome <- dplyr::distinct(data_outcome, .data$valid_indices,
                                    .data$window_length, .keep_all = TRUE)

    # For each plot facet, create columns to min-max scale the original time-series data.
    data_outcome <- data_outcome %>%
      dplyr::group_by(.data$window_length) %>%
      dplyr::mutate("min_scale" = min(cov, na.rm = TRUE),
                    "max_scale" = max(cov, na.rm = TRUE)) %>%
      dplyr::ungroup()

    data_outcome$outcome_scaled <- (((data_outcome$max_scale - data_outcome$min_scale) * (data_outcome[, outcome_names, drop = TRUE] - min(data_outcome[, outcome_names, drop = TRUE], na.rm = TRUE))) /
                                      (max(data_outcome[, outcome_names, drop = TRUE], na.rm = TRUE) - min(data_outcome[, outcome_names, drop = TRUE], na.rm = TRUE))) + data_outcome$min_scale

    p <- ggplot()
    p <- p + geom_line(data = data_plot_summary, aes(x = .data$valid_indices,
                                                     y = .data$cov, color = factor(.data$model),
                                                     group = paste0(.data$model, .data$window_number)), size = 1,
                       linetype = 1, alpha = .50)
    p <- p + geom_point(data = data_plot_summary, aes(x = .data$valid_indices,
                                                      y = .data$cov,
                                                      color = factor(.data$model),
                                                      group = paste0(.data$model, .data$window_number)),
                        show.legend = FALSE)
    p <- p + geom_line(data = data_outcome, aes(.data$valid_indices, .data$outcome_scaled,
                                                group = .data$window_number), color = "grey50")
    p <- p + scale_color_viridis_d()
    p <- p + theme_bw()
    p <- p + xlab("Dataset index/row") + ylab("Coefficient of variation (Abs)") + labs(color = "Model") +
      ggtitle("Forecast Variability Across Forecast Horizons")
    return(p)
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#' Plot an object of class forecast_results
#'
#' A forecast plot for each horizon for each model in \code{predict.forecast_model()}.
#'
#' @param x An object of class 'forecast_results' from \code{predict.forecast_model()}.
#' @param data_actual A data.frame containing the target/outcome name and any grouping columns.
#' @param actual_indices Required if 'data_actual' is given. A vector or 1-column data.frame
#' of numeric row indices or dates (class 'Date') with length \code{nrow(data_actual)}.
#' The data can be historical and/or holdout/test data, forecasts and actuals are matched by \code{row.names()}.
#' @param models Optional. Filter results by user-defined model name from \code{train_model()}.
#' @param horizons Optional. Filter results by horizon.
#' @param windows Optional. Filter results by validation window number.
#' @param facet_plot Adjust the plot display through \code{ggplot2::facet_grid()}.
#' \code{facet_plot = NULL} plots results in one facet.
#' @param group_filter Optional. A string for filtering plot results for grouped time-series (e.g., \code{"group_col_1 == 'A'"});
#' passed to \code{dplyr::filter()} internally.
#' @param ... Arguments passed to \code{base::plot()}
#' @return Forecast plot of class 'ggplot'.
#' @export
plot.forecast_results <- function(x, data_actual = NULL, actual_indices = NULL,
                                  models = NULL, horizons = NULL,
                                  windows = NULL,
                                  facet_plot = c("model", "model_forecast_horizon"),
                                  group_filter = NULL, ...) {

  data_forecast <- x

  if(!methods::is(data_forecast, "forecast_results")) {
    stop("The 'forecast_results' argument takes an object of class 'forecast_results' as input. Run predict() on a 'forecast_model' object first.")
  }

  type <- "forecast"  # Only one plot option at present.

  outcome_cols <- attributes(data_forecast)$outcome_cols
  outcome_names <- attributes(data_forecast)$outcome_names
  date_indices <- attributes(data_forecast)$date_indices
  groups <- attributes(data_forecast)$group
  n_outcomes <- length(outcome_cols)

  if (!is.null(data_actual)) {

    data_actual <- data_actual[, c(outcome_names, groups), drop = FALSE]

    data_actual$index <- actual_indices

    if (!is.null(group_filter)) {

      data_actual <- dplyr::filter(data_actual, eval(parse(text = group_filter)))
    }
  }

  forecast_horizons <- sort(unique(data_forecast$model_forecast_horizon))

  models <- if (is.null(models)) {unique(data_forecast$model)} else {models}
  horizons <- if (is.null(horizons)) {unique(data_forecast$model_forecast_horizon)} else {horizons}
  windows <- if (is.null(windows)) {unique(data_forecast$window_number)} else {windows}

  data_forecast <- data_forecast[data_forecast$model %in% models &
                                 data_forecast$model_forecast_horizon %in% horizons &
                                 data_forecast$window_number %in% windows, ]

  if (!is.null(group_filter)) {

    data_forecast$index <- as.numeric(row.names(data_forecast))

    data_forecast <- dplyr::filter(data_forecast, eval(parse(text = group_filter)))
  }

  data_forecast$model_forecast_horizon <- as.integer(data_forecast$model_forecast_horizon)
  data_forecast$window_number <- as.integer(data_forecast$window_number)

  data_forecast$model_forecast_horizon <- ordered(data_forecast$model_forecast_horizon, levels = rev(sort(unique(data_forecast$model_forecast_horizon))))
  data_forecast$window_number <- ordered(as.numeric(data_forecast$window_number), levels = rev(sort(unique(data_forecast$window_number))))
  #----------------------------------------------------------------------------

  if (type %in% c("forecast")) {

    possible_plot_facets <- c("model", "model_forecast_horizon")

    if (is.null(facet_plot)) {facet_plot <- ""}

    if (all(facet_plot == "model")) {
      facet_formula <- as.formula(paste("~", facet_plot[1]))
    } else if (all(facet_plot == "model_forecast_horizon")) {
      facet_formula <- as.formula(paste(facet_plot[1], "~ ."))
    } else if (length(facet_plot) == 2) {
      facet_formula <- as.formula(paste(facet_plot[2], "~", facet_plot[1]))
    }

    # For dimensions that aren't facets, create a grouping variable for ggplot.
    plot_group <- c(possible_plot_facets[!possible_plot_facets %in% facet_plot], "window_number", groups)

    data_forecast$plot_group <- apply(data_forecast[, plot_group, drop = FALSE], 1, paste, collapse = " + ")
    data_forecast$plot_group <- ordered(data_forecast$plot_group, levels = unique(data_forecast$plot_group))

    p <- ggplot()

    if (1 %in% horizons) {  # Use geom_point instead of geom_line to plot a 1-step-ahead forecast.

      p <- p + geom_point(data = data_forecast[data_forecast$model_forecast_horizon == 1, ],
                          aes(x = .data$forecast_period, y = eval(parse(text = paste0(outcome_names, "_pred"))),
                              color = .data$plot_group, group = .data$plot_group), show.legend = FALSE)
      }

    if (!all(horizons == 1)) {  # Plot forecasts for model forecast horizons > 1.

      p <- p + geom_line(data = data_forecast[data_forecast$model_forecast_horizon != 1, ],
                         aes(x = .data$forecast_period, y = eval(parse(text = paste0(outcome_names, "_pred"))),
                             color = .data$plot_group, group = .data$plot_group))

      # If the plotting data.frame has bother lower and upper forecasts plot these bounds.
      if (c(all(any(grepl("_pred_lower", names(data_forecast))), any(grepl("_pred_upper", names(data_forecast)))))) {

        p <- p + geom_ribbon(data = data_forecast[data_forecast$model_forecast_horizon != 1, ],
                             aes(x = .data$forecast_period, ymin = eval(parse(text = paste0(outcome_names, "_pred_lower"))),
                                 ymax = eval(parse(text = paste0(outcome_names, "_pred_upper"))),
                                 fill = .data$plot_group, color = NULL), alpha = .25, show.legend = FALSE)
      }
      }

    p <- p + geom_vline(xintercept = attributes(data_forecast)$data_stop, color = "red")

    if (!is.null(data_actual)) {

      data_actual$plot_group <- apply(data_actual[, groups, drop = FALSE], 1, paste, collapse = " + ")
      data_actual$plot_group <- ordered(data_actual$plot_group, levels = unique(data_actual$plot_group))

      if (is.null(groups)) {
        p <- p + geom_line(data = data_actual, aes(x = .data$index,
                                                   y = eval(parse(text = outcome_names))), color = "grey50")
      } else {
        p <- p + geom_line(data = data_actual, aes(x = .data$index,
                                                   y = eval(parse(text = outcome_names)),
                                                   color = .data$plot_group,
                                                   group = .data$plot_group))
      }
    }

    if (all(facet_plot != "")) {
      p <- p + facet_grid(facet_formula)
    }

    p <- p + scale_color_viridis_d()
    p <- p + theme_bw()
    p <- p + xlab("Dataset row / index") + ylab("Outcome") + labs(color = toupper(gsub("_", " ", paste(plot_group, collapse = " + \n")))) +
      ggtitle("N-Step-Ahead Model Forecasts")
    return(p)
  }
}
