
#' Train a model across horizons and validation datasets
#'
#' Train a user-defined forecast model for each horizon, h, and across the validation
#' datasets, d. A total of h * d models are trained--more if the user-defined model
#' performs any inner-loop cross-validation.
#'
#' @param lagged_df An object of class 'lagged_df' from create_lagged_df().
#' @param windows An object of class 'windows' from create_windows().
#' @param model_function A user-defined wrapper function for model training (see example).
#' @param model_name A name for the model (required).
#' @return A'forecast_model' object: A nested list of model results, nested by horizon > validation dataset.
#' @example /R/examples/example_train_model.R
#' @export
train_model <- function(lagged_df, windows, model_function, model_name = NULL) {

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
  date_indices <- attributes(data)$dates
  horizons <- attributes(data)$horizons
  data_stop <- attributes(data)$data_stop
  n_outcomes <- length(outcome_cols)
  groups <- attributes(data)$groups

  window_indices <- windows

  # Seq along model forecast horizon > cross-validation windows.
  data_out <- lapply(data, function(data) {
    #data <- data[[1]]
    #i <- 1
    model_plus_valid_data <- lapply(1:nrow(window_indices), function(i) {

      window_length <- window_indices[i, "window_length"]

      if (is.null(dates)) {

        valid_indices <- window_indices[i, "start"]:window_indices[i, "stop"]

      } else {

        valid_indices <- which(date_indices >= window_indices[i, "start"] &
                               date_indices <= window_indices[i, "stop"])
      }

      # A window length of 0 removes the nested cross-validation and trains on all input data in lagged_df.
      if (window_length == 0) {

        data_train <- data

      } else {

        data_train <- data[!row_indices %in% valid_indices, , drop = FALSE]
      }

      # Model training.
      model <- model_function(data_train, outcome_cols)

      x_valid <- data[row_indices %in% valid_indices, -(1:n_outcomes), drop = FALSE]
      y_valid <- data[row_indices %in% valid_indices, 1:n_outcomes, drop = FALSE]

      model_plus_valid_data  <- list("model" = model, "x_valid" = x_valid,
                                     "y_valid" = y_valid, "window" = window_length,
                                     "valid_indices" = valid_indices)

      model_plus_valid_data
    })  # End model training across nested cross-validation windows for the horizon in "data".

    attr(model_plus_valid_data, "horizon") <- attributes(data)$horizon
    model_plus_valid_data
  })  # End training across horizons.

  attr(data_out, "model_name") <- model_name
  attr(data_out, "outcome_cols") <- outcome_cols
  attr(data_out, "outcome_names") <- outcome_names
  attr(data_out, "row_indices") <- row_indices
  attr(data_out, "date_indices") <- date_indices
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
#' Predict with a 'forecast_model' object from train_model(). If data_forecast = NULL,
#' predictions are returned for the outer-loop nested cross-validation datasets.
#' If data_forecast is an object of class 'lagged_df' from create_lagged_df(..., type = "forecast"),
#' predictions are returned for the horizons specified in create_lagged_df().
#'
#' @param ... One or more trained models from train_model().
#' @param prediction_function A list of user-defined prediction functions.
#' @param data_forecast If NULL, predictions are returned for the validation datasets. If
#' an object of class 'lagged_df' from create_lagged_df(..., type = "forecast"), forecasts from 1:h.
#' @return If data_forecast = NULL, a 'training_results' object. If data_forecast = create_lagged_df(..., type = "forecast"),
#' a 'forecast_results' object.
#' @example /R/examples/example_predict_train_model.R
#' @export
predict.forecast_model <- function(..., prediction_function = list(NULL), data_forecast = NULL) {

  model_list <- list(...)

  if(!all(unlist(lapply(model_list, function(x) {class(x)[1]})) %in% "forecast_model")) {
    stop("The 'model_results' argument takes a list of objects of class 'forecast_model' as input. Run train_model() first.")
  }

  if(length(model_list) != length(prediction_function)) {
    stop("The number of prediction functions does not equal the number of forecast models.")
  }

  outcome_cols <- attributes(model_list[[1]])$outcome_cols
  outcome_names <- attributes(model_list[[1]])$outcome_names
  row_indices <- attributes(model_list[[1]])$row_indices
  date_indices <- attributes(model_list[[1]])$date_indices
  if (is.null(data_forecast)) {
    data_stop <- attributes(model_list[[1]])$data_stop
  } else {
    data_stop <- attributes(data_forecast)$data_stop
  }
  horizons <- attributes(model_list[[1]])$horizons
  groups <- attributes(model_list[[1]])$groups

  # Seq along model > forecast model horizon > validation window number.
  data_model <- lapply(seq_along(model_list), function(i) {
    #i <- j <- k <- 1
    prediction_fun <- prediction_function[[i]]

    data_horizon <- lapply(seq_along(model_list[[i]]), function(j) {

      data_win_num <- lapply(seq_along(model_list[[i]][[j]]), function(k) {

        data_results <- model_list[[i]][[j]][[k]]
        forecast_horizons <- data_forecast[[j]][, "horizon", drop = FALSE]
        data_for_forecast <- data_forecast[[j]][, !names(data_forecast[[j]]) == "horizon", drop = FALSE]  # Remove "horizon" for predict().

        # Predict on training data or the forecast dataset?
        if (is.null(data_forecast)) {

          data_pred <- prediction_fun(data_results$model, data_results$x_valid)  # Nested cross-validation.

          if (!is.null(groups)) {

            data_groups <- data_results$x_valid[, groups, drop = FALSE]
          }

        } else {

          data_pred <- prediction_fun(data_results$model, data_for_forecast)  # Forecast.

          if (!is.null(groups)) {

            data_groups <- data_for_forecast[, groups, drop = FALSE]
          }
        }

        names(data_pred) <- paste0(outcome_names, "_pred")  # 'data_pred' is a 1-column data.frame.

        model_name <- attributes(model_list[[i]])$model_name

        if (is.null(data_forecast)) {  # Nested cross-validation.

          data_temp <- data.frame("model" = model_name,
                                  "horizon" = attributes(model_list[[i]][[j]])$horizon,
                                  "window_length" = data_results$window,
                                  "window_number" = k,
                                  "valid_indices" = data_results$valid_indices)

          if (is.null(groups)) {

            data_temp <- cbind(data_temp, data_results$y_valid, data_pred)

          } else {

            data_temp <- cbind(data_temp, data_groups, data_results$y_valid, data_pred)
          }

        } else {  # Forecast.

          data_temp <- data.frame("model" = model_name,
                                  "model_forecast_horizon" = horizons[j],
                                  "horizon" = forecast_horizons,
                                  "window_length" = data_results$window,
                                  "window_number" = k,
                                  "forecast_period" = NA)  # For data.frame position, filled in in the code below.

          if (is.null(groups)) {

            data_temp <- cbind(data_temp, data_pred)

          } else {

            data_temp <- cbind(data_temp, data_groups, data_pred)
          }

          if (is.null(date_indices)) {  # Add row index column for forecast horizons.

            data_temp$forecast_period <- max(row_indices, na.rm = TRUE) + data_temp$horizon

          } else {  # Add date column for forecast horizons.

            max_date <- max(attributes(data_forecast)$dates, na.rm = TRUE)

            data_merge <- data.frame("date" = seq(max_date, by = attributes(data_forecast)$frequency, length = max(unique(data_temp$horizon), na.rm = TRUE)))
            data_merge$horizon <- 1:nrow(data_merge)

            data_temp <- dplyr::left_join(data_temp, data_merge, by = "horizon")
            data_temp$forecast_period <- data_temp$date
            data_temp$date <- NULL
          }
        }
        data_temp$model <- as.character(data_temp$model)
        data_temp
      })  # End cross-validation window predictions.
      data_win_num <- dplyr::bind_rows(data_win_num)
    })
    data_horizon <- dplyr::bind_rows(data_horizon)
  })
  data_out <- dplyr::bind_rows(data_model)

  attr(data_out, "outcome_cols") <- outcome_cols
  attr(data_out, "outcome_names") <- outcome_names
  attr(data_out, "row_indices") <- row_indices
  attr(data_out, "date_indices") <- date_indices
  attr(data_out, "data_stop") <- data_stop  # To-do: this may only be relevant for class "forecast_results".
  attr(data_out, "groups") <- groups

  if (is.null(data_forecast)) {
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
#' Several diagnostic plots can be returned to assess the quality of the forecats
#' based on predictions on the outer-loop validation datasets.
#'
#' @param training_results An object of class 'training_results' from predict.forecast_model().
#' @param type Plot type, default is "prediction".
#' @param models Filter results by user-defined model name from train_model() (optional).
#' @param horizons Filter results by horizon (optional).
#' @param windows Filter results by validation window number (optional).
#' @param valid_indices Filter results by validation row index (optional).
#' @return Diagnostic plots of class 'ggplot'.
#' @export
plot.training_results <- function(training_results,
                                  type = c("prediction", "residual", "forecast_stability", "forecast_variability"),
                                  models = NULL, horizons = NULL,
                                  windows = NULL, valid_indices = NULL) {

  data <- training_results

  if(!methods::is(data, "training_results")) {
    stop("The 'data' argument takes an object of class 'training_results' as input. Run predict() on a 'forecast_model' object first.")
  }

  data_results <- data

  type <- type[1]

  if (type == "forecast_stability") {
    if (!xor(is.null(windows), is.null(valid_indices))) {
      stop("Select either (a) one or more validation windows, 'windows', or (b) a range of dataset rows, 'valid_indices', to reduce plot size.")
    }
  }

  outcome_cols <- attributes(data_results)$outcome_cols
  outcome_names <- attributes(data_results)$outcome_names

  data_plot <- data_results

  data_plot$residual <- data_plot[, outcome_names] - data_plot[, paste0(outcome_names, "_pred")]

  forecast_stability_plot_windows <- windows

  models <- if (is.null(models)) {unique(data_plot$model)} else {models}
  horizons <- if (is.null(horizons)) {unique(data_plot$horizon)} else {horizons}
  windows <- if (is.null(windows)) {unique(data_plot$window_number)} else {windows}
  valid_indices <- if (is.null(valid_indices)) {unique(data_plot$valid_indices)} else {valid_indices}

  data_plot <- data_plot[data_plot$model %in% models & data_plot$horizon %in% horizons &
                         data_plot$window_number %in% windows
                         & data_plot$valid_indices %in% valid_indices, ]
  #----------------------------------------------------------------------------

  if (type %in% c("prediction", "residual")) {

    # Melt the data for plotting.
    data_plot <- tidyr::gather(data_plot, "outcome", "value",
                               -!!names(data_plot)[!names(data_plot) %in% c(outcome_names, paste0(outcome_names, "_pred"))])

    data_plot$group <- with(data_plot, paste0(model, "_", horizon, "_", window_length, "_", window_number))
    data_plot$group <- ordered(data_plot$group)

    if (type == "prediction") {
      p <- ggplot(data_plot[data_plot$outcome != outcome_names, ], aes(x = valid_indices, y = value, color = factor(model), group = group))
      p <- p + geom_line(size = 1.05, linetype = 1)
      p <- p + geom_line(data = data_plot[data_plot$outcome == outcome_names, ],
                         aes(x = valid_indices, y = value), color = "grey50")

    } else if (type == "residual") {
      p <- ggplot(data_plot[data_plot$outcome != outcome_names, ], aes(x = valid_indices, y = residual, color = factor(model), group = group))
      p <- p + geom_line(size = 1.05, linetype = 1)
      p <- p + geom_hline(yintercept = 0)
    }

    p <- p + scale_color_viridis_d()
    p <- p + facet_grid(horizon ~ .)
    p <- p + theme_bw()
      if (type == "prediction") {
        p <- p + xlab("Dataset index/row") + ylab("Outcome") + labs(color = "Model") +
        ggtitle("Forecasts vs. Actuals Through Time - Faceted by horizon")
      } else if (type == "residual") {
        p <- p + xlab("Dataset index/row") + ylab("Residual") + labs(color = "Model") +
        ggtitle("Forecast Error Through Time - Faceted by forecast horizon")
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
        p <- p + geom_line(data = data_plot, aes(x = forecast_origin, y = eval(parse(text = paste0(outcome_names, "_pred"))), color = factor(model)), size = 1, linetype = 1, show.legend = FALSE)
      }
      p <- p + geom_point(data = data_plot, aes(x = forecast_origin, y = eval(parse(text = paste0(outcome_names, "_pred"))), color = factor(model)))
      p <- p + geom_point(data = data_plot, aes(x = valid_indices, y = eval(parse(text = outcome_names)), fill = "Actual"))
      p <- p + scale_color_viridis_d()
      p <- p + facet_wrap(~ valid_indices)

      p <- p + geom_line(data = data_outcome, aes(x = index, y = eval(parse(text = outcome_names))), color = "gray50")
      p <- p + theme_bw()
      p <- p + xlab("Dataset index/row") + ylab("Outcome") + labs(color = "Model") + labs(fill = NULL) +
        ggtitle("Rolling Origin Forecast Stability - Faceted by dataset index/row")
    return(p)
  }
  #----------------------------------------------------------------------------

  if (type %in% c("forecast_variability")) {

    data_plot_summary <- data_plot %>%
      dplyr::group_by(model, valid_indices, window_length, window_number) %>%
      dplyr::summarise("cov" = abs(sd(eval(parse(text = paste0(outcome_names, "_pred"))), na.rm = TRUE) / mean(eval(parse(text = paste0(outcome_names, "_pred"))), na.rm = TRUE))) %>%
      dplyr::distinct(model, valid_indices, window_length, .keep_all = TRUE)
    data_plot_summary$group <- with(data_plot_summary, paste0(window_length))
    data_plot_summary$group <- ordered(data_plot_summary$group)

    data_outcome <- data_plot_summary
    data_outcome$window_number <- NULL
    data_outcome <- dplyr::distinct(data_outcome, valid_indices, window_length, .keep_all = TRUE)

    data_outcome <- dplyr::left_join(data_outcome, data_plot, by = c("model", "valid_indices", "window_length"))
    data_outcome <- dplyr::distinct(data_outcome, valid_indices, window_length, .keep_all = TRUE)

    # For each plot facet, create columns to min-max scale the original time-series data.
    data_outcome <- data_outcome %>%
      dplyr::group_by(window_length) %>%
      dplyr::mutate("min_scale" = min(cov, na.rm = TRUE),
                    "max_scale" = max(cov, na.rm = TRUE)) %>%
      dplyr::ungroup()

    data_outcome$outcome_scaled <- (((data_outcome$max_scale - data_outcome$min_scale) * (data_outcome[, outcome_names, drop = TRUE] - min(data_outcome[, outcome_names, drop = TRUE], na.rm = TRUE))) /
                                      (max(data_outcome[, outcome_names, drop = TRUE], na.rm = TRUE) - min(data_outcome[, outcome_names, drop = TRUE], na.rm = TRUE))) + data_outcome$min_scale

    p <- ggplot()
    p <- p + geom_line(data = data_plot_summary, aes(x = valid_indices, y = cov, color = factor(model), group = paste0(model, window_number)), size = 1, linetype = 1, alpha = .50)
    p <- p + geom_point(data = data_plot_summary, aes(x = valid_indices, y = cov, color = factor(model), group = paste0(model, window_number)), show.legend = FALSE)
    p <- p + geom_line(data = data_outcome, aes(valid_indices, outcome_scaled, group = window_number), color = "grey50")
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
#' A forecast plot for each horizon for each model in predict.forecast_model().
#'
#' @param forecast_results An object of class 'forecast_results' from predict.forecast_model().
#' @param data_actual A data.frame containing the target/outcome name; other columns are ignored.
#' The data can be historical and/or holdout/test data, forecasts and actuals are matched by row.names().
#' @param models Filter results by user-defined model name from train_model() (optional).
#' @param horizons Filter results by horizon (optional).
#' @param windows Filter results by validation window number (optional).
#' @param facet_plot Adjust the plot display through ggplot2::facet_grid(). facet_plot = NULL plots results in one facet.
#' @return Forecast plot of class 'ggplot'.
#' @export
plot.forecast_results <- function(forecast_results, data_actual = NULL,
                                  models = NULL, horizons = NULL,
                                  windows = NULL,
                                  facet_plot = c("model", "model_forecast_horizon")) {

  data_forecast <- forecast_results
  #data_forecast <- data_forecasts  # testing
  #data_actual <- data  # testing

  if(!methods::is(data_forecast, "forecast_results")) {
    stop("The 'forecast_results' argument takes an object of class 'forecast_results' as input. Run predict() on a 'forecast_model' object first.")
  }

  type <- "forecast"  # only one plot option at present.

  outcome_cols <- attributes(data_forecast)$outcome_cols
  outcome_names <- attributes(data_forecast)$outcome_names
  date_indices <- attributes(data_forecast)$date_indices
  groups <- attributes(data_forecast)$group
  n_outcomes <- length(outcome_cols)

  if (!is.null(data_actual)) {
    data_actual <- data_actual[, c(outcome_names, groups), drop = FALSE]
  }

  forecast_horizons <- sort(unique(data_forecast$model_forecast_horizon))

  models <- if (is.null(models)) {unique(data_forecast$model)} else {models}
  horizons <- if (is.null(horizons)) {unique(data_forecast$model_forecast_horizon)} else {horizons}
  windows <- if (is.null(windows)) {unique(data_forecast$window_number)} else {windows}

  data_forecast <- data_forecast[data_forecast$model %in% models &
                                 data_forecast$model_forecast_horizon %in% horizons &
                                 data_forecast$window_number %in% windows, ]

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

    #data_forecast$index <- data_forecast$horizon

    # if (!is.null(data_actual)) {
    #   data_forecast$index <- data_forecast$forecast_period  # data_forecast$horizon + attributes(data_forecast)$data_stop
    # }

    #data_actual <- tail(data_actual)

    p <- ggplot()

    if (1 %in% horizons) {  # Use geom_point instead of geom_line to plot a 1-step-ahead forecast.

      p <- p + geom_point(data = data_forecast[data_forecast$model_forecast_horizon == 1, ],
                          aes(x = index, y = eval(parse(text = paste0(outcome_names, "_pred"))),
                              color = plot_group, group = plot_group), show.legend = FALSE)

    } else {  # Plot forecasts for model forecast horizons > 1.

      p <- p + geom_line(data = data_forecast[data_forecast$model_forecast_horizon != 1, ],
                         aes(x = forecast_period, y = eval(parse(text = paste0(outcome_names, "_pred"))),
                             color = plot_group, group = plot_group))
    }

    #p <- p + geom_vline(xintercept = attributes(data_forecast)$data_stop, color = "red")

    if (!is.null(data_actual)) {

      data_actual$plot_group <- apply(data_actual[, groups, drop = FALSE], 1, paste, collapse = " + ")
      data_actual$plot_group <- ordered(data_forecast$plot_group, levels = unique(data_forecast$plot_group))

      if (is.null(date_indices)) {

        data_actual$index <- as.numeric(row.names(data_actual))

      } else {
        # To-do: This needs to be more robust. If a user passes in an actual data.frame for
        # plotting historicals and the row.names are re-indexed, the plot will not look right.
        # Good thing we're still at "Experimental" status.
        indices_in_input_dataset <- as.numeric(row.names(data_actual))
        data_actual$index <- date_indices[indices_in_input_dataset]
      }

      if (is.null(groups)) {
        p <- p + geom_line(data = data_actual, aes(x = index, y = eval(parse(text = outcome_names))), color = "grey50")
      } else {
        p <- p + geom_line(data = data_actual, aes(x = index, y = eval(parse(text = outcome_names)),
                                                   color = plot_group, group = plot_group))
      }
    }

    if (all(facet_plot != "")) {
      p <- p + facet_grid(facet_formula)
    }

    p <- p + scale_color_viridis_d()
    p <- p + theme_bw()
    p <- p + xlab("Dataset row / index") + ylab("Outcome") + labs(color = toupper(gsub("_", " ", paste(plot_group, collapse = " + \n")))) +
      ggtitle("N-Step-Ahead Model Forecasts")
    p + theme(legend.position = "none")
    #return(p)
  }
}
