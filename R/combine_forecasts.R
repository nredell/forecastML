#' Combine multiple horizon-specific forecast models to produce one forecast
#'
#' The horizon-specific models can either be combined to (a) produce final forecasts for only those
#' horizons at which they were trained (i.e., shorter-horizon models override longer-horizon models
#' when producing final short-horizon h-step-ahead forecasts) or (b) produce final forecasts using
#' any combination of horizon-specific models that minimized error over the validation/training dataset or
#' a holdout test dataset.
#'
#' @param ... One or more objects of class 'forecast_results' from running \code{predict.forecast_model()} on
#' an input forward-looking forecast dataset. These are the forecasts from the horizon-specific
#' direct forecasting models trained over the entire training dataset by setting \code{create_windows(..., window_length = 0)}.
#' If \code{type = 'horizon'}, 1 final h-step-ahead forecast is returned for each model passed in \code{...}.
#' @param type A character vector of length 1 that identifies the forecast combination method.
#' @param data_error Not implemented at present.
#' @return An S3 object of class 'forecastML' with final h-step-ahead forecasts.
#'
#'    \strong{Columns in returned 'forecastML' data.frame:}
#'     \itemize{
#'       \item \code{model}: User-supplied model name in \code{train_model()}.
#'       \item \code{model_forecast_horizon}: The direct-forecasting time horizon that the model was trained on.
#'       \item \code{horizon}: Forecast horizons, 1:h, measured in dataset rows.
#'       \item \code{forecast_period}: The forecast period in row indices or dates. The forecast period starts at either \code{attributes(create_lagged_df())$data_stop + 1} for row indices or \code{attributes(create_lagged_df())$data_stop + 1 * frequency} for date indices.
#'       \item \code{"groups"}: If given, the user-supplied groups in \code{create_lagged_df()}.
#'       \item \code{"outcome_name"_pred}: The final forecasts.
#'       \item \code{"outcome_name"_pred_lower}: If given, the lower forecast bounds returned by the user-supplied prediction function.
#'       \item \code{"outcome_name"_pred_upper}: If given, the upper forecast bounds returned by the user-supplied prediction function.
#'    }
#'
#' @section Methods and related functions:
#'
#' The output of \code{combine_forecasts()} has the following generic S3 methods
#'
#' \itemize{
#'   \item \code{\link[=plot.forecastML]{plot}}
#' }
#' @export
combine_forecasts <- function(..., type = c("horizon", "error"), data_error = list(NULL)) {

  data_forecast_list <- list(...)

  if (!all(unlist(lapply(data_forecast_list, function(x) {methods::is(x, "forecast_results")})))) {
    stop("One or more of the forecast datasets given in '...' is not an object of class 'forecast_results'.
         Run predict.forecast_model() on a forward-looking forecast dataset trained over a training dataset
         made with create_windows(window_length = 0).")
  }

  outcome_names <- attributes(data_forecast_list[[1]])$outcome_names
  outcome_levels <- attributes(data_forecast_list[[1]])$outcome_levels
  data_stop <- attributes(data_forecast_list[[1]])$data_stop

  data_forecast <- dplyr::bind_rows(data_forecast_list)  # Collapse the forecast_results list(...).
  data_forecast <- dplyr::as_tibble(data_forecast)
  #----------------------------------------------------------------------------
  # For factor outcomes, is the prediction a factor level or probability?
  if (!is.null(outcome_levels)) {
    factor_level <- if (any(names(data_forecast) %in% paste0(outcome_names, "_pred"))) {TRUE} else {FALSE}
    factor_prob <- !factor_level
  }
  #----------------------------------------------------------------------------

  if (any(unique(data_forecast$window_length) != 0)) {
    stop("Some models were trained using multiple validation windows. Retrain any final forecast models
         using create_windows(window_length = 0) before combining forecast models across horizons.")
  }

  type <- type[1]

  if (!type %in% c("horizon")) {  # List all available types here.
    stop("Select a forecast combination 'type' that is one of 'horizon'; 'error' is not yet supported.")
  }

  if (type == "horizon") {

    # Because different model forecast horizons could be passed in '...'--e.g., model A = 1- and 12-step-
    # ahead models and model B = 3-, 6-, and 9-step ahead models--, we'll combine the horizon-specific
    # forecasts into a singular forecast separately for each model.
    forecast_combination <- lapply(seq_along(data_forecast_list), function(i) {

      data_forecast <- data_forecast_list[[i]]

      model_forecast_horizons <- sort(unique(data_forecast$model_forecast_horizon))
      horizons <- sort(unique(data_forecast$horizon))

      #------------------------------------------------------------------------
      # Create a list of forecast horizons where each horizon-specific model will produce
      # a forecast. This is a greedy selection method where the final forecast is a combination of horizon-specific
      # models--the combination being that longer-term models only contribute forecasts that are not
      # already being contributed by shorter-term models.
      horizon_filter <- lapply(seq_along(model_forecast_horizons), function(i) {

        if (i == 1) {

          if (model_forecast_horizons[i] == 1) {

            x <- 1

          } else {

            x <- seq(1, model_forecast_horizons[i])
          }

        } else if (i < max(seq_along(model_forecast_horizons))) {

          x <- seq(model_forecast_horizons[i - 1] + 1, model_forecast_horizons[i], 1)

        } else {

          x <- seq(model_forecast_horizons[i - 1] + 1, model_forecast_horizons[i], 1)
        }
      })  # End the creation of model-specific forecast combination horizon filter indices.
      #--------------------------------------------------------------------------
      # Filter the results so that short-term forecasts from shorter-term horizon-specific models overwrite
      # short-term forecasts from longer-term horizon-specific models.
      data_forecast <- lapply(seq_along(model_forecast_horizons), function(i) {

        data_forecast[data_forecast$model_forecast_horizon == model_forecast_horizons[i] &
                        data_forecast$horizon %in% horizon_filter[[i]], ]
      })
      data_forecast <- dplyr::bind_rows(data_forecast)
      data_forecast <- dplyr::select(data_forecast, -.data$window_length, -.data$window_number)
      data_forecast <- dplyr::arrange(data_forecast, .data$horizon)
      data_forecast <- as.data.frame(data_forecast)
    })  # End forecast combination for all models given in '...'.
    #--------------------------------------------------------------------------
    data_forecast <- dplyr::bind_rows(forecast_combination)

    attr(data_forecast, "outcome_names") <- outcome_names
    attr(data_forecast, "outcome_levels") <- outcome_levels
    attr(data_forecast, "data_stop") <- data_stop

    class(data_forecast) <- c("forecastML", class(data_forecast))

    return(data_forecast)
  }
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#' Plot an object of class 'forecastML'
#'
#' A forecast plot of h-step-ahead forecasts produced from multiple horizon-specific forecast models
#' using \code{combine_forecasts()}.
#'
#' @param x An object of class 'forecastML' from \code{combine_forecasts()}.
#' @param data_actual A data.frame containing the target/outcome name and any grouping columns.
#' The data can be historical actuals and/or holdout/test data.
#' @param actual_indices Required if \code{data_actual} is given. A vector or 1-column data.frame
#' of numeric row indices or dates (class 'Date' or 'POSIXt') with length \code{nrow(data_actual)}.
#' The data can be historical actuals and/or holdout/test data.
#' @param models Optional. Filter results by user-defined model name from \code{train_model()}.
#' @param group_filter Optional. A string for filtering plot results for grouped time-series (e.g., \code{"group_col_1 == 'A'"});
#' passed to \code{dplyr::filter()} internally.
#' @param drop_facet Optional. Boolean. If actuals are given when forecasting factors, the plot facet with 'actual' data can be dropped.
#' @param ... Not used.
#' @return Forecast plot of class 'ggplot'.
#' @export
plot.forecastML <- function(x, data_actual = NULL, actual_indices = NULL,
                            models = NULL, group_filter = NULL,
                            drop_facet = FALSE, ...) { # nocov start

  if (!methods::is(x, "forecastML")) {
    stop("The 'x' argument takes an object of class 'forecastML' as input. Run combine_forecasts() first.")
  }

  data_forecast <- x
  rm(x)

  outcome_names <- attributes(data_forecast)$outcome_names
  outcome_levels <- attributes(data_forecast)$outcome_levels
  groups <- attributes(data_forecast)$group
  data_stop <- attributes(data_forecast)$data_stop
  #----------------------------------------------------------------------------
  # Remove when functionality is added.
  if (!is.null(groups)) {
    stop("Support for forecast combination plots with groups is still in progress.")
  }
  #----------------------------------------------------------------------------
  # For factor outcomes, is the prediction a factor level or probability?
  if (!is.null(outcome_levels)) {
    factor_level <- if (any(names(data_forecast) %in% paste0(outcome_names, "_pred"))) {TRUE} else {FALSE}
    factor_prob <- !factor_level
  }
  #----------------------------------------------------------------------------
  # Join the actual dataset, if given, to the forecast_results.
  if (!is.null(data_actual)) {

    data_actual <- data_actual[, c(outcome_names, groups), drop = FALSE]

    data_actual$index <- actual_indices

    if (!is.null(group_filter)) {

      data_actual <- dplyr::filter(data_actual, eval(parse(text = group_filter)))
    }
  }
  #----------------------------------------------------------------------------
  # Filter plots using user input.
  models <- if (is.null(models)) {unique(data_forecast$model)} else {models}

  data_forecast <- data_forecast[data_forecast$model %in% models, ]

  if (!is.null(group_filter)) {
    data_forecast <- dplyr::filter(data_forecast, eval(parse(text = group_filter)))
  }
  #----------------------------------------------------------------------------
  # For dimensions that aren't facets, create a grouping variable for ggplot.
    # if (length(unique(data_forecast$window_number)) == 1 && !is.null(groups)) {
    #   plot_group <- c(possible_plot_facets[!possible_plot_facets %in% facet_plot], groups)
    # } else {
    #   plot_group <- c(possible_plot_facets[!possible_plot_facets %in% facet_plot], "window_number", groups)
    # }
    #
    # data_forecast$plot_group <- apply(data_forecast[, plot_group, drop = FALSE], 1, paste, collapse = "-")
    # data_forecast$plot_group <- ordered(data_forecast$plot_group, levels = unique(data_forecast$plot_group))

    #--------------------------------------------------------------------------
    if (is.null(outcome_levels)) {  # Numeric outcome.

      p <- ggplot()

      #------------------------------------------------------------------------
      if (all(horizons == 1)) {  # Use geom_point instead of geom_line to plot a 1-step-ahead forecast.

        # If the plotting data.frame has both lower and upper forecasts plot these bounds.
        if (all(any(grepl("_pred_lower", names(data_forecast))), any(grepl("_pred_upper", names(data_forecast))))) {

          # geom_ribbon() does not work with a single data point when forecast bounds are plotted.
          p <- p + geom_linerange(data = data_forecast,
                                  aes(x = .data$forecast_period, ymin = eval(parse(text = paste0(outcome_names, "_pred_lower"))),
                                      ymax = eval(parse(text = paste0(outcome_names, "_pred_upper"))),
                                      color = .data$model), alpha = .25, size = 3, show.legend = FALSE)

        }

        p <- p + geom_point(data = data_forecast,
                            aes(x = .data$forecast_period, y = eval(parse(text = paste0(outcome_names, "_pred"))),
                                color = .data$model, group = .data$model), show.legend = FALSE)
      }  # End forecast horizon of 1.
      #------------------------------------------------------------------------
      if (!all(horizons == 1)) {  # Plot forecasts for model forecast horizons > 1.

        #----------------------------------------------------------------------------------
        # If the plotting data.frame has both lower and upper forecasts, plot these bounds.
        if (all(any(grepl("_pred_lower", names(data_forecast))), any(grepl("_pred_upper", names(data_forecast))))) {

          # For geom_ribbon(), rows need to be added to the plotting dataset to remove gaps in the colored
          # ribbons so that they touch each other when changing from one model forecast horizon to the next.
          # dplyr::distinct() keep the first distinct row which is the desired behavior.
          data_fill <- dplyr::distinct(data_forecast, .data$model, .data$model_forecast_horizon, .keep_all = TRUE)

          data_fill <- data_forecast %>%
            dplyr::group_by_at(dplyr::vars(.data$model)) %>%
            dplyr::mutate("model_forecast_horizon" = dplyr::lag(.data$model_forecast_horizon, 1)) %>%
            dplyr::filter(!is.na(.data$model_forecast_horizon))

          data_fill <- dplyr::bind_rows(data_forecast, data_fill)

          p <- p + geom_ribbon(data = data_fill,
                                  aes(x = .data$forecast_period, ymin = eval(parse(text = paste0(outcome_names, "_pred_lower"))),
                                      ymax = eval(parse(text = paste0(outcome_names, "_pred_upper"))),
                                      color = ordered(.data$model_forecast_horizon),
                                      fill = ordered(.data$model_forecast_horizon)),
                               linetype = 0, alpha = .25, show.legend = FALSE)
        }  # End plotting lower and upper forecast bounds.
        #----------------------------------------------------------------------

        p <- p + geom_line(data = data_forecast,
                           aes(x = .data$forecast_period, y = eval(parse(text = paste0(outcome_names, "_pred"))),
                               color = ordered(.data$model_forecast_horizon), group = .data$model))

        p <- p + geom_point(data = data_forecast,
                            aes(x = .data$forecast_period, y = eval(parse(text = paste0(outcome_names, "_pred"))),
                                color = ordered(.data$model_forecast_horizon), group = .data$model), color = "black")
      }  # End plot forecasts for model forecast horizons > 1.
      #------------------------------------------------------------------------
      # Add user-defined actuals data to the plots.
      if (!is.null(data_actual)) {

        # data_actual$plot_group <- apply(data_actual[, groups, drop = FALSE], 1, paste, collapse = "-")
        # data_actual$plot_group <- ordered(data_actual$plot_group, levels = unique(data_actual$plot_group))

        if (is.null(groups)) {

          p <- p + geom_line(data = data_actual, aes(x = .data$index,
                                                     y = eval(parse(text = outcome_names))), color = "grey50")
        } else {

          # p <- p + geom_line(data = data_actual, aes(x = .data$index,
          #                                            y = eval(parse(text = outcome_names)),
          #                                            color = .data$plot_group,
          #                                            group = .data$plot_group))
        }
      }  # End plot of user-supplied historcal and/or test set actuals.
      #------------------------------------------------------------------------
      p <- p + scale_color_viridis_d()
      p <- p + facet_wrap(~ model, ncol = 1, scales = "free_y")
      p <- p + theme_bw()
      #--------------------------------------------------------------------------
    } else {  # Factor outcome.

      data_plot <- data_forecast

      # The plot will be faceted by model.
      data_plot$ggplot_color_group <- apply(data_plot[,  c("model", groups), drop = FALSE], 1, function(x) {paste(x, collapse = "-")})

      if (factor_prob) {  # Plot predicted class probabilities.

        # Melt the data for plotting the multiple class probabilities in stacked bars.
        data_plot <- suppressWarnings(tidyr::gather(data_plot, "outcome", "value",
                                                    -!!names(data_plot)[!names(data_plot) %in% c(outcome_levels)]))

        if (is.null(groups)) {

          # The actuals, if given, will be combined with the forecasts in a single data.frame for plotting.
          if (!is.null(data_actual)) {

            # actual or forecast: these are all actuals.
            data_actual$actual_or_forecast <- "actual"
            # historical, test, or model_forecast: these may be any combination of historical data and a holdout test dataset.
            data_actual$time_series_type <- with(data_actual, ifelse(index <= attributes(data_forecast)$data_stop, "historical", "test"))
            names(data_actual)[names(data_actual) == outcome_names] <- "outcome"  # Standardize before concat with forecasts.
            data_actual$ggplot_color_group <- "Actual"  # Actuals will be plotted in the top plot facet.
            data_actual$value <- 1  # Plot a solid bar with probability 1 in geom_col().

            # In cases where historical data is provided in data_actual, duplicate the historical data
            # such that it appears as a sequence in each plot facet. Here, 'model' gives the,
            # possibly user-filtered, plot facets.
            if ("historical" %in% unique(data_actual$time_series_type)) {

              data_hist <- data_actual[data_actual$time_series_type == "historical", c("index", "outcome", "value", "ggplot_color_group")]
              n_rows <- nrow(data_hist)
              data_hist <- data_hist[rep(1:nrow(data_hist), length(unique(data_plot$ggplot_color_group))), ]
              data_hist$ggplot_color_group <- rep(unique(data_plot$ggplot_color_group), each = n_rows)

              data_actual <- suppressWarnings(dplyr::bind_rows(data_hist, data_actual))
            }
          }

          # Standardize names for plotting and before any concatenation with data_actual.
          names(data_plot)[names(data_plot) == "forecast_period"] <- "index"
          data_plot$actual_or_forecast <- "forecast"
          data_plot$time_series_type <- "model_forecast"

          if (!is.null(data_actual)) {
            data_plot <- suppressWarnings(dplyr::bind_rows(data_plot, data_actual))
          }

          data_plot$ggplot_color_group <- factor(data_plot$ggplot_color_group, levels = rev(unique(data_plot$ggplot_color_group)), ordered = TRUE)
          data_plot$value <- as.numeric(data_plot$value)
          data_plot$outcome <- factor(data_plot$outcome, levels = outcome_levels, ordered = TRUE)

          if (drop_facet) {
            data_plot <- data_plot[!grepl("Actual", data_plot$ggplot_color_group), ]
          }

          p <- ggplot()
          p <- p + geom_col(data = data_plot,
                            aes(x = .data$index, y = .data$value, color = .data$outcome, fill = .data$outcome),
                            position = position_stack(reverse = TRUE))
          p <- p + scale_y_continuous(limits = 0:1)
          p <- p + scale_color_viridis_d(drop = FALSE)
          p <- p + scale_fill_viridis_d(drop = FALSE)
          p <- p + facet_wrap(~ ggplot_color_group, ncol = 1, scales = "free_y")
          p <- p + theme_bw()
        }
        #------------------------------------------------------------------------
      } else {  # Plot predicted factor level.

        if (is.null(groups)) {

          if (!is.null(data_actual)) {

            # actual or forecast: these are all actuals.
            data_actual$actual_or_forecast <- "actual"
            # historical, test, or model_forecast: these may be any combination of historical data and a holdout test dataset.
            data_actual$time_series_type <- with(data_actual, ifelse(index <= attributes(data_forecast)$data_stop, "historical", "test"))
            names(data_actual)[names(data_actual) == outcome_names] <- "outcome"  # Standardize before concat with forecasts.
            data_actual$ggplot_color_group <- "Actual"  # Actuals will be plotted in the top plot facet.
            data_actual$value <- 1  # Plot a solid bar with probability 1 in geom_col().

            # In cases where historical data is provided in data_actual, duplicate the historical data
            # such that it appears as a sequence in each plot facet. Here, 'ggplot_color_group' gives the,
            # possibly user-filtered, plot facets.
            if ("historical" %in% unique(data_actual$time_series_type)) {

              data_hist <- data_actual[data_actual$time_series_type == "historical", c("index", "outcome", "value", "ggplot_color_group")]
              n_rows <- nrow(data_hist)
              data_hist <- data_hist[rep(1:nrow(data_hist), length(unique(data_plot$ggplot_color_group))), ]
              data_hist$ggplot_color_group <- rep(unique(data_plot$ggplot_color_group), each = n_rows)

              data_actual <- suppressWarnings(dplyr::bind_rows(data_hist, data_actual))
            }
          }

          # Standardize names for plotting and before any concatenation with data_actual.
          names(data_plot)[names(data_plot) == "forecast_period"] <- "index"
          names(data_plot)[names(data_plot) == paste0(outcome_names, "_pred")] <- "outcome"
          data_plot$value <- 1
          data_plot$actual_or_forecast <- "forecast"
          data_plot$time_series_type <- "model_forecast"

          if (!is.null(data_actual)) {
            data_plot <- suppressWarnings(dplyr::bind_rows(data_plot, data_actual))
          }

          data_plot$ggplot_color_group <- factor(data_plot$ggplot_color_group, levels = rev(unique(data_plot$ggplot_color_group)), ordered = TRUE)
          data_plot$value <- as.numeric(data_plot$value)
          data_plot$outcome <- factor(data_plot$outcome, levels = outcome_levels, ordered = TRUE)

          if (drop_facet) {
            data_plot <- data_plot[!grepl("Actual", data_plot$ggplot_color_group), ]
          }

          p <- ggplot()
          p <- p + geom_col(data = data_plot,
                            aes(x = .data$index, y = .data$value, color = .data$outcome, fill = .data$outcome),
                            position = position_stack(reverse = TRUE))
          p <- p + scale_y_continuous(limits = 0:1)
          p <- p + scale_color_viridis_d(drop = FALSE)
          p <- p + scale_fill_viridis_d(drop = FALSE)
          p <- p + facet_wrap(~ ggplot_color_group, ncol = 1, scales = "free_y")
          p <- p + theme_bw() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
        }
      }  # End factor level prediction plots.
    }  # End numeric and factor outcome plot setup.
    #--------------------------------------------------------------------------
    # Add a vertical line to mark the beginning of the forecast period.
    p <- p + geom_vline(xintercept = data_stop, color = "red")
    #--------------------------------------------------------------------------

    if (is.null(outcome_levels)) {  # Numeric outcome.

      p <- p + xlab("Dataset index") + ylab("Outcome") +
        labs(color = "Direct forecast horizon", fill = "Direct forecast horizon") +
        ggtitle("H-Step-Ahead Model Forecasts")

    } else {  # Factor ouctome.

      p <- p + xlab("Dataset index") + ylab("Outcome") +
        labs(color = "Outcome", fill = "Outcome") +
        ggtitle("H-Step-Ahead Model Forecasts")
    }

    return(suppressWarnings(p))
} # nocov end
