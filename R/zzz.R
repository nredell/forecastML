
# Utility function for plotting a rescaled versin of the outcome time series as a background graphic in various plots.
plot_outcome <- function(data_melt, data_outcome, join = c("model", "horizon", "window_length", "window_number"),
                         scale_outcome_by = NULL, facet_plot = c("model", "horizon", "window_length"),
                         outcome_names = NULL) {

  data_outcome <- dplyr::left_join(data_outcome, data_melt, by = join)

  data_outcome <- dplyr::distinct(data_outcome, valid_indices, !!!dplyr::syms(facet_plot),
                                  !!dplyr::sym(scale_outcome_by), .keep_all = TRUE)

  data_outcome <- data_outcome %>%
    #dplyr::group_by(!!!dplyr::syms(facet_plot), !!dplyr::sym(scale_outcome_by)) %>%
    dplyr::group_by(!!dplyr::sym(scale_outcome_by)) %>%
    dplyr::mutate("min_scale" = min(as.numeric(value), na.rm = TRUE),
                  "max_scale" = max(as.numeric(value), na.rm = TRUE)) %>%
    dplyr::ungroup()

  data_outcome <- as.data.frame(data_outcome)
  data_outcome$min_scale <- as.numeric(data_outcome$min_scale)
  data_outcome$max_scale <- as.numeric(data_outcome$max_scale)

  min_outcome <- min(data_outcome[, outcome_names, drop = TRUE], na.rm = TRUE)
  max_outcome <- max(data_outcome[, outcome_names, drop = TRUE], na.rm = TRUE)

  data_outcome$outcome_scaled <- (((data_outcome[["max_scale"]] - data_outcome[["min_scale"]]) * (data_outcome[, outcome_names, drop = TRUE] - min_outcome)) /
                                    (max_outcome - min_outcome)) + data_outcome$min_scale

  return(data_outcome)
}
#
# data_outcome <- plot_outcome(data_hyper_num, data_results, scale_outcome_by = "hyper",
#                              join = c("model", "horizon", "window_length", "window_number"),
#                              outcome_names = outcome_names)
#
#
# data_melt <- data_hyper_num
# data_outcome <- data_results
# scale_outcome_by <- "hyper"
# join <- c("model", "horizon", "window_length", "window_number")
#



# data_outcome <- backtestR:::plot_outcome(data_plot, data_results, scale_outcome_by = "error_metric",
#                                          outcome_names = outcome_names)
#
# p <- ggplot()
# p <- p + geom_line(data = data_plot, aes(x = window_midpoint, y = value, color = group, group = group), size = 1.05, alpha = .50)
# p <- p + geom_point(data = data_plot, aes(x = window_midpoint, y = value, color = group, group = group))
# p <- p + geom_line(data = data_outcome, aes(x = valid_indices, y = outcome_scaled), color = "gray50")
#
# p <- p + facet_grid(error_metric ~ window_length, scales = "free")
# p <- p + theme_bw()
# p <- p + xlab("Dataset row / index") + ylab("Forecast error metric") + labs(color = "Model - Horizon") +
#   ggtitle("Forecast Error Metrics at Validation Window Midpoint - Faceted by window length and error metric")
# p
