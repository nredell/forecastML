#------------------------------------------------------------------------------
# Temporal example 1: 2 forecasts, daily/monthly, 2 forecast periods at highest aggregation.
freq <- c("1 day", "1 month")

data_1_day <- data.frame("index" = seq(as.Date("2020-1-1"), as.Date("2020-2-29"), by = freq[1]),
                         "forecast" = c(rep(5, 31), rep(7, 29)))

data_1_month <- data.frame("index" = seq(as.Date("2020-1-1"), as.Date("2020-2-1"), by = freq[2]),
                           "forecast" = c(150, 200))

forecasts_reconciled <- reconcile_forecasts(list(data_1_day, data_1_month), freq,
                                            index = "index", outcome = "forecast",
                                            method = "temporal")
#------------------------------------------------------------------------------
# Temporal example 2: 3 forecasts, monthly/4-monthly/annually, 1 forecast period at highest aggregation.
freq <- c("1 month", "4 months", "1 year")

data_1_month <- data.frame("index" = seq(as.Date("2020-1-1"), as.Date("2020-12-1"), by = freq[1]),
                           "forecast" = rep(10, 12))

data_4_months <- data.frame("index" = seq(as.Date("2020-1-1"), as.Date("2020-12-1"), by = freq[2]),
                            "forecast" = c(40, 50, 45))

data_1_year <- data.frame("index" = as.Date("2020-01-01"),
                          "forecast" = c(110))

forecasts_reconciled <- reconcile_forecasts(list(data_1_month, data_4_months, data_1_year), freq,
                                            index = "index", outcome = "forecast",
                                            method = "temporal")
#------------------------------------------------------------------------------
# Temporal example 3: 2 forecasts, weekly/monthly, 2 forecast periods at highest aggregation.
freq <- c("1 week", "1 month")

data_1_week <- data.frame("index" = seq(as.Date("2020-1-1"), as.Date("2020-3-1"), by = freq[1]),
                          "forecast" = c(rep(3, 5), rep(2, 4)))

data_1_month <- data.frame("index" = seq(as.Date("2020-1-1"), as.Date("2020-2-1"), by = freq[2]),
                           "forecast" = c(11, 12))

forecasts_reconciled <- reconcile_forecasts(list(data_1_week, data_1_month), freq,
                                            index = "index", outcome = "forecast",
                                            method = "temporal")
#------------------------------------------------------------------------------
# Temporal example 4: 2 forecasts, hourly/daily, 3 forecast periods at highest aggregation.
freq <- c("1 hour", "1 day")
timezone <- "UTC"

data_1_hour <- data.frame("index" = seq(as.POSIXct("2020-01-01 00:00:00", tz = timezone),
                                        as.POSIXct("2020-01-03 23:00:00", tz = timezone),
                                        by = freq[1]),
                          "forecast" = rep(c(3, 5), 72 / 2))

data_1_day <- data.frame("index" = seq(as.Date("2020-1-1"), as.Date("2020-1-3"), by = freq[2]),
                         "forecast" = c(90, 100, 105))

forecasts_reconciled <- reconcile_forecasts(list(data_1_hour, data_1_day), freq,
                                            index = "index", outcome = "forecast",
                                            method = "temporal")
#------------------------------------------------------------------------------
# Grouped example 1: 2 forecasts, completely nested/hierarchical.
freq <- c("1 month")

dates <- seq(as.Date("2020-1-1"), as.Date("2020-3-1"), by = freq)

data_total <- data.frame("index" = dates,
                         "forecast" = c(50, 100, 75))

data_state <- data.frame("index" = rep(dates, 2),
                         "state" = c(rep("IL", length(dates)), rep("WI", length(dates))),
                         "forecast" = c(20, 60, 40, 25, 40, 50))

forecasts <- list("total" = data_total, "state" = data_state)

forecasts_reconciled <- reconcile_forecasts(forecasts, freq,
                                            index = "index", outcome = "forecast",
                                            method = "group")
#------------------------------------------------------------------------------
# Grouped example 2: 4 forecasts, non-nested.
freq <- c("1 month")

dates <- seq(as.Date("2020-1-1"), as.Date("2020-3-1"), by = freq)

data_total <- data.frame("index" = dates,
                         "forecast" = c(50, 100, 75))

data_state <- data.frame("index" = rep(dates, 2),
                         "state" = c(rep("IL", length(dates)), rep("WI", length(dates))),
                         "forecast" = c(20, 60, 40, 25, 40, 50))

data_sex <- data.frame("index" = rep(dates, 2),
                       "sex" = c(rep("M", length(dates)), rep("F", length(dates))),
                       "forecast" = c(25, 45, 40, 35, 40, 20))

data_state_sex <- data.frame("index" = rep(dates, 4),
                             "state" = c(rep("IL", length(dates)*2), rep("WI", length(dates)*2)),
                             "sex" = c(rep("M", 3), rep("F", 3), rep("M", 3), rep("F", 3)),
                             "forecast" = c(5, 15, 10, 30, 10, 10, 25, 30, 20, 10, 10, 15))

forecasts <- list("total" = data_total, "state" = data_state,
                  "sex" = data_sex, "state_sex" = data_state_sex)

forecasts_reconciled <- reconcile_forecasts(forecasts, freq,
                                            index = "index", outcome = "forecast",
                                            method = "group")
