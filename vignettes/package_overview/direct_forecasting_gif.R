
library(datasets, lib.loc = "C:/Users/redelln/R/win-library/3.4")
library(ggplot2, lib.loc = "C:/Users/redelln/R/win-library/3.4")
library(magick, lib.loc = "C:/Users/redelln/R/win-library/3.4")

data <- as.data.frame(datasets::Seatbelts)
data <- data[, c("DriversKilled", "kms", "PetrolPrice", "law")]

horizon <- 12

data$index <- 1:nrow(data)

data$fill <- NA
data$fill[(nrow(data_train) - horizon):(nrow(data_train) - 1)] <- 1:horizon
data$fill[(nrow(data_train)):(nrow(data_train) + 11)] <- 1:horizon
data$start <- data$fill

data$fill <- ordered(data$start)

data$start[!is.na(data$start)] <- data$index[!is.na(data$start)]
data$stop[!is.na(data$start)] <- data$index[!is.na(data$start)] + 1

# Add forecasts with uniform error.
set.seed(224)
data$forecast <- data$DriversKilled + data$DriversKilled * runif(nrow(data), -.1, .1)
data$forecast[data$index <= (nrow(data_train) - 1)] <- NA

# Remove rows for a cleaner plot.
data <- data[-(1:150), ]

# Create the list of fill datasets for the GIF.
data_fill <- lapply(1:horizon, function(i) {
  data[data$fill <= i, ]
})

data_fill[[1]]

img <- magick::image_graph(res = 96)

lapply(data_fill, function(x) {
  p <- ggplot(data, aes(x = index, y = DriversKilled))
  p <- p + geom_rect(data = x, aes(xmin = start, xmax = stop, ymin = -Inf, ymax = Inf, fill = fill),
                     alpha = 0.75, show.legend = FALSE)
  p <- p + geom_line()
  p <- p + geom_point(data = x, aes(x = index + .5, y = forecast), color = "black", size = 2)
  p <- p + geom_point(data = x, aes(x = index + .5, y = forecast), color = "white", size = 1)
  p <- p + geom_vline(xintercept = nrow(data_train), color = "red", size = 1.1)
  p <- p + theme_bw() + xlab("Time") + ylab("Outcome") + ggtitle("Direct Forecasting Illustration")
  p
})

animation <- magick::image_animate(img, fps = 1)
magick::image_write(animation, "direct_forecasting.gif")
