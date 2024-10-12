# import faithful.csv data
library(readr)
faithful <- read_csv("faithful.csv",
  col_types = cols(
    waiting = col_integer(),
    duration = col_double(),
    day = col_integer()
  )
)

# waiting time between successive eruptions
waiting <- faithful$waiting
# duration of the eruption
duration <- faithful$duration
# day recorded
day <- faithful$day

# 1 Importing data and summary statistics

# summary statistics of waiting time
summary(waiting)
# summary statistics of duration
summary(duration)

# boxplot of waiting time against day
boxplot(waiting ~ day,
  xlab = "Day",
  ylab = "Waiting time between successive eruptions / min",
  col = "orange"
)

# 2 Histograms and kernel density plots

# number of bins for histogram
bins <- c(10, 25, 50)

# histogram of waiting time with density plot
par(mfrow = c(1, 3))
for (i in 1:3) {
  hist(waiting,
    breaks = bins[i],
    col = "#0088ff",
    freq = FALSE,
    xlab = "Waiting time / min",
  )
  lines(density(waiting),
    lwd = 2,
    col = "red"
  )
}

# density plot of waiting time with different bandwidths
par(mfrow = c(1, 1))
plot(density(waiting),
  lwd = 2, col = "red",
  xlab = "Waiting time / min"
)
lines(density(waiting, bw = 2),
  lwd = 2, lty = 2, col = "green"
)
lines(density(waiting, bw = 0.75),
  lwd = 2, lty = 3, col = "blue"
)
legend("topright",
  legend = c("bw = default", "bw = 2", "bw = 0.75"),
  lty = 1:3,
  lwd = 2,
  col = c("red", "green", "blue")
)

# 3 Plotting consecutive eruption waiting times

par(mfrow = c(3, 5))

# x-axis limits: 0 to max number of eruptions in a day
xlim <- c(0, max(table(day)))
# y-axis limits: min to max waiting time
ylim <- c(min(waiting), max(waiting))

for (i in 1:15) {
  select <- which(day == i)
  plot(waiting[select],
    type = "l",
    title(main = paste("Day", i)),
    xlab = "Eruption number",
    xlim = xlim,
    ylab = "Waiting time / min",
    ylim = ylim,
    col = "blue"
  )
}

# 4 Scatterplots and linear regression

n <- length(waiting)
library(tsutils)
lagduration <- lagmatrix(duration, 1)

# scatterplot of waiting time against lagged duration
par(mfrow = c(1, 1))
plot(lagduration, waiting,
  pch = 19,
  xlab = "Duration of previous eruption / min",
  ylab = "Waiting time / min",
  col = "orange"
)

# linear regression of waiting time against lagged duration
model <- lm(waiting ~ lagduration)
abline(model, col = "red", lwd = 2)

# 5 K-means clustering

k <- 2
c <- kmeans(cbind(lagduration[2:n], waiting[2:n]), k)
plot(lagduration[2:n], waiting[2:n],
  pch = 19,
  xlab = "Duration of previous eruption / min",
  ylab = "Waiting time / min",
  col = as.factor(c$cluster)
)

# different number of clusters
k <- 3
c <- kmeans(cbind(lagduration[2:n], waiting[2:n]), k)
plot(lagduration[2:n], waiting[2:n],
  pch = 19,
  xlab = "Duration of previous eruption / min",
  ylab = "Waiting time / min",
  col = as.factor(c$cluster)
)

# lagged waiting time against waiting time
lagwaiting <- lagmatrix(waiting, 1)
k <- 2
c <- kmeans(cbind(lagwaiting[2:n], waiting[2:n]), k)
plot(lagwaiting[2:n], waiting[2:n],
  pch = 19,
  xlab = "Waiting time of previous eruption / min",
  ylab = "Waiting time / min",
  col = as.factor(c$cluster)
)
