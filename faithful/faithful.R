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
library(ggplot2)
boxplot <- ggplot(faithful) +
  aes(x = day, y = waiting, group = day) +
  geom_boxplot(fill = "orange") +
  labs(x = "Day", y = "Waiting time between successive eruptions / min") +
  theme_bw()
print(boxplot)

# 2 Histograms and kernel density plots

# number of bins for histogram
bins <- c(10, 25, 50)

# histogram of waiting time with density plot
library(ggpubr)

histograms <- lapply(
  bins,
  function(b) {
    ggplot(faithful) +
      aes(x = waiting) +
      geom_histogram(
        aes(y = after_stat(density)),
        binwidth = (max(waiting) - min(waiting) + 1) / b,
        fill = "#0088ff",
        colour = "black"
      ) +
      geom_density(
        lwd = 2,
        colour = "red"
      ) +
      labs(x = "Waiting time / min") +
      theme_bw()
  }
)

histogram <- ggarrange(plotlist = histograms, ncol = 3)
print(histogram)

# density plot of waiting time with different bandwidths
colours <- c("bw = default" = "red", "bw = 2" = "green", "bw = 0.75" = "blue")

densities <- ggplot(faithful) +
  aes(x = waiting, color = "bw = default") +
  geom_density(
    lwd = 2,
  ) +
  geom_density(
    aes(x = waiting, color = "bw = 2"),
    bw = 2,
    lwd = 2,
    linetype = "dashed",
  ) +
  geom_density(
    aes(x = waiting, color = "bw = 0.75"),
    bw = 0.75,
    lwd = 2,
    linetype = "dotted",
  ) +
  labs(x = "Waiting time / min", color = "Legend") +
  theme_bw() +
  scale_color_manual(values = colours)
print(densities)

# 3 Plotting consecutive eruption waiting times

# x-axis limits: 0 to max number of eruptions in a day
xlim <- c(0, max(table(day)))
# y-axis limits: min to max waiting time
ylim <- c(min(waiting), max(waiting))

scatters <- lapply(
  1:15,
  function(i) {
    select <- which(day == i)
    ggplot(faithful[select, ]) +
      aes(x = seq_along(waiting), y = waiting) +
      geom_line(colour = "blue") +
      labs(
        title = paste("Day", i),
        x = "Eruption number",
        y = "Waiting time / min"
      ) +
      theme_bw() +
      coord_cartesian(xlim = xlim, ylim = ylim)
  }
)

scatter <- ggarrange(plotlist = scatters, ncol = 5, nrow = 3)
print(scatter)

# 4 Scatterplots and linear regression

n <- length(waiting)
library(tsutils)
lagduration <- lagmatrix(duration, 1)

# scatterplot of waiting time against lagged duration
scatterplot <- ggplot(faithful) +
  aes(x = lagduration, y = waiting) +
  geom_point(
    colour = "orange",
  ) +
  labs(x = "Duration of previous eruption / min", y = "Waiting time / min") +
  theme_bw() +
  # linear regression of waiting time against lagged duration
  geom_smooth(method = "lm", se = FALSE)
print(scatterplot)

# 5 K-means clustering

k <- 2
c <- kmeans(cbind(lagduration[2:n], waiting[2:n]), k)
lagdata <- data.frame(lagduration = lagduration[2:n], waiting = waiting[2:n])
kmeans <- ggplot(lagdata) +
  aes(x = lagduration, y = waiting, colour = factor(c$cluster)) +
  geom_point() +
  labs(
    x = "Duration of previous eruption / min",
    y = "Waiting time / min",
    colour = "Cluster"
  ) +
  theme_bw()
print(kmeans)

# different number of clusters
k <- 3
c <- kmeans(cbind(lagduration[2:n], waiting[2:n]), k)
kmeans <- ggplot(lagdata) +
  aes(x = lagduration, y = waiting, colour = factor(c$cluster)) +
  geom_point() +
  labs(
    x = "Duration of previous eruption / min",
    y = "Waiting time / min",
    colour = "Cluster"
  ) +
  theme_bw()
print(kmeans)

# lagged waiting time against waiting time
lagwaiting <- lagmatrix(waiting, 1)
k <- 2
c <- kmeans(cbind(lagwaiting[2:n], waiting[2:n]), k)
lagdata <- data.frame(lagwaiting = lagwaiting[2:n], waiting = waiting[2:n])
kmeans <- ggplot(lagdata) +
  aes(x = lagwaiting, y = waiting, colour = factor(c$cluster)) +
  geom_point() +
  labs(
    x = "Waiting time of previous eruption / min",
    y = "Waiting time / min",
    colour = "Cluster"
  ) +
  theme_bw()
print(kmeans)
