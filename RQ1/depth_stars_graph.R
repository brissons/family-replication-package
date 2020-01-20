library(extrafont)
library(Hmisc)

#loading fonts
loadfonts(device = "win")

#no scientific notation
options(scipen = 5)

#load and process data
depth <-
  read.csv(
    "depth_stars.csv",
    header = TRUE,
    sep = ",",
    dec = ".",
    fileEncoding = "utf-8"
  )
depth_m <- as.matrix(depth)

#create depth/stars frequency graph
par(
  lwd = 1,
  mar = c(3.2, 5.5, 0.5, 7),
  family = "Linux Libertine G",
  mgp = c(4.5, 0.65, 0),
  cex = 0.8,
  cex.axis = 1,
  font.axis = 1
)
mgp.axis.labels(c(0, 0, 0), type = 'y')
barplot(
  depth_m,
  beside = TRUE,
  log = "y",
  col = c("#0072BD", "#EDB120"),
  yaxt = "n",
  ylim = c(1, 1000000),
  las = 1,
  xlab = "",
  ylab = expression(bold("Count (log)")),
  names.arg = c("0", "1", "2", "3", "4", "5"),
  space = c(0.5, 0, 0.5, 0, 0.5, 0, 0.5, 0, 0.5, 0, 0.5, 0)
)
title(xlab = expression(bold("Depth")), line = 2.2)
legend(
  x = 15,
  y = 1000000,
  c(expression(bold("Repositories")), expression(bold("Stars"))),
  fill = c("#0072BD", "#EDB120"),
  xpd = "TRUE"
)
axis(
  2,
  at = c(1, 100, 10000, 1000000),
  labels = formatC(
    c(1, 100, 10000, 1000000),
    big.mark = ",",
    format = "d"
  ),
  las = 2
)