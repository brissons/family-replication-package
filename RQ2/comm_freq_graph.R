library(extrafont)
library(Hmisc)

#loading fonts
loadfonts(device = "win")

#no scientific notation
options(scipen = 5)

#load and process data
freq <-
  read.csv(
    "comm_freq.csv",
    header = TRUE,
    sep = ",",
    dec = ".",
    fileEncoding = "utf-8"
  )
freq_m <- as.matrix(freq)

#create communication frequency graph
par(
  lwd = 1,
  mar = c(7.75, 5.75, 0.5, 6.5),
  family = "Linux Libertine G",
  mgp = c(4.5, 0.65, 0),
  cex = 0.8,
  cex.axis = 1,
  font.axis = 1
)
frequency_barplot <-
  barplot(
    freq_m,
    las = 2,
    log = "y",
    beside = TRUE,
    ylab = expression(bold("Count (log)")),
    ylim = c(1, 1000000),
    col = c("#0072BD", "#Dc642f", "#EDB120"),
    xaxt = "n",
    yaxt = "n"
  )
title(xlab = expression(bold("Metrics")), line = 6.75)
legend(
  x = 57,
  y = 1000000,
  c(expression(bold("Repository")), expression(bold("Family")), expression(bold("Outside"))),
  fill = c("#0072BD", "#Dc642f", "#EDB120"),
  xpd = "TRUE"
)
labs <-
  c(
    "",
    "Issues",
    "",
    "",
    "",
    "Issue Comments",
    "",
    "",
    "",
    "Issue Mentions",
    "",
    "",
    "",
    "Issue Subscribed",
    "",
    "",
    "",
    "Issue Unsubscribed"
    ,
    "",
    "",
    "",
    "Issue Closed",
    "",
    "",
    "",
    "Issue Reopened",
    "",
    "",
    "",
    "Issue Referenced",
    "",
    "",
    "",
    "Issue Assigned",
    "",
    "",
    "",
    "PRs",
    "",
    "",
    "",
    "PR Comments",
    "",
    "",
    "",
    "PR Mentions",
    "",
    "",
    "",
    "Followers",
    "",
    "",
    "",
    "Users"
  )
text(
  seq(54),
  log(length(frequency_barplot)) - 3.7,
  adj = 1,
  labels = labs,
  xpd = TRUE,
  srt = 55
)
axis(
  2,
  at = c(1, 100, 10000, 1000000) ,
  labels = formatC(
    c(1, 100, 10000, 1000000),
    big.mark = ",",
    format = "d"
  ),
  las = 2
)
