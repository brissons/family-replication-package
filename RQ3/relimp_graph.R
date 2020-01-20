library(extrafont)
library(Hmisc)

#loading fonts
loadfonts(device = "win")

#load and process data
relimp <-
  read.csv(
    "relimp_results.csv",
    header = TRUE,
    sep = ",",
    dec = ".",
    fileEncoding = "utf-8"
  )
relimp_m <- as.matrix(relimp)

#create relative importance graph
par(
  lwd = 1,
  mar = c(4, 10, 0.25, 0.5),
  family = "Linux Libertine G",
  mgp = c(9, 0.75, 0.25),
  cex = 0.8,
  cex.axis = 1,
  font.axis = 1
)
pal <- colorRampPalette(colors = c("#Dc642f", "#edb197"))(4)
barplot(
  relimp_m[c(3, 1, 4, 2)] * 100,
  las = 1,
  col = pal,
  horiz = TRUE,
  space = 0.3,
  xlim = c(0, 35),
  ylab = expression(bold("Category")),
  names.arg = c("Family", "Non-communicative", "Outside", "Repository")
)
title(xlab = expression(bold("% of Response Variance")), line = 3)
