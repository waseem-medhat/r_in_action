library(vcd)
library(gplots)

# Bar Plots ---------------------------------------------------------------

# feeding counts into barplot()
counts <- table(Arthritis$Improved)
barplot(counts, horiz = TRUE)

# plotting factors directly
plot(Arthritis$Improved, horiz = TRUE)

# stacked/grouped bars
counts_2w <- table(Arthritis$Improved, Arthritis$Treatment)
barplot(counts_2w, horiz = TRUE, beside = TRUE)

# example 6.2
barplot(
  counts_2w,
  horiz = TRUE,
  beside = TRUE,
  col = c("tomato2", "darkgoldenrod2", "steelblue"),
  legend = rownames(counts_2w)
)

# bar plot for means
states <- data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, by = list(state.region), FUN = mean)
means <- means[order(means$x), ]

barplot(means$x, names.arg = means$Group.1)

# add confidence intervals to the bar
barplot2(
  means$x,
  names.arg = means$Group.1,
  horiz = TRUE,
  plot.ci = TRUE,
  ci.l = (means$x - 0.25),
  ci.u = (means$x + 0.25)
)

# customize bar chart - example 6.4
par(mar = c(5, 8, 4, 2)) # margins
par(las = 2)             # rotates labels
counts <- table(Arthritis$Improved)
barplot(
  counts,
  horiz = TRUE,
  cex.names = 0.8, # label size
  names.arg = c(   # label text
    "No Improvement",
    "Some Improvement",
    "Marked Improvement"
  ),
  main = "Treatment Outcoumes"
)

# spinogram
spine(t(counts_2w))


# Pie Charts --------------------------------------------------------------


