library(car)
library(hexbin)
library(rgl)
library(RColorBrewer)

# Scatter Plots -----------------------------------------------------------

# basic scatter
with(mtcars, {
  plot(wt,mpg)
  abline(
    lm(mpg ~ wt),
    lwd = 2,
    lty = 2,
    col = "red")
  lines(
    lowess(wt, mpg),
    col = "blue",
    lwd = 2,
    lty = 2)})

# car scatter
scatterplot(
  mpg ~ wt | cyl,
  data = mtcars,
  main = "Scatterplot!")

# scatterplot matrices
pairs( ~ mpg + disp + drat + wt, data = mtcars, upper.panel = NULL)

scatterplotMatrix(
  ~ mpg + disp + drat + wt,
  data = mtcars,
  smoother.args = list(lty = 2, col = "tomato"),
  main = "`car` Scatterplot Matrix")

# overplotted scatterplots
set.seed(1234)
n <- 10000
c1 <- matrix(rnorm(n, 0, 0.5), ncol = 2)
c2 <- matrix(rnorm(n, 3,   2), ncol = 2)
mydata <- as.data.frame(rbind(c1, c2))
names(mydata) <- c("x", "y")

# smooth scatter (base)
with(
  mydata,
  smoothScatter(
    x, y,
    main = "Overplotting!"))

# hexbin
with(
  mydata, {
    bin <- hexbin(x , y, xbins = 50)
    plot(bin, main = "Hex-Bin!")})

# 3D scatterplot - scatterplot3d
with(
  mtcars, {
  scatterplot3d::scatterplot3d(
    wt, disp, mpg,
    type = "h",
    highlight.3d = TRUE,
    main = "3D Scatterplot!",
    sub = "... With regression planes and everything.") -> s3d
  lmmod <- lm(mpg ~ wt + disp, data = mtcars)
  s3d$plane3d(lmmod)}) # add a regression plane

# 3D scatterplot - rgl
with(
  mtcars,
  plot3d(wt, disp, mpg, col = "red", size = 5))

# bubble plots
with(
  mtcars, {
    r <- sqrt(disp / pi)
    symbols(
      wt, mpg, circles = r,
      inches = 0.3,
      fg = "white",
      bg = "lightblue")
    text(wt, mpg, labels = rownames(mtcars), cex = 0.6)
  })


# Line Charts -------------------------------------------------------------

# connecting the dots
par_default <- par(no.readonly = TRUE)
par(mfrow = c(1,2))

t1 <- subset(Orange, Tree == 1)

plot(t1$age, t1$circumference)
plot(t1$age, t1$circumference, type = "b")

par(par_default)

# NOTE: plot() creates a new plot, while lines() adds a line to an
#       existing plot

# complex example
Orange$Tree <- as.numeric(Orange$Tree)
ntrees <- max(Orange$Tree)

xrange <- range(Orange$age)
yrange <- range(Orange$circumference)

plot(
  xrange, yrange,
  type = "n",
  xlab = "Age (days)",
  ylab = "Circumference (mm)")

colors <- brewer.pal(ntrees, "Set2")
linetype <- c(1:ntrees)
plotchar <- seq(18, 18 + ntrees, 1)

for (i in 1:ntrees) {
  tree <- subset(Orange, Tree == i)
  lines(
    tree$age, tree$circumference,
    type = "b",
    lwd = 2,
    lty = linetype[i],
    col = colors[i],
    pch = plotchar[i])
}

title("Tree Growth")

legend(
  xrange[1], yrange[2],
  1:ntrees,
  col = colors,
  pch = plotchar,
  lty = linetype,
  title = "Tree")
