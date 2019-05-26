library(RColorBrewer)

# Basic Plot and Output ---------------------------------------------------

# direct graphics to pdf instead of R output
pdf('first_plot.pdf', family = "Helvetica-Narrow")

  plot(mtcars$wt, mtcars$mpg)
  abline(lm(mpg ~ wt, data = mtcars))
  title("Regression of MPG on Weight")

dev.off() # turn off; graphics are directed to R output


# Basic Example -----------------------------------------------------------

dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)

plot(dose, drugA, type = 'b') # b for BOTH lines and points


# Parameters --------------------------------------------------------------

default <- par(no.readonly = TRUE) # save defaults

par(lty = 2, pch = 17) # dashed line; triangular points
plot(dose, drugA, type = 'b')

par(default) # restore defaults

# use parameters inside plot()
plot(dose, drugA, type = 'b', 
     lty = 3, pch = 18, cex = 2, lwd = 3)

# lty = line type
# lwd = line width
# pch = point symbol
# cex = point size

# colors
n <- 5
thecolors <- brewer.pal(n, "Accent")
pie(rep(1, n), labels = thecolors, col = thecolors)

# text
par(
  cex       = 1,   # scale
  cex.lab   = 1.5,
  cex.main  = 2,
  font.lab  = 1,
  font.main = 3,
  family    = "sans"
)

  
# add font families
windowsFonts(
  Hack = "Hack",
  ohno = "Comic Sans MS"
)

# margins
par(pin=c(5,5), mai=c(1,.5, 1, .2))

plot(dose, drugA, type = 'b',
     main = "some title",
     ) # b for BOTH lines and points

par(default)


# Text, Axes, and Legends -------------------------------------------------

plot(
  dose, drugA, type="b",
  col="red", lty=2, pch=2, lwd=2,
  # main="Clinical Trials for Drug A",
  # sub="This is hypothetical data",
  # xlab="Dosage",
  # ylab="Drug Response",
  xlim=c(0, 60),
  ylim=c(0, 70),
  ann = FALSE,
  # axes = FALSE, # suppress all axes
  frame.plot = TRUE, # if axes are suppressed keep the frame
  xaxt = "n" # suppress x axis
)

title(
  main = "Main Title",
  col.main = "tomato",
  sub  = "Subtitle",
  xlab = "X label",
  ylab = "Y label"
)

# example 3.2

x <- 1:10
y <- x
z <- 10/x

default <- par(no.readonly = TRUE) # save defaults

par(mar = c(5, 4, 4, 8) + 0.1)

plot(
  x, y, type = 'b', pch  = 21, col  = 'red',
  yaxt = 'n', lty  = 3, ann  = FALSE
)

lines(
  x, z, type = 'b', pch  = 22,
  col  = 'blue', lty  = 2
)

axis(
  2, at = x, labels = x, col.axis = 'red', las = 2
)

axis(
  4, at = z, labels = round(z, 2), col.axis = 'blue',
  las = 2, cex.axis = 0.7, tck = -0.01
)

mtext(
  "y = 1 / x", side = 4, line = 3,
  cex.lab = 1, las = 2, col = 'blue'
)

title(
  "An Example of Axes", xlab = "X", ylab = "Y = X"
)

par(default)

abline(h = c(1, 5, 7), v = c(1, 2))

# # #

# example 3.3

# data
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)

# save default parameters
# default <- par(no.readonly = TRUE) # save defaults

# change parameters
par(lwd = 2, cex = 1.5, font.lab = 2)

# main plot w/ 1st line
plot(
  dose, drugA, type = 'b',
  pch = 15, lty = 1, col = 'red', ylim = c(0, 60),
  main = "Drug A vs. Drug B",
  xlab = "Drug Dose",
  ylab = "Drug Response"
)

# 2nd line
lines(
  dose, drugB, type = 'b',
  pch = 17, lty = 2, col = 'blue'
)

# ref line
abline(
  h = 30,
  lwd = 1.5, lty = 2, col = 'gray'
)

# minor tick marks
Hmisc::minor.tick()

# legend
legend(
  "topleft", inset = 0.05, title = "Drug Type", c("A", "B"),
  lty = c(1, 2), pch = c(15, 17), box.lwd = 0.5, col = c("red", "blue")
)

# reset to default parameters
par(default)

# # #

# annotations
plot(
  mtcars$wt, mtcars$mpg,
  pch = 18, col = "blue",
  main = "Mileage vs. Car Weight",
  xlab = "Weight",
  ylab = "Mileage"
)

text(
  mtcars$wt, mtcars$mpg,
  row.names(mtcars),
  cex = 0.6, pos = 4, col = "gray"
)

# fixed annotations
par(cex=1.5)
plot(1:7,1:7,type="n")
text(3,3,"Example of default text")
text(4,4,family="mono","Example of mono-spaced text")
text(5,5,family="serif","Example of serif text")
par(default)

# math annotations
demo(plotmath)


# Layouts -----------------------------------------------------------------

# arrange plots in r*c matrix
par(mfrow = c(3, 1)) # mfrow fills by row; mfcol by column
hist(mtcars$wt)
hist(mtcars$mpg)
hist(mtcars$disp)

# more complex layout
layout(
  matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE),
  widths = c(2, 1), heights = c(1, 2) # relative widths/heights of tiles
)
hist(mtcars$wt)
hist(mtcars$mpg)
hist(mtcars$disp)


# example 3.4

# save default parameters
# default <- par(no.readonly = TRUE) # save defaults

{
  # main plot
  par(fig = c(0, 0.8, 0, 0.8))
  plot(
    mtcars$wt, mtcars$mpg,
    ylab = "Miles per Gallon",
    xlab = "Car Weight"
  )
  
  # boxplot above
  par(fig = c(0, 0.8, 0.55, 1), new = TRUE)
  boxplot(mtcars$wt, horizontal = TRUE, axes = FALSE)
  
  # boxplot to the right
  par(fig = c(0.65, 1, 0, 0.8), new = TRUE)
  boxplot(mtcars$wt, axes = FALSE)
  
  # title
  mtext("Enhanced Scatterplot", outer = TRUE,
        line = -5, cex = 3, family = "Hack")
  
  # reset parameters
  par(default)
}
