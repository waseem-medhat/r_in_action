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
