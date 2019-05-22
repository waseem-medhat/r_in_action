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

plot(dose, drugA, type = 'b',
     main = "some title",
     ) # b for BOTH lines and points
  
# add font families
windowsFonts(
  Hack = "Hack",
  ohno = "Comic Sans MS"
)
