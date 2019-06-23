library(car)

# Simple Linear Regression -------------------------------------------------------

# model fitting and object functions
fit_linear <- lm(weight ~ height, data = women)

fitsum <- summary(fit_linear)                   # model summary
fitted(fit_linear)                              # fitted values
resid(fit_linear)                               # residuals

sqrt(fitsum$r.squared) * 100             # correlation (sqrt of R^2)

# draw plot with fit line
plot(women)                              
abline(fit_linear)


# Polynomial Regression ----------------------------------------------------------

fit_poly <- lm(weight ~ height + I(height ^ 2), data = women)

summary(fit_poly)

plot(women)
lines(women$height, fitted(fit_poly))


fit_poly3 <- lm(weight ~ height + I(height ^ 2) + I(height ^ 3), data = women)

summary(fit_poly3)

plot(women)
lines(women$height, fitted(fit_poly3))


# Multiple Linear Regression ---------------------------------------------------

states <- as.data.frame(
  state.x77[ ,c("Murder", "Population", "Illiteracy", "Income", "Frost")]
)

fit_mult <- lm(
  Murder ~ Population + Illiteracy + Income + Frost,
  data = states
)

summary(fit_mult)

# adding interactions
fit_mult2 <- lm(
  mpg ~ hp + wt + hp:wt,
  data = mtcars
)

summary(fit_mult2)

fit_mult3 <- lm(
  mpg ~ hp * wt,
  data = mtcars
)


# Regression Diagnostics -------------------------------------------------------

states <- as.data.frame(
  state.x77[ ,c("Murder", "Population", "Illiteracy", "Income", "Frost")]
)

fit <- lm(Murder ~ ., data = states)
confint(fit)

# diagnostic plots
par(mfrow = c(2,2))
plot(fit)

fit2 <- lm(weight ~ height + I(height ^ 2), data = women)
plot(fit2)

fit3 <- lm(weight ~ height + I(height ^ 2), data = women[-c(13, 15), ])
plot(fit3)

# `car` functions
qqPlot(
  fit,
  id.method = "identify",
  labels    = rownames(states),
  simulate  = TRUE,
  main      = "Q-Q Plot"
)

hist(rstudent(fit), breaks = 10)

# residual plot
residplot <- function(fit, nbreaks=10) {

  z <- rstudent(fit)

  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")

  rug(jitter(z), col="brown")

  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)

  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)

  legend("topright",
         legend = c("Normal", "Kernel"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

# durbin watson
durbinWatsonTest(fit)

# linearity: component+residual plot
crPlots(fit)

# homoscedasticity
ncvTest(fit)
spreadLevelPlot(fit)

# muticollinearity
vif(sqrt(fit))
