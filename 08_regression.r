
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

