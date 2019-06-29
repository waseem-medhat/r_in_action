library(multcomp)
library(gplots)
library(car)

## NOTE: Anova() in the `car` library can perform type II or III SS
##       unlike base R's aov() which uses only type I

# One-Way ANOVA -----------------------------------------------------------

# get group summaries
with(
  cholesterol,
  by(response, list(trt), summary)
)

# anova test
fit <- aov(response ~ trt, data = cholesterol)
summary(fit)

# means plot
plotmeans(response ~ trt, data = cholesterol)

# multiple comparisons: tukey's hsd
TukeyHSD(fit)

# plot comparisons
par(las = 2)
par(mar = c(5, 8, 4, 2))
plot(TukeyHSD(fit))

# multcomp methods
tuk <- glht(fit, linfct = mcp(trt = "Tukey"))
par(mar = c(5, 4, 6, 2))
plot(cld(tuk, level = 0.05), col = "steelblue4")

# assumptions - normal residuals
qqPlot(lm(response ~ trt, data = cholesterol), simulate = TRUE)

# assumptions - homoscedasticity
bartlett.test(response ~ trt, data = cholesterol)

# outliers
outlierTest(fit)
