library(dplyr)
library(multcomp)
library(gplots)
library(car)
library(effects)
library(HH)
library(MASS)

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


# One-Way ANCOVA ----------------------------------------------------------

head(litter)
table(litter$dose)

# summaries
litter %>%
  group_by(dose) %>%
  summarize(
    n    = n(),
    mean = mean(weight),
    sd   = sd(weight))

# ancova fitting
# dose controlled for gesttime
fit <- aov(weight ~ gesttime + dose, data = litter)
summary(fit)

# adjusted group means
effect(term = "dose", mod = fit)

# multiple comparisons
contrast <- rbind("no drug vs. drug" = c(3, -1, -1, -1))
summary(glht(fit, linfct = mcp(dose = contrast)))

# homogeneity of regression slopes
summary(aov(weight ~ gesttime * dose, data = litter))

# visualizing ancova
ancova(weight ~ gesttime + dose, data = litter)
ancova(weight ~ gesttime * dose, data = litter)


# Two-Way Factorial ANOVA -------------------------------------------------

head(ToothGrowth)

tooth <- ToothGrowth
tooth$dose <- factor(tooth$dose)

## NOTE: convert the grouping variable to a factor, otherwise it would
##       be considered as a numeric covariate

# summaries
tooth %>%
  group_by(supp, dose) %>%
  summarize(
    n    = n(),
    mean = mean(len),
    sd   = sd(len))

# anova
fit <- aov(len ~ supp * dose, data = tooth)
summary(fit)

# interaction plot - base
with(
  tooth,
  interaction.plot(
    dose, supp, len,
    type = "b",
    col  = c("tomato", "steelblue"),
    pch  = c(16, 18),
    main = "Interaction Plot between Dose and Supplement Type"))
  
# interaction plot - gplots
with(
  tooth,
  plotmeans(
    len ~ interaction(supp, dose, sep = " "),
    connect = list(c(1, 3, 5), c (2, 4, 6)),
    col = c("darkgreen", "darkgoldenrod"),
    main = "Interaction Plot with 95% CIs"))

# interaction plot - HH
with(tooth, interaction2wt(len ~ supp * dose))


# Repeated Measures ANOVA -------------------------------------------------

# prep
head(CO2)
co2 <- CO2
co2$conc <- factor(co2$conc)

co2ch <- subset(
  co2,
  Treatment == "chilled",
  select = c("Plant", "Type", "conc", "uptake"))

# fit
fit <- aov(
  uptake ~ conc * Type + Error(Plant / (conc)),
  data = co2ch)
summary(fit)

# interaction plot
par(las = 2)
par(mar = c(10, 4, 4, 2))
with(
  co2ch,
  interaction.plot(
    conc, Type, uptake,
    type = "b",
    col  = c("tomato2", "steelblue"),
    pch  = c(16, 18),
    main = "Interaction Plot for Plant Type and Concentration"))

boxplot(
  uptake ~ Type * conc,
  data = co2ch,
  col  = c("darkgoldenrod", "green"),
  main = "Chilled Quebec and Mississippi Plants",
  xlab = "",
  ylab = "CO2 dioxide uptake rate (umol/m^2 sec)")


# Multivariate ANOVA ------------------------------------------------------

head(UScereal)
str(UScereal)

cereal <- MASS::UScereal
cereal$shelf <- factor(cereal$shelf)

# dependent variables
y <- cbind(cereal$calories, cereal$fat, cereal$sugars)     

# covariance matrix
cov(y)

# anova fit
fit <- manova(y ~ cereal$shelf)
summary(fit)
summary.aov(fit)

# assumptions
cent <- colMeans(y)
n <- nrow(y)
p <- ncol(y)
covar <- cov(y)
d <- mahalanobis(y, cent, covar)

qqplot(qchisq(ppoints(n), df = p), d)
abline(a = 0, b = 1)

# outliers
mvoutlier::aq.plot(y)

# robust manova - wilk's test
rrcov::Wilks.test(y, cereal$shelf, method = "mcd")


# ANOVA & Regression ------------------------------------------------------

cholest <- multcomp::cholesterol
levels(cholest$trt)

# aov fit
fit_aov <- aov(response ~ trt, data = cholest)
summary(fit_aov)

# lm fit
fit_lm <- lm(response ~ trt, data = cholest)
summary(fit_lm)

# contrasts
contrasts(cholest$trt)
