library(dplyr)
library(multcomp)
library(gplots)
library(car)
library(effects)
library(HH)

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
