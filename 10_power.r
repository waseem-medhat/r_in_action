library(pwr)

# t-tests -----------------------------------------------------------------

# reaction time has a sd of 1.25 s
# 1 s is significant
# 90% power; 95% confidence
pwr.t.test(
  d = (1 / 1.25),
  sig.level = 0.05,
  power = 0.9,
  type = "two.sample",
  alternative = "two.sided"
)

# 0.5 sd difference (d = 0.5)
# 0.01 alpha
# n = 40
pwr.t.test(
  n = 20, # in EACH group
  d = 0.5,
  sig.level = 0.01,
  alternative = "two.sided"
)

# NOTE: arguments n1 and n2 can be used for unequal sample sizes.


# ANOVA -------------------------------------------------------------------

# beta = 0.8
# eff size = 0.25
# alpha = 0.05
# 5 groups
pwr.anova.test(
  k = 5,
  f = 0.25,
  sig.level = 0.05,
  power = 0.8
)


# Correlations ------------------------------------------------------------

# r = 0.25
# alpha = 0.05
# power = 0.9
pwr.r.test(
  r = 0.25,
  sig.level = 0.05,
  power = 0.9,
  alternative = "greater"
)


# Linear Models -----------------------------------------------------------

# significance = 0.05
# power = 0.9
# u = 3; (total predictors - B predictors)
# f2 = (0.35 - 0.3) / (1 - 0.35)
pwr.f2.test(
  u = 3,
  f2 = (0.35 - 0.3) / (1 - 0.35),
  sig.level = 0.05,
  power = 0.9
)

# NOTE: v = N - K - 1
#       N = v + K + 1


# Proportions -------------------------------------------------------------

