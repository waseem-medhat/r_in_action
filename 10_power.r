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


# Proportion Tests --------------------------------------------------------

# p's = 0.6, 0.65
# power = 0.9
# significance = 0.95
pwr.2p.test(
  h = ES.h(0.65, 0.6),
  power = 0.9,
  sig.level = 0.05,
  alternative = "greater"
)


# Chi-Square Test ---------------------------------------------------------

# population: 70% caucasian, 10% african, 20% hispanic
# promotion : 60% caucasian, 30% african, 30% hispanic
# expected  : 42% caucasian, 03% african, 10% hispanic

# tables (population vs. expected)
w <- ES.w2(matrix(c(0.42, 0.28, 0.03, 0.07, 0.1, 0.1), byrow = TRUE, nrow = 3))
pwr.chisq.test(
  w = 0.1853,
  df = 2,
  sig.level = 0.05,
  power = 0.9
)


# Power Plots -------------------------------------------------------------

# effect size vs. sample size
es <- seq(0.1, 0.5, 0.01)
nes <- length(es)

smpsize <- NULL
for (i in 1:nes) {
  result <- pwr.anova.test(
    k = 5,
    f = es[i],
    sig.level = 0.05,
    power = 0.9)
  smpsize[i] <- ceiling(result$n)}

plot(
  smpsize, es,
  type = "l",
  lwd = 2,
  col = "red")
