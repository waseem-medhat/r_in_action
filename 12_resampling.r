library(coin)
library(multcomp)

# Introduction ------------------------------------------------------------

trt <- data.frame(
  val = c(40, 57, 45, 55, 58, 57, 64, 55, 62, 65),
  grp = factor(rep(c("a", "b"), each = 5)))
trt

plot(trt$val, trt$grp, ylim = c(0,3), col = 'steelblue', pch = 15)

# NOTE: coin requires categorical variables as factors
#       and ordinal variables as orderd factors

# comparing indep. t-test with `coin` exact test
t.test(
  val ~ grp,
  data = trt,
  var.equal = TRUE)

oneway_test(
  val ~ grp,
  data = trt,
  distribution = "exact")

# comparing mann-whitney with `coin` test
uscrime <- transform(MASS::UScrime, So = factor(So))

wilcox.test(
  Prob ~ So,
  data = uscrime,
  distribution = "exact")

wilcox_test(
  Prob ~ So,
  data = uscrime,
  distribution = "exact")

# comparing one way anova with `coin`
set.seed(1234)
oneway_test(
  response ~ trt,
  data = cholesterol,
  distribution = approximate(nresample = 9999))
