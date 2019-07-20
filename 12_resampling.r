library(coin)
library(boot)
library(multcomp)

# Permutation Tests -------------------------------------------------------

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


# contingency tables
arth <- transform(vcd::Arthritis, Improved = as.factor(as.numeric(Improved)))
set.seed(1234)
chisq.test(arth$Improved, arth$Treatment)
chisq_test(Treatment ~ Improved, data = arth, distribution = approximate())

# numeric variables
states <- as.data.frame(state.x77)
cor.test(states$Illiteracy, states$Murder, method = "spearman")
spearman_test(Illiteracy ~ Murder, data = states, distribution = approximate())

# dependent samples
crime <- MASS::UScrime
wilcox.test(crime$U1, crime$U2, paired = TRUE, correct = FALSE)
wilcoxsign_test(U1 ~ U2, data = crime, distribution = "exact")


# Permutation Tests - Linear Models ---------------------------------------
