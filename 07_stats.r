library(doBy)
library(vcd)
library(magrittr)
library(ggm)

# Descriptive Statistics --------------------------------------------------

# numeric summary
mtcars_num <- mtcars[c("mpg", "hp", "wt")]
summary(mtcars_num)

# sapply
sapply(mtcars_num, fivenum)

mystats <- function(x, na.omit=FALSE){

  # omit NAs if specified
  if (na.omit) { x <- x[!is.na(x)] }

  # calculate stats
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x - m) ^ 3 / s ^ 3) / n
  kurt <- sum((x-m) ^ 4 / s ^ 4) / n - 3
  return(c(n = n, mean = m, stdev = s, skew = skew, kurtosis = kurt))

}

sapply(mtcars_num, mystats)

# Hmisc describe
Hmisc::describe(mtcars_num)

# pastecs stat.desc
pastecs::stat.desc(mtcars_num, norm = TRUE)

# psych describe
psych::describe(mtcars_num)

# grouped summary - aggregate()
# better with one function
aggregate(mtcars_num, by = list(am = mtcars$am), mean)

# grouped summary - by()
by(mtcars_num, mtcars$am, function(x) { sapply(x, mystats) })

# grouped summary - doBy::summaryBy()
doBy::summaryBy(mpg + hp + wt ~ am, data = mtcars, FUN = mystats)

# grouped summary - psych::describeBy()
psych::describeBy(mtcars_num, list(am = mtcars$am))


# Frequency and Contingency Tables ---------------------------------------------

# one-way table
oneway <- table(Arthritis$Improved)

# two-way table
twoway <- xtabs( ~ Treatment + Improved, data = Arthritis)

# margins and proportions
margin.table(twoway, 1)
prop.table(twoway, 1) %>% addmargins()

# CrossTable()
gmodels::CrossTable(Arthritis$Treatment, Arthritis$Improved, chisq = TRUE)

# multidimensional tables
nway <- table(Arthritis[ , c('Treatment', 'Sex', 'Improved')])

# flatten table
ftable(nway)

# chi-square
chisq.test(twoway)

# fisher exact
fisher.test(twoway)

# mantel haenszel
threeway <- xtabs( ~ Treatment + Improved + Sex, data = Arthritis)
mantelhaen.test(threeway)

# association: phi, cramer's v
assocstats(twoway)


# Correlations -----------------------------------------------------------------

states <- state.x77[ , 1:6]

# variance-covariance matrix
cov(states)
states[ , "Illiteracy"] %>% var()

# correlation matrices (pearson)
cor(states)

# correlation matrices (spearman)
cor(states, method = "spearman")

plot(states[ , c("HS Grad", "Population")])

# nonsquare correlation matrices
cor(
  states[ , c("Population", "Income", "Illiteracy", "HS Grad")],
  states[ , c("Life Exp", "Murder")]
)

# partial correlation:

paste0(1:ncol(states), ":", colnames(states))
# [1] "1:Population" "2:Income"     "3:Illiteracy" "4:Life Exp"   "5:Murder"    
# [6] "6:HS Grad"   

# first two numbers in the vector are indices of variables to be
# correlated, the rest are the control variable indices
ggm::pcor(c(1, 5, 2, 3, 6), cov(states))

# correlation significance:
# base cor.test works for a single pearson correlation
cor.test(states[ ,3], states[ ,5])

# multiple correlation tests
print(psych::corr.test(states, use = "complete"), short = FALSE)
psych::cor.ci(states)


# t-tests ----------------------------------------------------------------------

