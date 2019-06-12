library(doBy)
library(vcd)
library(magrittr)

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
