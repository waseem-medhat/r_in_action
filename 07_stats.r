library(doBy)
library(vcd)

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

