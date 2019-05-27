library(dplyr)

# Leadership Data Frame ---------------------------------------------------

# create the data frame
manager <- 1:5
date <- c("10/24/08", "10/28/08", "10/1/08",
          "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)

leadership <- data.frame(
  manager, date, country, gender,
  age, q1, q2, q3, q4, q5
)


# Data Frame Operations ---------------------------------------------------

my_data <- data.frame(
  x1 = c(2, 2, 6, 4),
  x2 = c(3, 4, 2, 8)
)

# mutating: transform function
# edits returns df with new columns
transform(
  my_data,
  sumx  = x1 + x2,
  meanx = (x1 + x2) / 2
)

# conditional recoding of values as NA
leadership$age[leadership$age == 99] <- NA

# recoding ages into categories
leadership$agecat[leadership$age >  75] <- "Elder"
leadership$agecat[leadership$age <= 75
                  & leadership$age >= 50] <- "Middle-Aged"
leadership$agecat[leadership$age <  50] <- "Young"

# alternative recoding
# the within() fn returns a modified df
leadership <- within(
  leadership,
  {
    agecat <- NA
    agecat[age >  75]   <- "Elder"
    agecat[age >= 50
           & age <= 75] <- "Middle-Aged"
    agecat[age <  50]   <- "Young"
  }
)

# renaming variables
names(leadership)[6:10] <- c("item1", "item2", "item3", "item4", "item5")

# dplyr
leadership <- leadership %>%
  rename(
    managerID = "manager",
    testDate  = "date"
  )

