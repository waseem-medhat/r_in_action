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


# Missing Values ----------------------------------------

# check for missing values
is.na(leadership[ , 6:10])

# other non-missing values
is.infinite(Inf)            # Inf = infinte
is.nan(sin(Inf))            # NaN = not a number

# missing values in action
x <- c(1, 2, NA, 5)
x[1] + x[2] + x[3] + x[4]   # NA
sum(x)                      # NA
sum(x, na.rm = TRUE)        # 8

# omit rows with NAs
na.omit(leadership)


# Dates -------------------------------------------------

# convert strings to dates
leadership$date <- as.Date(leadership$testDate, "%m/%d/%y")

# date functions
Sys.Date()
date()

today <- Sys.Date()
format(today, format = "%A")

# date arithmetic
date_start <- as.Date("2004-02-13")
date_end   <- as.Date("2011-01-22")

date_end - date_start
difftime(date_end, date_start, units = "mins")

# convert dates to strings
as.character(date_start)


# Sorting -----------------------------------------------------------------

with(
  leadership,
  leadership[order(age), ]          # sort by asc. age
)

with(
  leadership,
  leadership[order(gender, age), ]  # sort by asc. gender then age
)

with(
  leadership,
  leadership[order(gender, -age), ] # sort by asc. gender then desc. age
)


# Merging -----------------------------------------------------------------

# horizontally, like SQL join
# merge(dfA, dfB, by = 'ID')

# horizontal concatenation
# cbind()

# adding rows
# rbind(dfA, dfB)


# Subsetting --------------------------------------------------------------

# variables, by index
leadership[ , 6:10]

# variables, by name
leadership[paste0("item", 1:5)]

# exclude variables, by name
leadership[ !names(leadership) %in% c("item3", "item4") ]

# exclude variables, by index
leadership[ , -6:-10]

# rows, by index
leadership[1:3, ]

# rows, by condition
leadership[leadership$gender == "M"
           & leadership$age  > 30]

# using subset()
subset(
  leadership,
  age >= 35 | age < 24,
  select = paste0("item", 1:5)
)

# random sample of rows
leadership[
  sample(1:nrow(leadership), size = 3, replace = FALSE)
, ]
