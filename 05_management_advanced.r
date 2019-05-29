library(MASS)

# Statistical Functions ---------------------------------------------------

# standardizing data
scale(c(10, 11, 12)) # * SD + M

# probability functions
x <- pretty(-3:3, 30)
y <- dnorm(x)
plot(x, y, type = 'l')

# area under standard normal curve left of z = 1.96
pnorm(1.96)

# 90th percentile of standard normal given
# mean = 500, sd = 100
qnorm(0.9, mean = 500, sd = 100)

# 50 random normal deviates given
# mean = 50, sd = 10
rnorm(50, mean = 50, sd = 10)

# generate data from multivariate normal distribution
means <- c(230.7, 146.7, 3.6)
sigma <- matrix(
  c(15360.8, 6721.2, -47.1,
    6721.2, 4700.9, -16.5,
    -47.1, -16.5, 0.3),
  nrow = 3, ncol = 3
)

mvrnorm(500, means, sigma)


# Character Functions -----------------------------------------------------

# character legnth
nchar(c("hello", "world!"))

# extract or replace substring
str <- "helloo"
substr(str, 2, 4)

substr(str, 2, 4) <- "ood"
str

name <- "Bob"
cat("Hello", name, "\b.\n", "Isn\'t R", "\t", "GREAT?\n")


# Vectorized Applications -------------------------------------------------

mat <- matrix(runif(30), nrow = 6)
mat

mean(mat)
log(mat)

# apply function across a dimension
apply(mat, 1, mean) # apply on rows
apply(mat, 2, mean) # apply on cols


# Data Management Challenge -----------------------------------------------

# data
Student <- c(
  "John Davis", "Angela Willams", "Bullwinkle Moose", "David Jones",
  "Janice Markhammer", "Cheryl Cushing", "Reuven Ytzrhak", "Greg Knox",
  "Joel England", "Mary Rayburn"
)
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)

roster <- data.frame(
  Student, Math, Science, English,
  stringsAsFactors = FALSE
)

# scores
z <- scale(roster[ , 2:4])
score <- apply(z, 1, mean)

roster <- cbind(roster, score)

# grades
quants <- quantile(score, c(0.8, 0.6, 0.4, 0.2))
roster$grade[score >= quants[1]                    ] <- "A"
roster$grade[score < quants[1] & score >= quants[2]] <- "B"
roster$grade[score < quants[2] & score >= quants[3]] <- "C"
roster$grade[score < quants[3] & score >= quants[4]] <- "D"
roster$grade[score < quants[4]                     ] <- "F"

# split names and sort by them
name <- strsplit(roster$Student, " ")
FirstName <- sapply(name, "[", 1)
LastName  <- sapply(name, "[", 2)

roster <- cbind(FirstName, LastName, roster[-1])
roster <- roster[order(FirstName, LastName), ]

roster
