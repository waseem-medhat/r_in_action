## VECTORS
a <- c("k", "j", "h", "a", "c", "m")

# vector subsetting
a
a[3]
a[c(1, 3, 5)]
a[2:6]  # 2:6 is the same as c(2, 3, 4, 5, 6)


## MATRICES
m1 <- matrix(1:20, nrow = 5, ncol = 4)
m1

cells <- c(1, 26, 24, 28)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2")

m2 <- matrix(
  data = cells,
  nrow = 2,
  ncol = 2,
  byrow = TRUE, # fills cells by rows
  dimnames = list(rnames, cnames)
)
m2

m3 <- matrix(
  data = cells,
  nrow = 2,
  ncol = 2,
  byrow = FALSE, # fills cells by rows
  dimnames = list(rnames, cnames)
)
m3

# subsetting matrices
m1
m1[2, ]
m1[ ,2]
m1[1,3]
m1[1, c(3,4)]


## ARRAYS
dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")

ar <- array(1:24, c(2, 3, 4), dimnames = list(dim1, dim2, dim3))
ar  # 2x3x4 array

# subsetting arrays
ar[1, 2, 1]


## DATA FRAMES

patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")

patientdata <- data.frame(patientID, age, diabetes, status)
patientdata

# subsetting data frames
patientdata[1, ]
patientdata[ ,2]
patientdata[c("age", "status")]
patientdata$age

# setting a case identifier
patientdata <- data.frame(patientID, age, diabetes, status,
                          row.names = patientID)
patientdata


## FACTORS
diabetes <- factor(diabetes)
diabetes

status <- factor(status, ordered = TRUE,
                 levels = c("Poor", "Improved", "Excellent"))
status
unclass(status)

# convert numeric (coded) to factor
# e.g. males = 1, females = 2
sex <- c(1, 1, 2, 3)
sex <- factor(sex, levels = c(1, 2), labels = c("Male", "Female"))
sex

# update data frame with factor variable
patientdata <- data.frame(patientID, age, diabetes, status,
                          row.names = patientID)

str(patientdata)
summary(patientdata)