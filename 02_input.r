
# Manual Input ------------------------------------------------------------

mydata <- data.frame(
  age = numeric(),
  gender = character(),
  weight = numeric()
)

mydata <- edit(mydata)
fix(mydata) # alternative to the line above


# Text Files --------------------------------------------------------------

read.table(
  "data/grades.txt",
  header = TRUE,
  sep = ",",
  row.names = 'StudentID',
  stringsAsFactors = FALSE
)

# NOTE: 'incomplete final line' error results from the absence of newline
#       character in the last line


# SPSS --------------------------------------------------------------------

Hmisc::spss.get("data/hep.sav")
foreign::read.spss("data/hep.sav", to.data.frame = TRUE)
