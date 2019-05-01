# refer to variable names
with(mtcars, summary(mpg))

# multiple commands
with(mtcars, {
  print(summary(mpg))
  print(cor(mpg, disp))
})

# assignments
with(mtcars, {
  mpg_summary <- summary(mpg)
  mpg_summary2 <<- summary(mpg)
})

mpg_summary
mpg_summary2