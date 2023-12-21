library(tidyverse)
students <- read_csv("data/students.csv", na = c("N/A", ""))
students |>
  rename(
    student_id = 'Student ID',
    full_name = 'Full Name'
  )