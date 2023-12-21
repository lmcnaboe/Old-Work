library(tidyverse)
# students <- read_csv("https://pos.it/r4ds-students-csv", na = c("N/A", ""))
# students |>
#   rename(
#     student_id = "Student ID",
#     full_name = "Full Name"
#   )
# 
# students <- students |>
#   janitor::clean_names() |>
#   mutate(
#     meal_plan = factor(meal_plan),
#     age = parse_number(if_else(age == "five", "5", age))
#   )
# 
# read_csv(
#   "a,b,c
#   1,2,3
#   4,5,6"
# )

ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, shape = class)) +
  geom_point()

ggplot(
  mpg, 
  mapping = aes(
    x = hwy, 
    y = displ)) + 
  geom_point(
    shape = 17, 
    color = "pink")

ggplot(mpg, mapping = aes(x = displ, y = hwy, color = displ < 5)) + 
  geom_point()