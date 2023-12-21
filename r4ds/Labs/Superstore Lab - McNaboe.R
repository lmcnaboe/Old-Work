library(tidyverse)
library(ggthemes)
library(ggplot2)
library(stringr)
library(lubridate)

# read in file
library(readxl)
superstore <- read_excel("data/Superstore.xlsx")

# take a peekie at if its tidy or not
view(superstore)

# cleaning names
library(janitor)
superstore <- clean_names(superstore, case = "snake")
view(superstore)

# dropping columns
superstore <- superstore |>
  select(
    category, department, discount, 
    order_date, order_priority, order_quantity,
    profit, region, sales, ship_mode, shipping_cost, 
    state, customer_name
    )
view(superstore)

# creating a column
superstore <- superstore |>
  mutate(expenses = sales - profit)
view(superstore)

# large discount
superstore <- superstore |>
  mutate(large_discount = (discount > 0.1))
view(superstore)

# middle name fraud
split <- strsplit(superstore$customer_name, " ")
superstore <- superstore |> mutate(num_of_words = lengths(split))
all_names <- superstore |> distinct(customer_name, num_of_words)
mid <- all_names |>
  filter(num_of_words != 2)
print(count(mid))

# no more middle name
superstore <- superstore |>
  filter(num_of_words == 2)
view(superstore)

# relocate
superstore <- superstore |>
  relocate(where(is.character))
view(superstore)

# east region
east <- superstore |>
  filter(region == "East") |>
  group_by(state) |>
  summarize(tot_sales = sum(sales))
view(east)

# profit by state
state_profits <- superstore |>
  group_by(state, department) |>
  summarize(avg_profit = mean(profit))
view(state_profits)

# order quantity
superstore <- superstore |>
  separate(order_date, 
           into = c("year", "month", "day"), sep = "-")
view(superstore)

order_amount <- superstore |>
  group_by(month) |>
  summarize(tot_orders = sum(order_quantity)) |>
  arrange(desc(tot_orders))
view(order_amount)

# avg, lowest, highest discount for each department + data points
department_discounts <- superstore |>
  group_by(department) |>
  summarize (
    avg_discount = mean(discount),
    lowest_discount = min(discount),
    higest_discount = max(discount),
    count = length(discount)
  )
view(department_discounts)

# plotting and scheming

combo <- superstore |>
  group_by(department, year, month) |>
  summarize(total_profit = sum(profit))
view(combo)

ggplot(
  combo,
  aes(
    x = paste(year, month),
    y = total_profit,
    group = department,
    color = department
  )
) +
  geom_line() +
  labs(
    x = "Date",
    y = " Total Profit",
    title = "Profit Change of Each Department Over Time"
  ) +
  theme(axis.text.x = element_text(angle = 90, size = 5))

# plotting and scheming: proportionality
orders_by_month <- superstore |>
  group_by(month, order_priority) |>
  summarize(total_orders = n()) |>
  mutate(prop = total_orders / sum(total_orders))
view(orders_by_month)

ggplot(
  orders_by_month,
  aes(
  x = month,
  y = prop,
  fill = order_priority)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Month",
    y = "Relative Frequency",
    title = "Proportion of Order Priorities per Month"
    )

# pt 2
orders_by_dep <- superstore |>
  group_by(department, order_priority) |>
  summarize(total_orders = n()) |>
  mutate(prop = total_orders / sum(total_orders))
view(orders_by_dep)

ggplot(
  orders_by_dep,
  aes(
    x = department,
    y = prop,
    fill = order_priority)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Department",
    y = "Relative Frequency",
    title = "Proportion of Order Priorities per Department"
  )

# stats
cor(superstore$order_quantity, superstore$sales)
