library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(ggthemes)

# reading in
library(readxl)
crater <- read_excel(
  "r4ds/data/weather.xlsx", 
  sheet = "Crater Lake")
view(crater)

# renaming
library(janitor)
crater <- clean_names(crater, case = "snake")
view(crater)

# past data frame
past <- crater |>
  mutate(newDay = seq(1, length(day))) |>
  select(
    record_high_f,
    record_low_f,
    month,
    day,
    newDay
  )
# present data frame
present <- crater |>
  mutate(newDay = seq(1, length(day))) |>
  select(
    high_f,
    low_f,
    average_high_f,
    average_low_f,
    month,
    day,
    newDay
  ) |>
  mutate(avg_temp = (high_f + low_f) / 2)

# historic background plot
p <- ggplot(past)+
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),  
        axis.title = element_blank()) +
  geom_linerange(
    past, 
    mapping = aes(
      x = newDay, 
      ymin = record_low_f, 
      ymax = record_high_f), 
    color = "darkslateblue", 
    alpha = 0.2)

# average highs and lows per day historically
p <- p +
  geom_linerange(
    present, 
    mapping = aes(
      x = newDay, 
      ymin = average_low_f, 
      ymax = average_high_f), 
    color = "pink")
print(p)

# high temp on the actual day
p <- p +
  geom_line(
    present, 
    mapping = aes(
      x = newDay, 
      y = high_f, 
      group = 1, color = "darkblue")) +
  geom_vline(
    xintercept = 0, 
    color = "darkblue", 
    linetype = 1,
    size = 1)

# low temp on the actual day
p <- p +
  geom_line(
    present, 
    mapping = aes(
      x = newDay, 
      y = low_f, 
      group = 1, color = "darkred")) +
  geom_vline(
    xintercept = 0, 
    color = "darkred", 
    linetype = 1,
    size = 1)

print(p)

# making lines
p <- p + 
  geom_hline(yintercept = -20, colour = "white", linetype=1) +
  geom_hline(yintercept = -10, colour = "white", linetype=1) +
  geom_hline(yintercept = 0, colour = "white", linetype=1) +
  geom_hline(yintercept = 10, colour = "white", linetype=1) +
  geom_hline(yintercept = 20, colour = "white", linetype=1) +
  geom_hline(yintercept = 30, colour = "white", linetype=1) +
  geom_hline(yintercept = 40, colour = "white", linetype=1) +
  geom_hline(yintercept = 50, colour = "white", linetype=1) +
  geom_hline(yintercept = 60, colour = "white", linetype=1) +
  geom_hline(yintercept = 70, colour = "white", linetype=1) +
  geom_hline(yintercept = 80, colour = "white", linetype=1) +
  geom_hline(yintercept = 90, colour = "white", linetype=1) +
  geom_hline(yintercept = 100, colour = "white", linetype=1)

print(p)

# making more lines
p <- p + 
  geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5) 

print(p)

# changin the x axis
p <- p +
  coord_cartesian(
    ylim = c(-20,100)
  ) +
  scale_y_continuous(
    breaks = seq(-20,100, by=10), 
    labels = a
  ) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("January", "February", "March", "April",
                                "May", "June", "July", "August", "September",
                                "October", "November", "December"))

print(p)

crater <- crater |>
  mutate(newDay = seq(1, length(day)))

# mutating idk
PresentHighs <- crater |>
  mutate(record = ifelse(high_f >= record_high_f, "Y", "N")) |>
  filter(record == "Y")
PresentLows <- crater |>
  mutate(record = ifelse(low_f <= record_low_f, "y", "n")) |>
  filter(record == "y")

# different lines 
p <- p +
  geom_point(
    data = PresentLows,
    aes(x=newDay, y = low_f),
    color="blue3") +
  geom_point(
    data = PresentHighs,
    aes(x=newDay, y = high_f),
    color="firebrick3")

print(p)

# graph title
p <- p +
  ggtitle("Crater Lakes's Weather in 2022") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20)) +
  annotate("text", x = 19, y = 98, label = "                Temperature", size=4, fontface="bold")

# capschion
p <- p +
  annotate("text", x = 110, y = 93, 
           label = "Data represents average daily temperatures.", size=3, colour="gray30") +
  annotate("text", x = 106, y = 89, 
           label = "Data for 2022 is available through December 31.", size=3, colour="gray30") +
  annotate("text", x = 108, y = 85, 
           label = "Average temperature for the year was 40.1°", size=3, colour="gray30") +
  annotate(
    "text", 
    x = 18, 
    y = 81, 
    label = ".", 
    size=3, 
    colour="gray30")

# capschion on a point
p <- p +
  annotate("segment", x = 49, xend = 56, y = 68, yend = 63, colour = "black") +
  annotate("text", x = 90, y = 63, label = "We had 3 days that were the", size=3, colour="black") +
  annotate("text", x = 90, y = 58, label = "warmest historically", size=3, colour="black")

# adding a guide
p <- p +
  annotate("segment", x = 181, xend = 181, y = 5, yend = 25, colour = "darkslateblue", alpha = 0.1, size=3) +
  annotate("segment", x = 181, xend = 181, y = 12, yend = 18, colour = "pink", size=3) +
  geom_line(data=legend_data, aes(x=x,y=y)) +
  annotate("segment", x = 183, xend = 185, y = 17.7, yend = 17.7, colour = "red", size=.5) +
  annotate("segment", x = 183, xend = 185, y = 12.2, yend = 12.2, colour = "blue", size=.5) +
  annotate("segment", x = 185, xend = 185, y = 12.2, yend = 17.7, colour = "wheat4", size=.5) +
  annotate("text", x = 196, y = 14.75, label = "Average Range", size=2, colour="gray30") +
  annotate("text", x = 162, y = 14.75, label = "2022 Temp", size=2, colour="gray30") +
  annotate("text", x = 193, y = 25, label = "Rec. High", size=2, colour="gray30") +
  annotate("text", x = 193, y = 5, label = "Rec. Low", size=2, colour="gray30")

print(p)