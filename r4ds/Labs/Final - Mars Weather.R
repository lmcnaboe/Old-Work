library(tidyverse)
library(ggthemes)
library(ggplot2)
library(stringr)
library(lubridate)
library(gifski)
library(gganimate)
library(dplyr)

# read in file
mars <- read_csv("data/mars-weather.csv")
view(mars)
jax <- read_csv("data/KJAX.csv")
view(jax)

# clean column names
library(janitor)
mars <- clean_names(mars, case = "snake")
view(mars)

# creating an average temperature column
mars <- mars |>
  mutate(avg_temp = (min_temp + max_temp) / 2) |>
  relocate (avg_temp, .after = max_temp)
view(mars)

# making bins of average temperature
mars <- mars |>
  mutate(temp_bin = cut(avg_temp, breaks = 7)) |>
  relocate (temp_bin, .after = avg_temp)
view(mars)

# defining weather section??? idk what to call it
mars <- mars |>
  mutate(season = cut(ls, breaks = 4)) |>
  relocate (season, .after = ls)
mars$season <- factor(
  mars$season, 
  levels = c(
    "(-0.359,89.8]", 
    "(89.8,180]", 
    "(180,269]", 
    "(269,359]"), 
  labels = c(
    "Winter", 
    "Spring", 
    "Summer", 
    "Autumn"))
view(mars)

# average temperature based on the season
ggplot(mars, aes(x = season, y = avg_temp)) + 
  geom_boxplot(
    fill = c(
      rgb(0,0,.408), # dark blue winter
      rgb(.984,.74,.692), # pink spring
      rgb(.14, .44, .6), # pale blue summer
      rgb(.98,.532,.292))) + # orange autumn
  labs(title = "Five Number Summary of Temperature per Season",
         x = "Season",
         y = "Average Temperature (?C)")

#-------------------------------------------------------------------------------

# mars weather time series graph
view(jax)
time <- ggplot(mars, aes(x=sol, y=avg_temp)) +
  geom_line(color = "hotpink") + 
  labs(
    title = "Change in Average Temperature Over Time (Mars", 
    x = "Days Since Landed", 
    y = "Average Temperature (?C)")
time

#-------------------------------------------------------------------------------

# animated ^^
mars_graph = mars |>
  ggplot(aes(x=sol, y=avg_temp, color = "pink")) +
  geom_line(size = 2, alpha = 0.75) +
  theme_solarized_2(light = FALSE) +
  labs(title = "Temperature Over Time",
       x = "Days Since Landed",
       y = "Average Temperature (?C)") +
  theme(text = element_text(family = "DM Sans Medium", colour = "#EEEEEE"),
        title = element_text(color = "#EEEEEE"),
        axis.title.x = element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Pastel1") +
  geom_point() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 2100, by = 100))

mars_graph.animation = mars_graph +
  transition_reveal(sol) + 
  view_follow(fixed_y = TRUE)

animate(mars_graph.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100, renderer = gifski_renderer())
anim_save("mars weather.gif")

#-------------------------------------------------------------------------------

# animated jacksonville weather
#FIX THIS SOMETHING IS WRONG !!!!!!!!!!!!!!!!!!!!!
jax$date <- as.Date(jax$date, format = "%m/%d/%Y")
mars$terrestrial_date <- as.Date(mars$terrestrial_date, format = "%m/%d/%Y")
mars <- mars |>
  rename(date = terrestrial_date)
view(mars)

common_dates <- semi_join(mars, jax, by = "date")

both_graph = ggplot(common_dates) +
  geom_line(aes(x = date, y = avg_temp), color = "orange", size = 2, alpha = 0.75) +
  geom_line(data = jax, aes(x = date, y = actual_mean_temp), color = "blue", size = 2, alpha = 0.75) +
  theme_solarized_2(light = FALSE) +
  labs(title = "Temperature Over Time",
       y = "Average Temperature (°C)",
       x = "Earth Date") +
  theme(text = element_text(family = "DM Sans Medium", colour = "#EEEEEE"),
        title = element_text(color = "#EEEEEE"),
        axis.title.x = element_text(),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Pastel1") +
  geom_point() + 
  theme(legend.position = "none") +
  #scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

both_graph.animation = both_graph +
  transition_reveal(date) + 
  view_follow(fixed_y = TRUE)

animate(both_graph.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100, renderer = gifski_renderer())
anim_save("both weather.gif")

#-------------------------------------------------------------------------------

season_avg_temp <- mars |>
  group_by(season) |>
  summarize(avg_temp = mean(avg_temp, na.rm = TRUE))

ggplot(season_avg_temp, aes(x = season, y = avg_temp, fill = season)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Temperatures by Season",
       x = "Season",
       y = "Average Temperature (?C)")

# pressure over time
time <- ggplot(mars, aes(x=sol, y=pressure)) +
  geom_line(color = "hotpink") + 
  labs(
    title = "Change in Average Temperature Over Time (Mars)", 
    x = "Days Since Landed", 
    y = "Pressure (kPa)")
time

# scatter of pressure vs. temperature
season_colors <- c(
  "Spring" = rgb(.984,.74,.692), 
  "Summer" = rgb(.14, .44, .6), 
  "Autumn" = rgb(.98,.532,.292), 
  "Winter" = rgb(0,0,.408))
pres_temp <- mars |>
  ggplot(aes(x=pressure, y=avg_temp, color = season)) + 
  geom_point() +
  scale_color_manual(values = season_colors) +
  labs(title = "Pressure (kPa) vs. Mean Temperature (?C)",
       x = "Pressure (kPa)",
       y = "Temperature (?C)")
pres_temp

#------------------------------------------------------------------------------

# animated map of temperature in Jacksonville, Fl
library("sf")
library("maps")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

(sites <- data.frame(
  longitude = c(
    -81.8288090, -82.0670956), 
  latitude = c(
    30.1142026, 30.5191069)))

(sites <- st_as_sf(sites, coords = c("longitude", "latitude"), 
                   crs = 4326, agr = "constant"))

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("florida", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, fill = NA, color = gray(.5)) +
  coord_sf(xlim = c(-82.25, -81), ylim = c(30, 30.75), expand = FALSE)

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, aes(fill = jax$actual_mean_temp)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

# loc = 'Florida'
# map = get_map(location = loc, zoom = 6)
# map_data = ggmap(map) + geom_point(data = jax, aes(x = ))
# base_map <- ggplot(data = jax, mapping = aes(x = 30.3322, y = 81.6557)) +
#   geom_polygon(color = "black", fill = "white") +
#   coord_quickmap() +
#   theme_void()
# base_map
#-------------------------------------------------------------------------------
p <- ggplot()
p <- p + 
  theme(legend.position = "none") +
  geom_line(
    mapping = aes(
      mars,
      x = date,
      y = avg_temp,
      group = 1, color = "orange")) +
  geom_vline(
    xintercept = 0,
    color = "orange",
    linetype = 1,
    size = 1) +
  geom_line(
    jax,
    mapping = aes(
      x = date,
      y = actual_mean_temp,
      group = 1, color = "blue")) +
  geom_vline(
    xintercept = 0,
    color = "blue",
    linetype = 1,
    size = 1)

print(p)

