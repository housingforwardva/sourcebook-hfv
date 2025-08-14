library(tidyverse)
library(air)
library(gganimate)
library(scales)


var_data <- read_rds("shiny/04-homeownership/var/home-sales.rds")

hrra <- c("Chesapake City", "Franklin City", "Norfolk City", "Portsmouth City", "Suffolk City", 
  "Virginia Beach City", "Isle of Wight County")

southside <- var_data |> 
  filter(name %in% hrra) |>
  mutate(quarter_num = as.numeric(as.factor(quarter))) |>
  arrange(quarter_num)

# Animated home sales plot
p1 <- ggplot(southside,
  aes(x = quarter,
      y = units,
      fill = name)) +
  geom_col() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    strip.text = element_text(size = 11, face = "bold")
  ) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title = "Home Sales by Quarter",
    subtitle = "Hampton Roads Region Areas: {closest_state}",
    caption = "Source: VAR"
  ) +
  facet_wrap(~name) +
  transition_time(quarter_num) +
  shadow_wake(wake_length = 1, alpha = 0.3) +
  ease_aes('sine-in-out')

# Render animation
anim1 <- animate(p1, 
                width = 1200, 
                height = 800, 
                res = 150,
                fps = 5,
                duration = 10) 


bps <- read_rds("shiny/03-supply/bps/bps.rds") |> 
  filter(name_long %in% hrra) |> 
  mutate(type = case_when(
    type == "2-units" ~ "2-4 units",
    type == "3-4 units" ~ "2-4 units",
    TRUE ~ type
  )) |>
  mutate(year_num = as.numeric(year)) |>
  arrange(year_num)

# Animated building permits plot
p2 <- ggplot(bps,
  aes(x = year,
      y = units,
      fill = type)) +
  geom_col(position = "stack") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = comma_format()) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Building Permits by Type and Year",
    subtitle = "Hampton Roads Region Areas: {closest_state}",
    x = "Year",
    y = "Permits Issued",
    fill = "Unit Type",
    caption = "Source: Building Permit Survey"
  ) +
  transition_time(year_num) +
  shadow_wake(wake_length = 1, alpha = 0.3) +
  ease_aes('sine-in-out')

# Render animation
anim2 <- animate(p2, 
                width = 1200, 
                height = 800, 
                res = 150,
                fps = 5,
                duration = 12)

# Save animations for presentation
anim_save("home_sales_animation.gif", anim1)
anim_save("building_permits_animation.gif", anim2)

# Display animations (uncomment to view)
# anim1
# anim2

anim1
anim2