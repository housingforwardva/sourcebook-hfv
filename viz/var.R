library(tidyverse)
library(air)
library(gganimate)
library(scales)


var_data <- read_rds("shiny/04-homeownership/var/data.rds") 

  
top <- var_data |>
  mutate(quarter_num = as.numeric(as.factor(quarter))) |>
  arrange(quarter_num) |> 
  filter(quarter == "2025 Q2") |> 
  filter(geography == "MSA")



hrra <- c("Chesapake City", "Franklin City", "Norfolk City", "Portsmouth City", "Suffolk City", 
  "Virginia Beach City", "Isle of Wight County")

southside_units <- var_data |> 
  filter(name %in% hrra) |>
  mutate(quarter_num = as.numeric(as.factor(quarter))) |>
  arrange(quarter_num) |> 
  mutate(year_date = as.character(quarter)) |> 
  mutate(year_label = substr(year_date, 1, 4)) |> 
  group_by(year_label, name) |> 
  summarise(units = sum(units))

southside_price <- var_data |> 
  filter(name %in% hrra)


library(ggplot2)
library(scales)

# Assuming these are the six unique names in your data
names <- unique(southside_units$name)

# Create a named vector mapping names to HFV colors
name_colors <- setNames(
  c(hfv_colors$sky, hfv_colors$grass, hfv_colors$lilac, 
    hfv_colors$berry, hfv_colors$desert, hfv_colors$shadow),
  names
)

ggplot(southside_units, aes(x = year_label, y = units, fill = name)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = name_colors) +
  scale_color_manual(values = name_colors) +
  scale_y_continuous(labels = number_format(scale = 1, big.mark = ","),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Homes Sold in South Hampton Roads",
    x = "Year",
    y = "Housing Units Sold"
  ) +
  theme_minimal(base_family = "Open Sans") +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_blank(),
    plot.margin = margin(10, 10, 40, 10) # Extra bottom margin for logo
  )

library(ggplot2)
library(scales)
library(ggrepel)

# Create a data frame for labels
label_data <- southside_price %>%
  group_by(name) %>%
  filter(quarter == max(quarter)) %>%
  mutate(
    label_x = as.numeric(max(as.Date(southside_price$quarter))) + 90, # Convert to numeric and add 90 days
    label_y = med_price
  )

ggplot(southside_price, aes(x = quarter, y = med_price, group = name)) +
  geom_line(aes(color = name), size = 1.2) +
  geom_text(
    data = label_data,
    aes(x = label_x, y = label_y, label = name, color = name),
    size = 4,
    hjust = 0 # Left-align labels
  ) +
  scale_color_manual(values = name_colors) +
  labs(
    title = "Median Sales Price in South Hampton Roads"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # Format x-axis labels as years
  theme_minimal(base_family = "Open Sans") +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.title = element_blank(),
    plot.margin = margin(10, 10, 40, 80) # Increased right margin to 80
  )

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