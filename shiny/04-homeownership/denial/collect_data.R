library(tidyverse)
library(shiny)
library(arrow)


# Show the denial rate for each race_ethnicity based on loan purpose and occupancy type.

local_lookup <- read_csv("data/local_lookup.csv") |> 
  mutate(fips_full = as.character(fips_full))

# Pull the hmda_va_clean.parquet, aggregate the data based on fips_full, then join to the geographic look up table.

loans_race <- read_parquet("data/parquet/hmda_va_clean.parquet") |> 
  select(activity_year, lei, fips_full = county_code, race_ethnicity, action_taken, purchaser_type, loan_purpose,
         occupancy_type)|> 
  mutate(count = 1) |> 
  group_by(activity_year, fips_full, race_ethnicity, action_taken, loan_purpose, occupancy_type) |> 
  summarise(count = sum(count)) %>% 
  left_join(local_lookup, by = "fips_full") |> 
  filter(state == "Virginia") # Remove entries that are coded for loans out-of-state.

# Aggregate the data for the state, cbsa, and jurisdiction levels and then calculate the denial rate for each race_ethnicity. 
# loan_purpose and occupancy_type will be interactive filters for the user.

denial_state <- loans_race |> 
  filter(activity_year == 2024) |> 
  group_by(state, race_ethnicity, loan_purpose, occupancy_type) |> 
  mutate(total = sum(count)) |> 
  filter(loan_purpose == "Home purchase") |> 
  filter(occupancy_type == "Principal residence") |> 
  group_by(race_ethnicity, action_taken, total) |> 
  summarise(count = sum(count)) |> 
  mutate(rate = count/total) |> 
  filter(action_taken == "Application denied")


denial_cbsa <- loans_race |> 
  filter(activity_year == 2024) |> 
  group_by(cbsa_title, race_ethnicity, loan_purpose, occupancy_type) |> 
  mutate(total = sum(count)) |> 
  filter(loan_purpose == "Home purchase") |> 
  filter(occupancy_type == "Principal residence") |> 
  group_by(cbsa_title, race_ethnicity, action_taken, total) |> 
  summarise(count = sum(count)) |> 
  mutate(rate = count/total) |> 
  filter(action_taken == "Application denied")


denial_juris <- loans_race |> 
  filter(activity_year == 2024) |> 
  group_by(name_long, race_ethnicity, loan_purpose, occupancy_type) |> 
  mutate(total = sum(count)) |> 
  filter(loan_purpose == "Home purchase") |> 
  filter(occupancy_type == "Principal residence") |> 
  group_by(name_long, race_ethnicity, action_taken, total) |> 
  summarise(count = sum(count)) |> 
  mutate(rate = count/total) |> 
  filter(action_taken == "Application denied")

# Create bar charts showing the denial rate arranged in descending order. The bar chart should be 
# horizontally oriented and the race_ethnicity categories should be colors consistently.

ggplot(denial_state,
aes(x = reorder(race_ethnicity, rate),
    y = rate,
    fill = race_ethnicity)) +
  geom_col() +
  coord_flip() +
  theme_minimal()
