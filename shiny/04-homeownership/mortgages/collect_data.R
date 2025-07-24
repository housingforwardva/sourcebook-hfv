library(tidyverse)
library(shiny)
library(arrow)


# Show the number of loans originated in each activity year by race.

local_lookup <- read_csv("data/local_lookup.csv")


loans_race <- read_parquet("data/parquet/hmda_va_clean.parquet") |> 
  select(activity_year, lei, fips_full = county_code, loan_product, race_ethnicity,
         ethnicity, race, action_taken, purchaser_type, loan_purpose,
         occupancy_type) |> 
  mutate(fips_full = as.numeric(fips_full)) %>% 
  filter(action_taken == "Loan originated") |> 
  mutate(count = 1) |> 
  group_by(activity_year, fips_full, race_ethnicity, loan_purpose, occupancy_type) |> 
  summarise(count = sum(count)) %>% 
  left_join(local_lookup, by = "fips_full")

write_parquet(loans_race, "shiny/mortgages/hmda_va_clean.parquet")

library(tidyverse)
library(shiny)
library(arrow)


# Show the number of loans originated in each activity year by race.

local_lookup <- read_csv("data/local_lookup.csv")


loans_race <- read_parquet("shiny/mortgages/hmda_va_clean.parquet") |> 
  select(activity_year, lei, fips_full = county_code, loan_product, race_ethnicity,
         ethnicity, race, action_taken, purchaser_type, loan_purpose,
         occupancy_type) |> 
  mutate(fips_full = as.numeric(fips_full)) %>% 
  mutate(count = 1) |> 
  group_by(activity_year, fips_full, race_ethnicity, action_taken, loan_purpose, occupancy_type) |> 
  summarise(count = sum(count)) %>% 
  left_join(local_lookup, by = "fips_full")



# State-level visualization

ggplot(
  loans_race |> 
    group_by(activity_year, race_ethnicity, loan_purpose, occupancy_type) %>% 
    summarise(count = sum(count)) %>% 
  filter(activity_year == 2021) |> 
  filter(loan_purpose == "Home purchase") |> 
  filter(occupancy_type == "Principal residence"),
aes(x = reorder(race_ethnicity, count),
    y = count,
    fill = race_ethnicity)) +
  geom_col() +
  coord_flip() +
  theme_minimal()

# Regional-level visualization

ggplot(
  loans_race |> 
    group_by(activity_year, cbsa_title, race_ethnicity, loan_purpose, occupancy_type) %>% 
    summarise(count = sum(count)) %>% 
    filter(activity_year == 2021) |> 
    filter(cbsa_title == "Richmond, VA") %>% 
    filter(loan_purpose == "Home purchase") |> 
    filter(occupancy_type == "Principal residence"),
  aes(x = reorder(race_ethnicity, count),
      y = count,
      fill = race_ethnicity)) +
  geom_col() +
  coord_flip() +
  theme_minimal()

# Local-level visualization

ggplot(
  loans_race |> 
    group_by(activity_year, name_long, race_ethnicity, loan_purpose, occupancy_type) %>% 
    summarise(count = sum(count)) %>% 
    filter(activity_year == 2021) |> 
    filter(name_long == "Richmond City") %>% 
    filter(loan_purpose == "Home purchase") |> 
    filter(occupancy_type == "Principal residence"),
  aes(x = reorder(race_ethnicity, count),
      y = count,
      fill = race_ethnicity)) +
  geom_col() +
  coord_flip() +
  theme_minimal()
