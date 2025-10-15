library(tidyverse)
library(paws)
library(lubridate)

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "census/b17001_data.rds"
)

lookup <- read_csv("data/local_lookup.csv") %>% 
  mutate(GEOID = as.character(fips_full))

data <- tryCatch({
  decompressed <- memDecompress(s3_response$Body, type = "gzip")
  readRDS(rawConnection(decompressed))
}, error = function(e) {
  # If decompression fails, try reading directly
  readRDS(rawConnection(s3_response$Body))
}) 


va_data <- data |> 
  right_join(lookup, by = "GEOID") |> 
  mutate(age = case_when(
    age == "Under 5 years" ~ "17 years and under",
    age == "5 years" ~ "17 years and under",
    age == "6 to 11 years" ~ "17 years and under",
    age == "12 to 14 years" ~ "17 years and under",
    age == "15 years" ~ "17 years and under",
    age == "16 and 17 years" ~ "17 years and under",
    TRUE ~ age
  )) |> 
  mutate(age_group = case_when(
    age %in% c("17 years and under", "18 to 24 years", "25 to 34 years") ~ "Young (Under 35)",
    age %in% c("35 to 44 years", "45 to 54 years", "55 to 64 years") ~ "Middle-aged (35-64)", 
    age %in% c("65 to 74 years", "75 years and over") ~ "Older adults (65+)",
    TRUE ~ NA_character_  # catches any unexpected values
  )) |> 
  select(NAME = name_long, geography, year, poverty, age, age_group, estimate) |> 
  drop_na() |> 
  group_by(NAME, geography, year, poverty, age, age_group) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  group_by(NAME, geography, year, age, age_group) |> 
  mutate(total = sum(estimate)) |> 
  mutate(rate = estimate/total)

state_data <- data |> 
  filter(geography == "state") |> 
  mutate(age = case_when(
    age == "Under 5 years" ~ "17 years and under",
    age == "5 years" ~ "17 years and under",
    age == "6 to 11 years" ~ "17 years and under",
    age == "12 to 14 years" ~ "17 years and under",
    age == "15 years" ~ "17 years and under",
    age == "16 and 17 years" ~ "17 years and under",
    TRUE ~ age
  )) |> 
  mutate(age_group = case_when(
    age %in% c("17 years and under", "18 to 24 years", "25 to 34 years") ~ "Young (Under 35)",
    age %in% c("35 to 44 years", "45 to 54 years", "55 to 64 years") ~ "Middle-aged (35-64)", 
    age %in% c("65 to 74 years", "75 years and over") ~ "Older adults (65+)",
    TRUE ~ NA_character_  # catches any unexpected values
  )) |> 
  select(NAME, geography, year, poverty, age, age_group, estimate) |> 
  drop_na() |> 
  group_by(NAME, geography, year, poverty, age, age_group) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup()  |> 
  group_by(NAME, geography, year, age, age_group) |> 
  mutate(total = sum(estimate))  |> 
  mutate(rate = estimate/total)

cbsa_data <- data |> 
  filter(geography == "cbsa") |> 
  filter(str_detect(NAME, "VA")) |> 
  mutate(age = case_when(
    age == "Under 5 years" ~ "17 years and under",
    age == "5 years" ~ "17 years and under",
    age == "6 to 11 years" ~ "17 years and under",
    age == "12 to 14 years" ~ "17 years and under",
    age == "15 years" ~ "17 years and under",
    age == "16 and 17 years" ~ "17 years and under",
    TRUE ~ age
  )) |> 
  mutate(age_group = case_when(
    age %in% c("17 years and under", "18 to 24 years", "25 to 34 years") ~ "Young (Under 35)",
    age %in% c("35 to 44 years", "45 to 54 years", "55 to 64 years") ~ "Middle-aged (35-64)", 
    age %in% c("65 to 74 years", "75 years and over") ~ "Older adults (65+)",
    TRUE ~ NA_character_  # catches any unexpected values
  )) |> 
  select(NAME, geography, year, poverty, age, age_group, estimate) |> 
  drop_na() |> 
  group_by(NAME, geography, year, poverty, age, age_group) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  group_by(NAME, geography, year, age, age_group) |> 
  mutate(total = sum(estimate))  |> 
  mutate(rate = estimate/total) 


combined_data <- rbind(va_data, state_data, cbsa_data) 

write_rds(combined_data, "shiny/02-econ/poverty/age_data.rds")

va_race <- data |> 
  right_join(lookup, by = "GEOID") |> 
  select(NAME = name_long, geography, year, poverty, race, estimate) |> 
  drop_na() |> 
  mutate(race = case_when(
    race == "Native Hawaiian And Other Pacific Islander" ~ "Native Hawaiian/Pacific Islander",
    race == "Black Or African American" ~ "Black",
    race == "Two Or More Races" ~ "Multiracial",
    race == "All householders" ~ "All households",
    race == "White, Not Hispanic or Latino" ~ "White, non-Hispanic",
    race == "American Indian And Alaska Native" ~ "American Indian/Alaska Native",
    TRUE ~ race 
  )) |> 
  group_by(NAME, geography, year, poverty, race) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  group_by(NAME, geography, year, race) |> 
  mutate(total = sum(estimate)) |> 
  mutate(rate = estimate/total)

state_race <- data |> 
  filter(geography == "state") |> 
  select(NAME, geography, year, poverty, race, estimate) |> 
  drop_na() |> 
  mutate(race = case_when(
    race == "Native Hawaiian And Other Pacific Islander" ~ "Native Hawaiian/Pacific Islander",
    race == "Black Or African American" ~ "Black",
    race == "Two Or More Races" ~ "Multiracial",
    race == "All householders" ~ "All households",
    race == "White, Not Hispanic or Latino" ~ "White, non-Hispanic",
    race == "American Indian And Alaska Native" ~ "American Indian/Alaska Native",
    TRUE ~ race 
  )) |> 
  group_by(NAME, geography, year, poverty, race) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup()  |> 
  group_by(NAME, geography, year, race) |> 
  mutate(total = sum(estimate))  |> 
  mutate(rate = estimate/total)

cbsa_race <- data |> 
  filter(geography == "cbsa") |> 
  filter(str_detect(NAME, "VA")) |> 
  select(NAME, geography, year, poverty, race, estimate) |> 
  drop_na() |> 
  mutate(race = case_when(
    race == "Native Hawaiian And Other Pacific Islander" ~ "Native Hawaiian/Pacific Islander",
    race == "Black Or African American" ~ "Black",
    race == "Two Or More Races" ~ "Multiracial",
    race == "All householders" ~ "All households",
    race == "White, Not Hispanic or Latino" ~ "White, non-Hispanic",
    race == "American Indian And Alaska Native" ~ "American Indian/Alaska Native",
    TRUE ~ race 
  )) |> 
  group_by(NAME, geography, year, poverty, race) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  group_by(NAME, geography, year, race) |> 
  mutate(total = sum(estimate))  |> 
  mutate(rate = estimate/total)

combined_data <- rbind(va_race, state_race, cbsa_race) 

write_rds(combined_data, "shiny/02-econ/poverty/race_data.rds")

