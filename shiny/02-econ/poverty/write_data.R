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
  select(NAME = name_long, geography, year, poverty, age, estimate) |> 
  drop_na() |> 
  group_by(NAME, geography, year, poverty, age) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  group_by(NAME, geography, year, age) |> 
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
  select(NAME, geography, year, poverty, age, estimate) |> 
  drop_na() |> 
  group_by(NAME, geography, year, poverty, age) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup()  |> 
  group_by(NAME, geography, year, age) |> 
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
  select(NAME, geography, year, poverty, age, estimate) |> 
  drop_na() |> 
  group_by(NAME, geography, year, poverty, age) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  group_by(NAME, geography, year, age) |> 
  mutate(total = sum(estimate))  |> 
  mutate(rate = estimate/total) 


combined_data <- rbind(va_data, state_data, cbsa_data) 

write_rds(combined_data, "shiny/02-econ/poverty/age_data.rds")
