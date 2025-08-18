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


va_race <- data |> 
  right_join(lookup, by = "GEOID") |> 
  select(NAME = name_long, geography, year, poverty, race, estimate) |> 
  drop_na() |> 
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
  group_by(NAME, geography, year, poverty, race) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  group_by(NAME, geography, year, race) |> 
  mutate(total = sum(estimate))  |> 
  mutate(rate = estimate/total)

combined_data <- rbind(va_race, state_race, cbsa_race) 

write_rds(combined_data, "shiny/02-econ/poverty/race_data.rds")
