# Pull Median Income by Race data from S3 for Sourcebook ---------------------------------------------------

library(tidyverse)
library(paws)
library(lubridate)

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "census/b19013_data.rds"
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
  drop_na(race) |> 
  select(NAME = name_long, geography, year, race, estimate, adjusted) |> 
  filter(race != "All households")

state_data <- data |> 
  filter(geography == "state") |> 
  drop_na(race) |> 
  select(NAME, geography, year, race, estimate, adjusted) |> 
  filter(race != "All households")

cbsa_data <- data |> 
  filter(geography == "cbsa") |> 
  drop_na(race) |> 
  filter(str_detect(NAME, "VA")) |> 
  select(NAME, geography, year, race, estimate, adjusted) |> 
  filter(race != "All households")

combined_data <- rbind(va_data, state_data, cbsa_data) |> 
  mutate(race = case_when(
    race == "White Alone, Not Hispanic or Latino" ~ "White, non-Hispanic",
    race == "Black or African American" ~ "Black",
    race == "Two or More Races" ~ "Multiracial",
    TRUE ~ race))


write_rds(combined_data, "shiny/02-econ/med-inc-race/data.rds")
