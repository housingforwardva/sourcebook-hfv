# Pull Median Income by Age data from S3 for Sourcebook ---------------------------------------------------

library(tidyverse)
library(paws)
library(lubridate)

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "census/b19049_data.rds"
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
  drop_na(age) |> 
  select(NAME = name_long, geography, year, age, estimate, adjusted)

state_data <- data |> 
  filter(geography == "state") |> 
  drop_na(age) |> 
  select(NAME, geography, year, age, estimate, adjusted)

cbsa_data <- data |> 
  filter(geography == "cbsa") |> 
  drop_na(age) |> 
  filter(str_detect(NAME, "VA")) |> 
  select(NAME, geography, year, age, estimate, adjusted)

combined_data <- rbind(va_data, state_data, cbsa_data) 

write_rds(combined_data, "shiny/02-econ/med-inc-age/data.rds")

