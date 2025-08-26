library(tidyverse)
library(paws)
library(lubridate)

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "census/b25119_data.rds"
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
  select(NAME = name_long, geography, year, tenure, estimate, adjusted)

state_data <- data |> 
  filter(geography == "state") |> 
  select(NAME, geography, year, tenure, estimate, adjusted) 

cbsa_data <- data |> 
  filter(geography == "cbsa") |> 
  filter(str_detect(NAME, "VA")) |> 
  select(NAME, geography, year, tenure, estimate, adjusted) 

combined_data <- rbind(va_data, state_data, cbsa_data) |> 
  mutate(tenure = case_when(
    is.na(tenure) ~ "All households",
    TRUE ~ tenure
  ))

write_rds(combined_data, "shiny/02-econ/med-inc-tenure/data.rds")
