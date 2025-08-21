library(tidyverse)
library(paws)
library(lubridate)

# Write latest data for Tenure by Occupants per Bedroom

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "census/b25010_data.rds"
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
  filter(geography == "county") |> 
  right_join(lookup, by = "GEOID") %>% 
  select(NAME = name_long, geography, year, tenure, estimate) |> 
  mutate(year = as.numeric(year)) 

cbsa_data <- data |> 
  filter(geography == "cbsa") |> 
  filter(str_detect(NAME, "VA")) %>% 
  select(NAME, geography, year, tenure, estimate) |> 
  mutate(year = as.numeric(year)) 

state_data <- data |> 
  filter(geography == "state")%>% 
  filter(NAME == "Virginia") |> 
  select(NAME, geography, year, tenure, estimate) |> 
  mutate(year = as.numeric(year)) 

combined_data <- rbind(va_data, cbsa_data, state_data)

write_rds(combined_data, "shiny/01-dem/hh-size-avg/b25010_data.rds")
