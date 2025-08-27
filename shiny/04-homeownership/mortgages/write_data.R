library(tidyverse)
library(paws)
library(arrow)

s3 <- s3()

# Debug: Check what we actually downloaded
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "cfpb/hmda_va_clean.parquet"
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

va_data <- data  |> 
  select(year = activity_year, lei, GEOID = county_code, race_ethnicity, action_taken, purchaser_type, loan_purpose,
         occupancy_type)|> 
  mutate(count = 1) |> 
  group_by(year, GEOID, race_ethnicity, action_taken, loan_purpose, occupancy_type) |> 
  summarise(count = sum(count)) %>% 
  ungroup() |> 
  left_join(lookup, by = "GEOID") |> 
  filter(state == "Virginia")



write_rds(va_data, "shiny/04-homeownership/mortgages/data.rds")
