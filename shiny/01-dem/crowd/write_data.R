library(tidyverse)
library(paws)
library(lubridate)

# Write latest data for Tenure by Occupants per Bedroom

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "census/b25014_data.rds"
)

lookup <- read_csv("data/local_lookup.csv") %>% 
  mutate(GEOID = as.character(fips_full))


data <- tryCatch({
  decompressed <- memDecompress(s3_response$Body, type = "gzip")
  readRDS(rawConnection(decompressed))
}, error = function(e) {
  # If decompression fails, try reading directly
  readRDS(rawConnection(s3_response$Body))
}) %>% 
  right_join(lookup, by = "GEOID") %>% 
  mutate(year = as.numeric(year)) %>% 
  drop_na(tenure)

write_rds(data, "shiny/01-dem/crowd/b25014_data.rds")
