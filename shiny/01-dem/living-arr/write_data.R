library(tidyverse)
library(paws)
library(lubridate)

# Write latest data for Living Arrangements of Adults

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "census/b09021_data.rds"
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
  filter(!is.na(age)) |> 
  filter(age != "All ages") |> 
  group_by(name_long, cbsa_title, year, age, type) |> 
  summarise(estimate = sum(estimate))


write_rds(data, "shiny/01-dem/living-arr/b09021_data.rds")
