library(tidyverse)
library(paws)
library(lubridate)

# Write latest data for Household Composition

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "pep/pop_change.rds"
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

data_join <- data %>% 
  right_join(lookup, by = "GEOID") %>% 
  mutate(year = as.numeric(year))

write_rds(data_join, "shiny/01-dem/pop-change/pop_change.rds")
