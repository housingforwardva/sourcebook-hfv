library(tidyverse)
library(paws)
library(lubridate)

# Write latest data for Household Composition

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "census/b11012_data.rds"
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
  mutate(type != "All households") %>% 
  mutate(type = case_when(
    type == "Married-couple household" ~ "Married or cohabitating coupple",
    type == "Cohabiting couple household" ~ "Married or cohabitating coupple",
    TRUE ~ "Householder with no partner"
  )) %>% 
  filter(!str_detect(sub, "All"))

write_rds(data, "shiny/01-dem/hh-type/b11012_data.rds")
