library(tidyverse)
library(paws)
library(lubridate)

# Write latest data for Tenure by Occupants per Bedroom

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "census/b25009_data.rds"
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
  drop_na(tenure)%>% 
  mutate(hhsize = case_when(
    hhsize == "4-person" ~ "4 or more person",
    hhsize == "5-person" ~ "4 or more person",
    hhsize == "6-person" ~ "4 or more person",
    hhsize == "7-or-more person" ~ "4 or more person",
    TRUE ~ hhsize
  )) %>% 
  filter(hhsize != "All sizes")

write_rds(data, "shiny/01-dem/hh-size/b25009_data.rds")
