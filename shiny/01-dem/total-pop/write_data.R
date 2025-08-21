library(tidyverse)
library(paws)
library(lubridate)

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "pep/pop_data.rds"
)

lookup <- read_csv("data/local_lookup.csv") %>% 
  mutate(GEOID = as.character(fips_full))

# Extract the raw data and deserialize the RDS
total_pop <- readRDS(rawConnection(s3_response$Body)) %>% 
  right_join(lookup, by = "GEOID") %>% 
  mutate(year = as.numeric(year))



write_data(total_pop, "shiny/01-dem/total_pop/total_pop.rds")
