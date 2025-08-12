library(tidyverse)
library(paws)
library(lubridate)

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "pep/race_ethnicity.rds"
)

lookup <- read_csv("data/local_lookup.csv") %>% 
  mutate(GEOID = as.character(fips_full))

# Extract the raw data and deserialize the RDS
data <- readRDS(rawConnection(s3_response$Body)) %>% 
  right_join(lookup, by = "GEOID") %>% 
  mutate(year = as.numeric(year))

write_rds(data, "shiny/01-dem/pop-race/race_ethnicity.rds")
