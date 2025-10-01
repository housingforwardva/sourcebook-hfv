library(tidyverse)
library(paws)
library(lubridate)

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "census/b25106_data.rds"
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
  drop_na(tenure) |> 
  group_by(year, name_long, cbsa_title, tenure, income, cost_burden) |> 
  summarise(estimate = sum(estimate))

write_rds(va_data, "shiny/06-affordability/cb-acs/data.rds")

