library(tidyverse)
library(paws)
library(lubridate)

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "census/b25032_data.rds"
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
  left_join(lookup, by = "GEOID") |> 
  filter(tenure != "All households") |> 
  filter(structure != "All units") |> 
  mutate(structure = case_when(
    structure == "2" ~ "2 to 4", 
    structure == "3 or 4" ~ "2 to 4",
    TRUE ~ structure
  )) |> 
  group_by(year, tenure, name_long, cbsa_title, structure) |> 
  summarise(estimate = sum(estimate))

write_rds(va_data, "shiny/03-supply/housing-type/data.rds")


state <- va_data%>% 
  group_by(year, cbsa_title, tenure, structure) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>% 
  group_by(year, cbsa_title, tenure) %>% 
  mutate(percent = estimate/sum(estimate)) %>% 
  group_by(year, cbsa_title) %>% 
  mutate(percent_total = estimate/sum(estimate))
