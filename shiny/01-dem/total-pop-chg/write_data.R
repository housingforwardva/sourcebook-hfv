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




write_data(total_pop, "shiny/01-dem/total-pop-chg/total_pop.rds")



# Load the data
total_pop <- read_rds("shiny/01-dem/total-pop-chg/total_pop.rds")
  
calculate_pop_changes <- function(data) {
  data %>% 
    mutate(
      diff = value - lag(value),
      diff = replace_na(diff, 0),
      run_diff = cumsum(diff),
      pct = run_diff / value[1]  # Explicitly use first row
    ) 
}
  
  # Pre-compute datasets
  state_data <- total_pop %>% 
      group_by(year, counttype) %>% 
      summarise(value = sum(value), .groups = "drop") %>% 
      ungroup() |> 
  mutate(diff = estimate - lag(estimate),
         diff = replace_na(diff, 0))


  
  cbsa_data <- total_pop %>% 
    group_by(year, cbsa_title, counttype) %>% 
    summarise(value = sum(value), .groups = "drop") %>% 
    ungroup() |> 
    group_by(cbsa_title) |> 
    mutate(diff = value - lag(value),
         diff = replace_na(diff, 0),
          run_diff = cumsum(diff),
          pct = run_diff / value[1])  # Explicitly use first row
