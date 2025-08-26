library(tidyverse)
library(paws)
library(lubridate)

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "census/b25004_data.rds"
)

lookup <- read_csv("data/local_lookup.csv") %>% 
  mutate(GEOID = as.character(fips_full))

vacancy <- tryCatch({
  decompressed <- memDecompress(s3_response$Body, type = "gzip")
  readRDS(rawConnection(decompressed))
}, error = function(e) {
  # If decompression fails, try reading directly
  readRDS(rawConnection(s3_response$Body))
}) 

vacancy_data <- vacancy |> 
  right_join(lookup, by = "GEOID") %>%
  filter(!is.na(vacancy)) |> 
  filter(grepl("rent", vacancy, ignore.case = TRUE))



# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "census/b25003_data.rds"
)

lookup <- read_csv("data/local_lookup.csv") %>% 
  mutate(GEOID = as.character(fips_full))

occupied <- tryCatch({
  decompressed <- memDecompress(s3_response$Body, type = "gzip")
  readRDS(rawConnection(decompressed))
}, error = function(e) {
  # If decompression fails, try reading directly
  readRDS(rawConnection(s3_response$Body))
}) 

occupied_data <- occupied |> 
  right_join(lookup, by = "GEOID") %>%
  filter(across(c(tenure), ~!is.na(.x))) %>%
  filter(str_detect(tenure, "Renter")) |> 
  filter(race == "All households")

b25004_data_wide <- vacancy_data %>%
  select(GEOID, name_long, cbsa_title, year, vacancy, estimate) |> 
  pivot_wider(
    names_from = vacancy,
    values_from = estimate)

rental_vacancy <- inner_join(occupied_data, b25004_data_wide, by = c("GEOID", "year"))

output_vacancy <- rental_vacancy %>%
  select(GEOID, NAME, year, estimate, geography, name_long = name_long.x, cbsa_title = cbsa_title.x,
for_rent = `For rent`, rented_unoccupied = `Rented, not occupied`) 

va_vacancy <- output_vacancy |> 
  group_by(year) |> 
  summarise(estimate = sum(estimate),
            for_rent = sum(for_rent),
            rented_unoccupied = sum(rented_unoccupied)) |> 
  ungroup() |> 
  mutate(GEOID = 51,
  NAME = "Virginia",
geography = "state",
name_long = "Virginia",
cbsa_title = "NA")

cbsa_vacancy <- output_vacancy |> 
  group_by(year, cbsa_title) |> 
  summarise(estimate = sum(estimate),
            for_rent = sum(for_rent),
            rented_unoccupied = sum(rented_unoccupied)) |> 
  ungroup() |> 
  mutate(GEOID = NA,
  NAME = cbsa_title,
geography = "cbsa",
name_long = cbsa_title,
cbsa_title = cbsa_title)

vacancy_data <- rbind(output_vacancy, va_vacancy, cbsa_vacancy) |> 
  mutate(total_rental = (estimate + for_rent + rented_unoccupied)) |> 
  mutate(total_vacant = total_rental - estimate) |> 
  mutate(rate = total_vacant/total_rental)

write_rds(vacancy_data, "shiny/05-rental/vacancy/data.rds")

