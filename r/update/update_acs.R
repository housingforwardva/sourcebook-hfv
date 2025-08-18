library(tidycensus)
library(tidyverse)
library(paws)

source("r/helper_functions.R")

## TABLE B25118 - Tenure by Household Income  ----------------------------------

b25118_vars <- load_table_vars("B25118", 2010) %>% 
  separate(label, into = c("est", "total", "tenure", "income"), sep = "!!") %>% 
  drop_na() %>% 
  select(variable = name, tenure, income) %>% 
  mutate(tenure = case_when(
    str_detect(tenure, "Owner") ~ "Homeowner",
    TRUE ~ "Renter"
  )) %>% 
  mutate(income_range = case_when(
      income %in% c("Less than $5,000", "$5,000 to $9,999") ~ "Under $10,000",
      income %in% c("$10,000 to $14,999", "$15,000 to $19,999") ~ "$10,000 to $19,999",
      income %in% c("$20,000 to $24,999", "$25,000 to $34,999") ~ "$20,000 to $34,999",
      income == "$35,000 to $49,999" ~ "$35,000 to $49,999",
      income == "$50,000 to $74,999" ~ "$50,000 to $74,999",
      TRUE ~ income
  )) 


# PULL RAW ACS DATA ------------------------------------------------------------

# Set requested years
requested_years <- 2010:2023
survey <- "acs5"

# Define all tables including race variants
tables <- c(
  "B25118"  # Tenure by Household Income
)

## COUNTY DATA PULL --------------------------------------------------------------

message("Pulling county data...")

county_data <- map(tables, function(table) {
  valid_years <- get_available_years(table, requested_years)
  if (!is.null(valid_years)) {
    get_va_acs(table, "county", valid_years, survey)
  } else {
    NULL
  }
}) %>%
  set_names(tables)

# Remove NULL results
county_data <- compact(county_data)

## STATE DATA PULL -------------------------------------------------------------

message("Pulling state data...")

state_data <- map(tables, function(table) {
  valid_years <- get_available_years(table, requested_years)
  if (!is.null(valid_years)) {
    get_state_acs(table, valid_years, "acs5")
  } else {
    NULL
  }
}) %>%
  set_names(tables)

# Remove NULL results
state_data <- compact(state_data)

## CBSA DATA PULL --------------------------------------------------------------

message("Pulling CBSA data...")

cbsa_data <- map(tables, function(table) {
  valid_years <- get_available_years(table, requested_years)
  if (!is.null(valid_years)) {
    get_cbsa_acs(table, valid_years, "acs5")
  } else {
    NULL
  }
}) %>%
  set_names(tables)

# Remove NULL results
cbsa_data <- compact(cbsa_data)


b25118_data <- safe_process_data(county_data, state_data, cbsa_data, "B25118", b25118_vars)


# Upload to S3 bucket
s3 <- paws::s3()

temp_file <- tempfile(fileext = ".rds")
write_rds(b25118_data, temp_file)
s3$put_object(
  Bucket = "hda-data-hub",
  Key = "census/b25118_data.rds",
  Body = temp_file
)
file.remove(temp_file)



