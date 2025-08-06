# Helper functions --------------------------------------------------------


# Load required packages
library(tidyverse)
library(tidycensus)
library(fredr)
library(lubridate)


# Function to load variables for a specific table
load_table_vars <- function(table_name, year = current_acs, survey = "acs5") {
  load_variables(year, survey) %>%
    filter(str_detect(name, table_name))
}


# Function to convert variable to race or ethnicity
tenure_race <- function(x) {
  x %>%
    str_remove_all(regex("HOUSEHOLDER", ignore_case = TRUE)) %>%
    str_remove_all(regex("TENURE \\(|\\)", ignore_case = TRUE)) %>%
    str_to_title() %>%
    str_replace_all("And", "and") %>%
    str_replace_all("Or", "or") %>%
    str_remove_all(" Alone")
}


# Function to convert variable to race or ethnicity
hh_race <- function(x) {
  x %>% 
    str_extract("[^(]+(?=\\)$)") %>% 
    str_remove_all(" Alone")
}

# Function to convert variable to race or ethnicity
structure_race <- function(x) {
  x %>% 
    str_extract("[^(]+(?=\\)$)") %>% 
    str_remove_all(" Alone") %>% 
    str_remove_all(" Householder")
}


# Function to create race and ethnicity category
poverty_race <- function(x) {
  x %>%
    str_remove_all(regex("POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE \\(|\\)", ignore_case = TRUE)) %>%
    str_to_title() %>% 
    str_remove_all(" Alone")
}


# Function to get ACS data for a specific table
# geography options = "tract", "block group", "block", "county", 
# "state legislative district (upper chamber)", ""state legislative district (lower chamber)",
# "zcta", "congressional district"

get_va_acs <- function(table_name, geography, years_to_use, srvy) {
  map_dfr(years_to_use, function(yr) {
    get_acs(
      geography = geography,
      state = "VA",
      table = table_name,
      year = yr,
      survey = srvy,
      cache_table = TRUE
    ) %>%
      mutate(year = yr)
  })
}

# Function to get county ACS data for a specific table
get_county_acs <- function(table_name, years_to_use = years) {
  map_dfr(years_to_use, function(yr) {
    get_acs(
      geography = "county",
      table = table_name,
      year = yr,
      survey = "acs5",
      cache_table = TRUE
    ) %>%
      mutate(year = yr)
  })
}

# Function to get ACS data for a specific table
get_cbsa_acs <- function(table_name, years_to_use = years) {
  map_dfr(years_to_use, function(yr) {
    get_acs(
      geography = "metropolitan statistical area/micropolitan statistical area",
      table = table_name,
      year = yr,
      survey = "acs5",
      cache_table = TRUE
    ) %>%
      mutate(year = yr)
  })
}


# Function to get ACS data for a specific table
get_state_acs <- function(table_name, years_to_use = years) {
  map_dfr(years_to_use, function(yr) {
    get_acs(
      geography = "state",
      table = table_name,
      year = yr,
      survey = "acs5",
      cache_table = TRUE
    ) %>%
      mutate(year = yr)
  })
}





# Get CPI for All Urban Consumers for income inflation adjustment

cpi <- fredr(
  series_id = "CPIAUCSL"
) %>%
  select(date, value) %>%
  mutate(
    date = as.Date(date),
    value = as.numeric(value),
    year = year(date)
  ) %>%
  group_by(year) %>%
  summarise(index = mean(value))

# Get CPI for rent inflation adjustment
cpi_rent <- fredr(
  series_id = "CUSR0000SEHA"
) %>%
  select(date, value) %>%
  mutate(
    date = as.Date(date),
    value = as.numeric(value),
    year = year(date)
  ) %>%
  group_by(year) %>%
  summarise(cpi = mean(value))



