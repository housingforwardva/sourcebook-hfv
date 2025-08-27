library(tidyverse)
library(tidygeocoder)
library(readxl)

juris <- read_rds("data/va_co_shape.rds")

# Load NHPD data
subsidies <- read_excel("data/xls_csv/nhpd_subsidies_va.xlsx") 
  
va_subsidies <- subsidies |> 
  mutate(fulladdress = paste(subsidies$`Street Address`, subsidies$`City`, subsidies$`State`, subsidies$`Zip Code`, sep = ", ")) |> 
  janitor::clean_names()|> 
  select(fulladdress,
    nhpd_property_id,
         subsidy_status,
         subsidy_name,
         subsidy_subname, 
         start_date,
         end_date,
         assisted_units,
         inactive_status_description,
         construction_type) 

lat_long <- va_subsidies %>%
  geocode(address = fulladdress, method = 'geocodio',
          full_results = TRUE,
          unique_only = FALSE,
          lat = Latitude, 
          long = Longitude)

manual_review <- lat_long |> 
  filter(accuracy < .8 | is.na(accuracy))


write_csv(manual_review, "data/va_subsidies_manual_check.csv")

checked_review <- read_csv("data/va_subsidies_manual_check.csv")

combined_data <- rbind(lat_long, checked_review) |> 
  mutate(name_long = address_components.county) |> 
  left_join(lookup, by = "name_long") |> 
  select(subsidy_status,
         subsidy_name,
         subsidy_subname, 
         start_date,
         end_date,
         assisted_units,
         inactive_status_description,
         construction_type,
         name_long,
         cbsa_title) |> 
  mutate(assisted_units = case_when(
    subsidy_status == "Inactive" ~ -(assisted_units),
    TRUE ~ assisted_units
  )) |> 
  mutate(subsidy_status = case_when(
    subsidy_status == "Inconclusive" ~ "Active/Inconclusive",
    subsidy_status == "Active" ~ "Active/Inconclusive",
    TRUE ~ subsidy_status
  ))


write_rds(combined_data, "shiny/05-rental/nhpd/data.rds")
