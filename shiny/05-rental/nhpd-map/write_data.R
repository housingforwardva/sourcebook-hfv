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

combined_data <- rbind(lat_long, checked_review)





properties <- read_excel("../../../data/xls_csv/nhpd_properties_va.xlsx") |> 
  janitor::clean_names() |> 
  select(1:27) 
         
properties_with_subsidies <- properties %>%
  left_join(
    subsidies %>%
      group_by(nhpd_property_id) %>%
      summarise(
        num_subsidies = n(),
        subsidy_names = paste(unique(subsidy_name), collapse = "; "),
        subsidy_subnames = paste(unique(subsidy_subname), collapse = "; "),
        active_subsidies = paste(subsidy_name[subsidy_status == "Active"], 
                                 collapse = "; "),
        max_assisted_units = max(assisted_units, na.rm = TRUE),
        min_assisted_units = min(assisted_units, na.rm = TRUE),
        earliest_start = min(start_date, na.rm = TRUE),
        latest_end = max(end_date, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "nhpd_property_id"
  )

# Convert to sf object
nhpd <- st_as_sf(properties_with_subsidies, coords = c("longitude", "latitude"), crs = 4326)