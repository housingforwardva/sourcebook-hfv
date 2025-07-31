library(tidyverse)
library(readxl)

# Download subsidy-level data from National Housing Preservation Datbase, 
# filtering for State = "VA"

subsidies <- read_excel("data/xls_csv/nhpd_subsidies_va.xlsx") |> 
  janitor::clean_names() |> 
  select(nhpd_property_id,
         subsidy_status,
         subsidy_name,
         subsidy_subname, 
         start_date,
         end_date,
         assisted_units,
         inactive_status_description,
         construction_type) 

properties <- read_excel("data/xls_csv/nhpd_properties_va.xlsx") |> 
  janitor::clean_names() |> 
  select(1:27) 
  
         
properties_with_subsidies <- properties %>%
  left_join(
    subsidies %>%
      group_by(nhpd_property_id) %>%
      summarise(
        # Count of subsidies
        num_subsidies = n(),
        
        # Concatenated subsidy names
        subsidy_names = paste(unique(subsidy_name), collapse = "; "),
        
        # Concatenated subsidy subnames  
        subsidy_subnames = paste(unique(subsidy_subname), collapse = "; "),
        
        # Active subsidies only
        active_subsidies = paste(subsidy_name[subsidy_status == "Active"], 
                                 collapse = "; "),
        
        # Maximum assisted units (to avoid double-counting layered subsidies)
        max_assisted_units = max(assisted_units, na.rm = TRUE),
        
        # Range of assisted units across subsidies
        min_assisted_units = min(assisted_units, na.rm = TRUE),
        
        # Note: Don't sum assisted_units as subsidies may layer over same units
        
        # Date ranges
        earliest_start = min(start_date, na.rm = TRUE),
        latest_end = max(end_date, na.rm = TRUE),
        
        .groups = "drop"
      ),
    by = "nhpd_property_id"
  )