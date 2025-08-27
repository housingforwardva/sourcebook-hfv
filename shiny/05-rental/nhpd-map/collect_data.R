library(tidyverse)
library(readxl)
library(stringr)

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

library(mapgl)
library(sf)
library(air)


juris <- read_rds("data/va_co_shape.rds")



nhpd <- st_as_sf(properties_with_subsidies, coords = c("longitude", "latitude"), crs = 4326)

map <- mapboxgl(bounds = juris) |> 
  add_fill_layer(
    id = "juris",
    source = juris,
    fill_opacity = 0.1,
    fill_outline_color = "blue",
    hover_options = list(
        fill_color = "#1B365D",
        fill_opacity = 0.8
      ))|> 
  add_circle_layer(
  id = "properties",
  source = nhpd,
circle_color = "green",
circle_radius = 2,
circle_opacity = 0.8,
popup = concat(
      '<div style="background: linear-gradient(135deg, #011E41 0%, #66788d 100%); ',
      'padding: 12px; border-radius: 8px; box-shadow: 0 5px 15px rgba(0,0,0,0.3); ',
      'color: white; font-family: -apple-system, BlinkMacSystemFont, sans-serif; ',
      'max-width: 280px; position: relative;">',


      # Property name with housing icon
      '<h3 style="margin: 0 0 10px 0; font-size: 16px; font-weight: 600; ',
      'display: flex; align-items: center; gap: 6px;">',
      '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">',
      '<path d="M3 9l9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"></path>',
      '<polyline points="9,22 9,12 15,12 15,22"></polyline></svg>',
      get_column("property_name"),
      '</h3>',

      # Address
      '<div style="font-size: 12px; opacity: 0.9; margin-bottom: 10px;">',
      get_column("property_address"), '<br>',
      get_column("city"), ', ', get_column("state"), ' ', get_column("zip_code"),
      '</div>',

      # Status badge
      '<div style="background: rgba(255,255,255,0.2); padding: 6px 8px; border-radius: 4px; ',
      'margin-bottom: 10px; font-size: 11px; font-weight: 600; text-align: center;">',
      'Status: ', get_column("property_status"),
      '</div>',

      # Stats grid - 3 columns
      '<div style="display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 6px; margin-bottom: 10px;">',

      # Max assisted units card
      '<div style="background: rgba(255,255,255,0.15); padding: 6px; border-radius: 4px; text-align: center;">',
      '<div style="font-size: 10px; opacity: 0.8; margin-bottom: 2px;">Max Assisted</div>',
      '<div style="font-size: 14px; font-weight: 600;">',
      get_column("max_assisted_units"),
      '</div>',
      '</div>',

      # Total units card
      '<div style="background: rgba(255,255,255,0.15); padding: 6px; border-radius: 4px; text-align: center;">',
      '<div style="font-size: 10px; opacity: 0.8; margin-bottom: 2px;">Total Units</div>',
      '<div style="font-size: 14px; font-weight: 600;">',
      get_column("total_units"),
      '</div>',
      '</div>',

      # Subsidies count card
      '<div style="background: rgba(255,255,255,0.15); padding: 6px; border-radius: 4px; text-align: center;">',
      '<div style="font-size: 10px; opacity: 0.8; margin-bottom: 2px;">Subsidies</div>',
      '<div style="font-size: 14px; font-weight: 600;">',
      get_column("num_subsidies"),
      '</div>',
      '</div>',
      '</div>',

      # Active subsidies - condensed
      '<div style="margin-bottom: 8px;">',
      '<div style="font-size: 11px; font-weight: 600; margin-bottom: 4px;">Active Subsidies:</div>',
      '<div style="font-size: 10px; background: rgba(255,255,255,0.1); padding: 4px; border-radius: 4px;">',
      get_column("active_subsidies"),
      '</div>',
      '</div>',

      # Data source footer
      '<div style="font-size: 9px; opacity: 0.7; text-align: center; ',
      'padding-top: 6px; border-top: 1px solid rgba(255,255,255,0.2);">',
      'NHPD',
      '</div>',

      '</div>'
    ))
 
  

map
