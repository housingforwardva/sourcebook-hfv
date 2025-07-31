# Load required packages
library(readxl)
library(dplyr)
library(httr)

# Create vectors for years and corresponding URL patterns
years <- 2017:2025
il_codes <- paste0("il", substr(years, 3, 4))  # il17, il18, etc.
fy_codes <- paste0("FY", substr(years, 3, 4))  # FY17, FY18, etc.

# Create base URL pattern
base_url <- "https://www.huduser.gov/portal/datasets/il/"

# Initialize empty list to store data frames
data_list <- list()

# Download and read each file
for (i in seq_along(years)) {
  year <- years[i]
  il_code <- il_codes[i]
  fy_code <- fy_codes[i]
  
  # Construct URL - try the pattern from your examples
  url <- paste0(base_url, il_code, "/Section8-", fy_code, ".xlsx")
  
  # Print URL to debug
  cat("Trying URL:", url, "\n")
  
  # Create temporary file
  temp_file <- tempfile(fileext = ".xlsx")
  
  cat("Downloading", year, "data...\n")
  
  # Download file
  tryCatch({
    download.file(url, temp_file, mode = "wb", quiet = TRUE)
    
    # Read the Excel file
    # Note: You may need to adjust sheet name or skip rows based on file structure
    data <- read_excel(temp_file)
    
    # Find and rename the median column for this year
    median_col_name <- paste0("median", year)
    
    if (median_col_name %in% names(data)) {
      # Rename the year-specific median column to just "median"
      names(data)[names(data) == median_col_name] <- "median"
      cat("Renamed", median_col_name, "to 'median'\n")
    } else {
      # Check if there's any column that starts with "median"
      median_cols <- names(data)[grepl("^median", names(data), ignore.case = TRUE)]
      if (length(median_cols) > 0) {
        names(data)[names(data) == median_cols[1]] <- "median"
        cat("Found and renamed", median_cols[1], "to 'median'\n")
      } else {
        cat("Warning: No median column found for", year, "\n")
      }
    }
    
    # Standardize area name column - check for various possible names
    area_name_variants <- c("hud_area_name", "Metro_Area_Name", "area_name", "metro_area", "HUD_Area_Name")
    
    for (variant in area_name_variants) {
      if (variant %in% names(data)) {
        names(data)[names(data) == variant] <- "area_name"
        cat("Renamed", variant, "to 'area_name'\n")
        break
      }
    }
    
    st_variants <- c("State_Alpha", "stusps")
    
    for (variant in st_variants) {
      if (variant %in% names(data)) {
        names(data)[names(data) == variant] <- "state_abbrev"
        cat("Renamed", variant, "to 'state_abbrev'\n")
        break
      }
    }
    
    # Check if we successfully found an area name column
    if (!"area_name" %in% names(data)) {
      # Look for any column with "area" in the name
      area_cols <- names(data)[grepl("area", names(data), ignore.case = TRUE)]
      if (length(area_cols) > 0) {
        names(data)[names(data) == area_cols[1]] <- "area_name"
        cat("Found and renamed", area_cols[1], "to 'area_name'\n")
      } else {
        cat("Warning: No area name column found for", year, "\n")
      }
    }
    
    # Add year column
    data$year <- year
    
    # Store in list
    data_list[[as.character(year)]] <- data
    
    cat("Successfully downloaded and processed", year, "data\n")
    
    # Clean up temp file
    unlink(temp_file)
    
  }, error = function(e) {
    cat("Error downloading", year, "data:", e$message, "\n")
    unlink(temp_file)
  })
}

# Combine all data frames
if (length(data_list) > 0) {
  combined_data <- bind_rows(data_list)
  cat("Combined data has", nrow(combined_data), "rows and", ncol(combined_data), "columns\n")
  cat("Years included:", sort(unique(combined_data$year)), "\n")
} else {
  cat("No data was successfully downloaded\n")
}

# View structure of combined data
str(combined_data)


va_hud_ami <- combined_data |> 
  janitor::clean_names() |> 
  filter(state_abbrev == "VA") |> 
  pivot_longer(9:32,
               names_to = "income",
               values_to = "limit") |> 
  select(-c(state_2, county_2, fips, hud_area_code)) |> 
  mutate(ami = case_when(
    str_detect(income, "l50") ~ "Very low-income",
    str_detect(income, "l80") ~ "Low-income",
    str_detect(income, "eli") ~ "Extremely low-income"
  )) |> 
  mutate(hh_size = case_when(
    str_detect(income, "_1") ~ "One-person",
    str_detect(income, "_2") ~ "Two-person",
    str_detect(income, "_3") ~ "Three-person",
    str_detect(income, "_4") ~ "Four-person",
    str_detect(income, "_5") ~ "Five-person",
    str_detect(income, "_6") ~ "Six-person",
    str_detect(income, "_7") ~ "Seven-person",
    str_detect(income, "_8") ~ "Eight-person"
  ))
  
write_rds(va_hud_ami, "data/rds/va_hud_ami.rds")

# Optional: Save combined data
# write.csv(combined_data, "section8_income_limits_2017_2025.csv", row.names = FALSE)