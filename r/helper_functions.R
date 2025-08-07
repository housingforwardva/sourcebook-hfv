# Helper functions --------------------------------------------------------

# Load required packages
library(tidyverse)
library(tidycensus)
library(fredr)
library(lubridate)

# ADD THIS: Define current ACS year if not already defined
current_acs <- 2023


# Define year availability for each table
table_years <- list(
  "B11001" = 2010:2023,
  "B11012" = c(2010:2014, 2019:2023),  # Gap in 2015-2018
  "B09021" = 2015:2023,                # Only available from 2015
  "B25003" = 2010:2023,
  "B25004" = 2010:2023,
  "B25007" = 2010:2023,
  "B25009" = 2010:2023,
  "B25010" = 2010:2023,
  "B25042" = 2010:2023,
  "B25032" = 2010:2023,
  "B25127" = 2010:2023,
  "B25063" = 2010:2023,
  "B25118" = 2010:2023,
  "B25014" = 2010:2023,
  "B17001" = 2010:2023,
  "B25106" = 2010:2023,
  "B19049" = 2010:2023,
  "B25119" = 2010:2023,
  "B19013" = 2010:2023,
  "B25064" = 2010:2023,
  "B25031" = 2015:2023,                # Only available from 2015
  "B25058" = 2010:2023
)

# Function to get available years for a table
get_available_years <- function(table_name, requested_years) {
  base_table <- str_extract(table_name, "^[A-Z][0-9]+")
  
  if (base_table %in% names(table_years)) {
    available <- table_years[[base_table]]
    valid_years <- intersect(requested_years, available)
    
    if (length(valid_years) == 0) {
      warning(paste("No data available for table", table_name, "in requested years:", 
                    paste(requested_years, collapse = ", ")))
      return(NULL)
    }
    
    missing_years <- setdiff(requested_years, available)
    if (length(missing_years) > 0) {
      message(paste("Table", table_name, "- Data not available for years:", 
                    paste(missing_years, collapse = ", "), 
                    "| Available years:", paste(valid_years, collapse = ", ")))
    }
    
    return(valid_years)
  } else {
    # Default to requested years if not specified
    return(requested_years)
  }
}

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

# ENHANCED: Add error handling to your existing get_va_acs function
get_va_acs <- function(table_name, geography, years_to_use, srvy) {
  # Filter out invalid years first
  valid_years <- years_to_use[!is.na(years_to_use) & years_to_use >= 2005 & years_to_use <= 2023]
  
  if (length(valid_years) == 0) {
    warning(paste("No valid years provided for table", table_name))
    return(NULL)
  }
  
  map_dfr(valid_years, function(yr) {
    tryCatch({
      get_acs(
        geography = geography,
        state = "VA",
        table = table_name,
        year = yr,
        survey = srvy,
        cache_table = TRUE
      ) %>%
        mutate(year = yr)
    }, error = function(e) {
      message(paste("Failed to get data for", table_name, "year", yr, ":", e$message))
      return(NULL)
    })
  })
}

# ENHANCED: Add error handling to get_county_acs function
get_county_acs <- function(table_name, years_to_use) {
  valid_years <- years_to_use[!is.na(years_to_use) & years_to_use >= 2005 & years_to_use <= 2023]
  
  if (length(valid_years) == 0) {
    warning(paste("No valid years provided for table", table_name))
    return(NULL)
  }
  
  map_dfr(valid_years, function(yr) {
    tryCatch({
      get_acs(
        geography = "county",
        table = table_name,
        year = yr,
        survey = "acs5",
        cache_table = TRUE
      ) %>%
        mutate(year = yr)
    }, error = function(e) {
      message(paste("Failed to get county data for", table_name, "year", yr, ":", e$message))
      return(NULL)
    })
  })
}

# ENHANCED: Add error handling to get_cbsa_acs function
get_cbsa_acs <- function(table_name, years_to_use, srvy) {
  valid_years <- years_to_use[!is.na(years_to_use) & years_to_use >= 2005 & years_to_use <= 2023]
  
  if (length(valid_years) == 0) {
    warning(paste("No valid years provided for table", table_name))
    return(NULL)
  }
  
  map_dfr(valid_years, function(yr) {
    tryCatch({
      get_acs(
        geography = "metropolitan statistical area/micropolitan statistical area",
        table = table_name,
        year = yr,
        survey = srvy,
        cache_table = TRUE
      ) %>%
        mutate(year = yr)
    }, error = function(e) {
      message(paste("Failed to get CBSA data for", table_name, "year", yr, ":", e$message))
      return(NULL)
    })
  })
}

# ENHANCED: Add error handling to get_state_acs function
get_state_acs <- function(table_name, years_to_use, srvy) {
  valid_years <- years_to_use[!is.na(years_to_use) & years_to_use >= 2005 & years_to_use <= 2023]
  
  if (length(valid_years) == 0) {
    warning(paste("No valid years provided for table", table_name))
    return(NULL)
  }
  
  map_dfr(valid_years, function(yr) {
    tryCatch({
      get_acs(
        geography = "state",
        table = table_name,
        year = yr,
        survey = srvy,
        cache_table = TRUE
      ) %>%
        mutate(year = yr)
    }, error = function(e) {
      message(paste("Failed to get state data for", table_name, "year", yr, ":", e$message))
      return(NULL)
    })
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
  summarise(index = mean(value))

# Store current index values for reference
current_index <- cpi %>% filter(year == current_acs) %>% pull(index)
current_rent_index <- cpi_rent %>% filter(year == current_acs) %>% pull(index)

# Function to adjust for inflation using rent CPI
adjust_for_rent_inflation <- function(data) {
  data %>%
    left_join(cpi_rent, by = "year") %>%
    mutate(adjusted = ((current_rent_index/index) * estimate))
}

# Function to join with CPI and adjust for inflation
process_median_income <- function(data) {
  data %>%
    left_join(cpi, by = "year") %>%
    mutate(adjusted = ((current_index/index) * estimate))
}

# Function to standardize reliability metrics
add_reliability_metrics <- function(data) {
  data %>%
    mutate(
      cv = ((moe/1.645)/estimate) * 100,
      reliability = case_when(
        cv < 15 ~ "High",
        cv >= 15 & cv <= 30 ~ "Medium",
        cv > 30 ~ "Low"
      )
    )
  
}
  # Function to safely combine race variants
  combine_race_variants <- function(base_table, data_list) {
    variants <- c(base_table, paste0(base_table, LETTERS[2:8]))
    
    combined_data <- map(variants, ~data_list[[.x]]) %>% 
      compact() %>% 
      bind_rows()
    
    if (nrow(combined_data) == 0) {
      return(NULL)
    }
    
    return(combined_data)
  }
  
  # Function to safely process data if it exists
  safe_process_data <- function(county_data, state_data, cbsa_data, table_name, vars_df, process_func = NULL) { {
    
    # Initialize data list
    data_list <- list()
    
    # Add county data if exists
    if (!is.null(county_data[[table_name]])) {
      data_list$county <- county_data[[table_name]] %>%
        left_join(vars_df, by = "variable") %>%
        mutate(geography = "county")
    }
    
    # Add state data if exists
    if (!is.null(state_data[[table_name]])) {
      data_list$state <- state_data[[table_name]] %>%
        left_join(vars_df, by = "variable") %>%
        mutate(geography = "state")
    }
    
    # Add CBSA data if exists
    if (!is.null(cbsa_data[[table_name]])) {
      data_list$cbsa <- cbsa_data[[table_name]] %>%
        left_join(vars_df, by = "variable") %>%
        mutate(geography = "cbsa")
    }
    
    # Check if we have any data
    if (length(data_list) == 0) {
      warning(paste("No data available for table", table_name, "across all geographies"))
      return(NULL)
    }
    
    # Combine all available data
    result <- bind_rows(data_list) %>%
      add_reliability_metrics()
    
    # Apply additional processing if function provided
    if (!is.null(process_func)) {
      result <- process_func(result)
    }
    
    return(result)
  }
}