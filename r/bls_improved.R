################################################################################
# BLS Data Processing Script
# Purpose: Process QCEW and OEWS data for Virginia housing analysis
################################################################################

## Setup ------------------------------

# Load required libraries

library(tidyverse)
library(glue)
library(readxl)
library(janitor)
library(httr2)

# Load locality table

local_lookup <- read_csv("data/local_lookup.csv") |> 
  rename(fips = fips_full)


## BLS reference tables ---------------

# Read in as needed
# See ref/bls/qcew/readme.md

# Ownership Titles (values for for own_code field)
# 
# own_code <- read_csv("ref/bls/qcew/ownership-titles-csv.csv)

# Aggregation Level Codes (values for for agglvl_code field)
# 
# agglvl_code <- read_csv("ref/bls/qcew/agg-level-titles-csv.csv)


## get_qcew_data() --------------------

#' Get QCEW (Quarterly Census of Employment and Wages) data
#'
#' This function retrieves QCEW data from the BLS API for specified areas, years,
#' and quarters. It allows filtering by ownership and aggregation level codes,
#' and can join with local lookup data.
#'
#' @param years Numeric vector of years to retrieve data for (required)
#' @param qtr Either "a" for annual data or a vector of quarters 1-4 (default: c(1:4))
#' @param fips Vector of 5-digit FIPS codes for localities (required)
#' @param fields Optional vector of column names to retain (defaults based on qtr)
#' @param own_code Optional ownership code(s) to filter by (0-5, 8, 9)
#' @param agglvl_code Optional aggregation level code(s) to filter by (default: 70)
#' @param join_lookup Optional vector of field names from local_lookup to join
#' @param local_lookup Data frame containing lookup table with FIPS codes
#'
#' @return A tibble containing the requested QCEW data
#'
get_qcew_data <- function(
    years = NULL,
    qtr = c(1:4),
    fips = NULL,
    fields = NULL,
    own_code = NULL,
    agglvl_code = 70,
    join_lookup = NULL,
    local_lookup = NULL
) {
  # Check required packages
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required. Please install it.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it.")
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package 'purrr' is required. Please install it.")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Please install it.")
  }
  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("Package 'glue' is required. Please install it.")
  }
  
  # Import required functions
  `%>%` <- dplyr::`%>%`
  
  # Input validation
  if (is.null(years)) {
    stop("Please provide one or more years to retrieve data")
  }
  
  if (is.null(fips)) {
    stop("Please provide one or more FIPS codes")
  }
  
  # Handle quarterly vs annual data
  is_annual <- FALSE
  if (length(qtr) == 1 && (qtr == "a" || qtr == "A")) {
    is_annual <- TRUE
    qtr <- "a"  # Ensure lowercase for API
  } else {
    # For quarterly data, ensure quarters are in the correct format (1-4)
    qtr <- as.character(qtr)
    if (!all(qtr %in% c("1", "2", "3", "4"))) {
      stop("Quarterly values must be 1, 2, 3, or 4")
    }
  }
  
  # Define default fields based on quarterly or annual
  default_qtr_fields <- c("area_fips", "year", "qtr", "own_code", "industry_code", "agglvl_code", 
                          "qtrly_estabs", "month1_emplvl", "month2_emplvl", "month3_emplvl", "avg_wkly_wage")
  
  default_annual_fields <- c("area_fips", "year", "qtr", "own_code", "industry_code", "agglvl_code", 
                             "annual_avg_estabs", "annual_avg_emplvl", "annual_avg_wkly_wage", "avg_annual_pay")
  
  # Set fields based on input or default
  if (is.null(fields)) {
    fields <- if (is_annual) default_annual_fields else default_qtr_fields
  }
  
  # Helper function to get QCEW data for a specific area, year, and quarter
  get_area_data <- function(year, qtr, area) {
    url <- glue::glue("http://data.bls.gov/cew/data/api/{year}/{qtr}/area/{toupper(area)}.csv")
    
    message(glue::glue("    Fetching: {url}"))
    
    # Use httr2 to fetch the data
    response <- tryCatch({
      httr2::request(url) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()
    }, error = function(e) {
      message(glue::glue("    Error accessing URL: {e$message}"))
      return(NULL)
    })
    
    # Process the response
    if(!is.null(response) && httr2::resp_status(response) == 200) {
      tryCatch({
        content <- httr2::resp_body_string(response)
        data <- readr::read_csv(content, na = " ", show_col_types = FALSE)
        
        # Add year and qtr if they don't exist in the data
        if (!"year" %in% names(data)) {
          data <- data |> dplyr::mutate(year = year)
        }
        if (!"qtr" %in% names(data)) {
          data <- data |> dplyr::mutate(qtr = qtr)
        }
        
        message(glue::glue("    Successfully retrieved {nrow(data)} rows"))
        return(data)
      }, error = function(e) {
        message(glue::glue("    Error parsing CSV: {e$message}"))
        return(NULL)
      })
    } else {
      status <- if(!is.null(response)) httr2::resp_status(response) else "NA"
      message(glue::glue("    Failed with status code: {status}"))
      return(NULL)
    }
  }
  
  # Process for each area and year/quarter combination
  all_data <- purrr::map(fips, function(area) {
    message(glue::glue("Processing area: {area}"))
    
    # Process for each year
    year_data <- purrr::map(years, function(yr) {
      message(glue::glue("  Processing year: {yr}"))
      
      # Process each quarter (or annual)
      qtr_data <- purrr::map(qtr, function(q) {
        period_label <- if(q == "a") "annual" else toupper(q)
        message(glue::glue("    Processing {period_label} data"))
        
        qcew_pull <- get_area_data(yr, q, area)
        
        if(is.null(qcew_pull) || nrow(qcew_pull) == 0) {
          return(NULL)
        }
        
        # Apply filters
        filtered_data <- qcew_pull
        
        # Filter by own_code if specified
        if (!is.null(own_code)) {
          filtered_data <- filtered_data |> dplyr::filter(own_code %in% !!own_code)
          
          if (nrow(filtered_data) == 0) {
            message(glue::glue("    No data after own_code filter"))
            return(NULL)
          }
        }
        
        # Filter by agglvl_code if specified
        filtered_data <- filtered_data |> dplyr::filter(agglvl_code %in% !!agglvl_code)
        
        if (nrow(filtered_data) == 0) {
          message(glue::glue("    No data after agglvl_code filter"))
          return(NULL)
        }
        
        # Select requested fields
        if (all(fields %in% names(filtered_data))) {
          filtered_data <- filtered_data |> dplyr::select(dplyr::all_of(fields))
        } else {
          missing_fields <- setdiff(fields, names(filtered_data))
          if (length(missing_fields) > 0) {
            message(glue::glue("    Warning: Missing fields: {paste(missing_fields, collapse = ', ')}"))
          }
          filtered_data <- filtered_data |> dplyr::select(dplyr::any_of(fields))
        }
        
        message(glue::glue("    Final data rows: {nrow(filtered_data)}"))
        return(filtered_data)
      }) |>
        purrr::compact() |>
        purrr::list_rbind()
      
      return(qtr_data)
    }) |>
      purrr::compact() |>
      purrr::list_rbind()
    
    return(year_data)
  }) |>
    purrr::compact() |>
    purrr::list_rbind()
  
  # Check if we got any data
  if (is.null(all_data) || nrow(all_data) == 0) {
    message("No data retrieved for the specified parameters")
    return(dplyr::tibble())
  }
  
  # Join with lookup data if requested
  if (!is.null(join_lookup) && !is.null(local_lookup)) {
    if (!all(join_lookup %in% names(local_lookup))) {
      missing_fields <- setdiff(join_lookup, names(local_lookup))
      message(glue::glue("Warning: Missing lookup fields: {paste(missing_fields, collapse = ', ')}"))
      join_lookup <- intersect(join_lookup, names(local_lookup))
    }
    
    # Make sure local_lookup has a fips column
    if (!"fips" %in% names(local_lookup)) {
      if ("fips_full" %in% names(local_lookup)) {
        local_lookup <- local_lookup |> dplyr::mutate(fips = fips_full)
      } else {
        stop("local_lookup must have a 'fips' or 'fips_full' column")
      }
    }
    
    # Prepare lookup data for joining
    lookup_to_join <- local_lookup |> 
      dplyr::select(fips, dplyr::all_of(join_lookup)) |>
      dplyr::distinct()
    
    # Join with lookup data
    all_data <- all_data |>
      dplyr::rename(fips = area_fips) |>
      dplyr::left_join(lookup_to_join, by = "fips")
    
    message(glue::glue("Joined with {length(join_lookup)} fields from lookup table"))
  }
  
  message(glue::glue("Final dataset: {nrow(all_data)} rows, {ncol(all_data)} columns"))
  return(all_data)
}

## Test example -----------------------

test <- get_qcew_data(
  years = c(2022:2023),                       # 2022 and 2023
  qtr = "a",                                  # Annual data
  fips = c(51041, 51760),                     # Chesterfield and Richmond
  agglvl_code = c(70, 71, 72),                # Total, Total by ownership, Total by domain
  join_lookup = c("name_long", "cbsa_title"), # Add name_long and cbsa_title fields
  local_lookup = local_lookup                 # Define table to join
)

va_fips <- unique(local_lookup$fips)

qcew_pull <- get_qcew_data(
  years = c(2015:2024),                       # 2022 and 2023
  qtr = "a",                                  # Annual data
  fips = va_fips,                     # Chesterfield and Richmond
  agglvl_code = c(70, 71, 72),                # Total, Total by ownership, Total by domain
  join_lookup = c("name_long", "cbsa_title"), # Add name_long and cbsa_title fields
  local_lookup = local_lookup                 # Define table to join
)

# Upload to S3 bucket
s3 <- paws::s3()

temp_file <- tempfile(fileext = ".rds")
write_rds(qcew_pull, temp_file)
s3$put_object(
  Bucket = "hda-data-hub",
  Key = "bls/va_qcew.rds",
  Body = temp_file
)
file.remove(temp_file)

# OEWS DATA --------------------------------------------------------------------

# OEWS Download with rvest and Session Management
# This approach uses persistent sessions to mimic browser behavior
# AMENDED VERSION with proper MSA_M{year}_dl.xlsx processing

# Load required libraries
library(rvest)       # For web scraping with sessions
library(httr)        # For HTTP requests
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(purrr)       # For functional programming
library(here)        # For file paths

# Install packages if not available
required_packages <- c("rvest", "httr", "readxl", "dplyr", "purrr", "here")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
  
  # Load them
  for (pkg in missing_packages) {
    library(pkg, character.only = TRUE)
  }
}

# Create directories
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data/oews_rvest")) dir.create("data/oews_rvest")
if (!dir.exists("data/oews_processed")) dir.create("data/oews_processed")

cat("=== OEWS Download with rvest Session Management ===\n")
cat("This approach uses persistent HTTP sessions to download files\n\n")

# Years to download
years <- 2016:2024
base_url <- "https://www.bls.gov/oes/special-requests/oesm{year}ma.zip"

# Create a persistent session
cat("Creating persistent HTTP session...\n")
session <- session("https://www.bls.gov")

# Function to download with session
download_with_session <- function(year, session) {
  year_short <- sprintf("%02d", year %% 100)
  url <- glue::glue(base_url, year = year_short)
  filename <- paste0("oesm", year_short, "ma.zip")
  filepath <- here("data", "oews_rvest", filename)
  
  cat("Downloading", year, "from:", url, "\n")
  
  # Add random delay to appear more human-like
  delay <- runif(1, 2, 5)  # Random delay between 2-5 seconds
  cat("Waiting", round(delay, 1), "seconds...\n")
  Sys.sleep(delay)
  
  tryCatch({
    # Try to download using the session
    response <- session %>%
      session_jump_to(url) %>%
      session_response()
    
    # Check if we got the file
    if (status_code(response) == 200) {
      # Check content type
      content_type <- headers(response)$`content-type`
      
      if (is.null(content_type) || grepl("zip|octet-stream", content_type, ignore.case = TRUE)) {
        # Looks like a zip file, save it
        writeBin(content(response, "raw"), filepath)
        
        # Verify it's actually a zip file
        if (file.exists(filepath) && file.size(filepath) > 1000) {
          cat("Successfully downloaded:", filename, "(", round(file.size(filepath)/1024/1024, 2), "MB )\n")
          return(TRUE)
        } else {
          cat("Downloaded file seems too small or corrupted for", year, "\n")
          if (file.exists(filepath)) file.remove(filepath)
          return(FALSE)
        }
      } else {
        cat("Unexpected content type for", year, ":", content_type, "\n")
        return(FALSE)
      }
    } else {
      cat("HTTP status", status_code(response), "for year", year, "\n")
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("Error downloading", year, ":", e$message, "\n")
    return(FALSE)
  })
}

# Alternative function using httr directly (fallback)
download_with_httr <- function(year) {
  year_short <- sprintf("%02d", year %% 100)
  url <- glue::glue(base_url, year = year_short)
  filename <- paste0("oesm", year_short, "ma.zip")
  filepath <- here("data", "oews_rvest", filename)
  
  cat("Trying direct download for", year, "...\n")
  
  tryCatch({
    response <- GET(
      url,
      user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"),
      add_headers(
        "Accept" = "application/zip, application/octet-stream, */*",
        "Accept-Language" = "en-US,en;q=0.9",
        "Accept-Encoding" = "gzip, deflate, br",
        "Connection" = "keep-alive",
        "Upgrade-Insecure-Requests" = "1"
      ),
      timeout(60)
    )
    
    if (status_code(response) == 200) {
      writeBin(content(response, "raw"), filepath)
      
      if (file.exists(filepath) && file.size(filepath) > 1000) {
        cat("Direct download successful for", year, "\n")
        return(TRUE)
      }
    }
    
    cat("Direct download failed for", year, "- status:", status_code(response), "\n")
    return(FALSE)
    
  }, error = function(e) {
    cat("Direct download error for", year, ":", e$message, "\n")
    return(FALSE)
  })
}

# Try downloading files --------------------------------------------------------
cat("\n=== DOWNLOADING FILES ===\n")
download_results <- vector("logical", length(years))
names(download_results) <- as.character(years)

for (i in seq_along(years)) {
  year <- years[i]
  cat(paste0("\n[", i, "/", length(years), "] "))
  
  # First try with session
  success <- download_with_session(year, session)
  
  # If session method fails, try direct httr
  if (!success) {
    cat("Session method failed, trying direct approach...\n")
    success <- download_with_httr(year)
  }
  
  download_results[as.character(year)] <- success
}

# Check what we got
successful_years <- years[download_results]
failed_years <- years[!download_results]

cat("\n=== DOWNLOAD SUMMARY ===\n")
cat("Successful downloads:", paste(successful_years, collapse = ", "), "\n")
if (length(failed_years) > 0) {
  cat("Failed downloads:", paste(failed_years, collapse = ", "), "\n")
}

# List downloaded files
downloaded_files <- list.files(here("data", "oews_rvest"), pattern = "\\.zip$", full.names = TRUE)
cat("\nDownloaded files:\n")
for (file in downloaded_files) {
  file_size <- round(file.size(file) / 1024 / 1024, 2)
  cat("  ", basename(file), " (", file_size, " MB)\n")
}

cat("\n=== EXTRACTING ZIP FILES ===\n")

# Function to extract zip files
extract_zip_file <- function(zip_path) {
  filename <- basename(zip_path)
  year_match <- regmatches(filename, regexpr("\\d{2}", filename))
  
  if (length(year_match) > 0) {
    year_short <- year_match[1]
    year_full <- as.numeric(year_short) + ifelse(as.numeric(year_short) > 50, 1900, 2000)
    
    # Create extraction directory structure
    extract_dir <- here("data", "oews_processed", paste0("may_", year_full))
    
    if (!dir.exists(extract_dir)) {
      dir.create(extract_dir, recursive = TRUE)
    }
    
    cat("Extracting", filename, "for year", year_full, "...\n")
    
    tryCatch({
      # Extract the zip file
      unzip(zip_path, exdir = extract_dir, overwrite = TRUE)
      
      # List what was extracted
      extracted_files <- list.files(extract_dir, recursive = TRUE)
      cat("  Extracted", length(extracted_files), "files\n")
      
      # Look for the main folder and Excel files
      folders <- list.dirs(extract_dir, full.names = FALSE, recursive = FALSE)
      excel_files <- list.files(extract_dir, pattern = "\\.xlsx?$", recursive = TRUE, ignore.case = TRUE)
      
      if (length(excel_files) > 0) {
        cat("  Excel files found:", paste(basename(excel_files), collapse = ", "), "\n")
      }
      
      if (length(folders) > 0) {
        cat("  Folders created:", paste(folders, collapse = ", "), "\n")
      }
      
      return(TRUE)
      
    }, error = function(e) {
      cat("  Error extracting", filename, ":", e$message, "\n")
      return(FALSE)
    })
  } else {
    cat("Could not determine year from filename:", filename, "\n")
    return(FALSE)
  }
}

# Extract all downloaded zip files
zip_files <- list.files(here("data", "oews_rvest"), pattern = "\\.zip$", full.names = TRUE)

if (length(zip_files) == 0) {
  cat("No zip files found to extract!\n")
} else {
  cat("Found", length(zip_files), "zip files to extract\n")
  
  extraction_results <- map_lgl(zip_files, extract_zip_file)
  
  successful_extractions <- sum(extraction_results)
  cat("\nExtraction summary:\n")
  cat("Successfully extracted:", successful_extractions, "out of", length(zip_files), "files\n")
  
  if (successful_extractions > 0) {
    cat("\nExtracted file structure:\n")
    processed_dirs <- list.dirs(here("data", "oews_processed"), full.names = FALSE, recursive = FALSE)
    for (dir in processed_dirs) {
      if (dir != "") {  # Skip the root directory entry
        cat("  ", dir, "/\n")
        subdir_path <- here("data", "oews_processed", dir)
        subdirs <- list.dirs(subdir_path, full.names = FALSE, recursive = FALSE)
        for (subdir in subdirs) {
          if (subdir != "") {
            cat("    ", subdir, "/\n")
            # Show Excel files in this subdirectory
            excel_files <- list.files(file.path(subdir_path, subdir), pattern = "\\.xlsx?$", ignore.case = TRUE)
            if (length(excel_files) > 0) {
              cat("      Excel files:", paste(excel_files, collapse = ", "), "\n")
            }
          }
        }
      }
    }
  }
}
# Fixed OEWS Processing Script - Handles the correct file structure
# Files are located at: data/oews_processed/may_YEAR/oesmaYYma/MSA_MYEAR_dl.xlsx

library(readxl)
library(dplyr)
library(purrr)
library(here)

cat("=== FIXED OEWS PROCESSING SCRIPT ===\n")
cat("Looking for files in structure: data/oews_processed/may_YEAR/oesmaYYma/MSA_MYEAR_dl.xlsx\n\n")

# Define years
years <- 2016:2024

# Function to process a single year with correct file path
process_year_correct_path <- function(year) {
  year_short <- sprintf("%02d", year %% 100)
  
  # Correct file path structure
  folder_name <- paste0("may_", year)
  zip_folder_name <- paste0("oesm", year_short, "ma")
  target_file <- paste0("MSA_M", year, "_dl.xlsx")
  
  # Full path to the Excel file
  excel_file <- here("data", "oews_processed", folder_name, zip_folder_name, target_file)
  
  cat("Processing year", year, "...\n")
  cat("Looking for:", excel_file, "\n")
  
  # Check if the file exists
  if (!file.exists(excel_file)) {
    cat("Target file not found. Let me check what's actually there...\n")
    
    # Check the intermediate directories
    base_folder <- here("data", "oews_processed", folder_name)
    if (dir.exists(base_folder)) {
      cat("Base folder exists:", base_folder, "\n")
      
      zip_folder <- file.path(base_folder, zip_folder_name)
      if (dir.exists(zip_folder)) {
        cat("Zip folder exists:", zip_folder, "\n")
        
        # List all files in the zip folder
        all_files <- list.files(zip_folder, full.names = FALSE)
        cat("Files in zip folder:", paste(all_files, collapse = ", "), "\n")
        
        # Look for Excel files
        excel_files <- all_files[grepl("\\.(xlsx|xls)$", all_files, ignore.case = TRUE)]
        if (length(excel_files) > 0) {
          cat("Excel files found:", paste(excel_files, collapse = ", "), "\n")
          
          # Try to find the closest match
          msa_files <- excel_files[grepl("MSA", excel_files, ignore.case = TRUE)]
          if (length(msa_files) > 0) {
            excel_file <- file.path(zip_folder, msa_files[1])
            cat("Using alternative file:", msa_files[1], "\n")
          } else {
            cat("No MSA files found for", year, "\n")
            return(NULL)
          }
        } else {
          cat("No Excel files found for", year, "\n")
          return(NULL)
        }
      } else {
        cat("Zip folder not found:", zip_folder, "\n")
        cat("Available folders in base:", paste(list.dirs(base_folder, full.names = FALSE, recursive = FALSE), collapse = ", "), "\n")
        return(NULL)
      }
    } else {
      cat("Base folder not found:", base_folder, "\n")
      return(NULL)
    }
  } else {
    cat("Found target file:", excel_file, "\n")
  }
  
  # Try to read the Excel file
  tryCatch({
    cat("Reading Excel file for", year, "...\n")
    
    # Check available sheets
    sheets <- excel_sheets(excel_file)
    cat("Available sheets:", paste(sheets, collapse = ", "), "\n")
    
    # Read the first sheet
    df <- read_excel(excel_file, sheet = 1, .name_repair = "minimal")
    cat("Initial read - dimensions:", nrow(df), "rows x", ncol(df), "columns\n")
    
    # Check first few rows to identify header row
    # OEWS files often have title rows at the top
    header_row <- 1
    for (i in 1:min(10, nrow(df))) {
      row_text <- paste(as.character(df[i, ]), collapse = " ")
      if (grepl("AREA|MSA|OCCUPATION|OCC_CODE|OCC_TITLE|EMPLOYMENT|WAGE", row_text, ignore.case = TRUE)) {
        header_row <- i
        cat("Found header row at line:", i, "\n")
        break
      }
    }
    
    # Re-read with correct header if needed
    if (header_row > 1) {
      cat("Re-reading with header starting at row", header_row, "\n")
      df <- read_excel(excel_file, sheet = 1, skip = header_row - 1)
    }
    
    # Add year column
    df$survey_year <- year
    
    # Clean column names
    names(df) <- tolower(gsub("[^A-Za-z0-9]", "_", names(df)))
    names(df) <- gsub("_+", "_", names(df))
    names(df) <- gsub("^_|_$", "", names(df))
    
    # Harmonize column names that changed over the years
    # This maps old/alternative names to standard names
    column_mapping <- list(
      "area_title" = "area_name",
      "o_group" = "occ_group",
      "occ_group" = "occ_group",  # Keep as standard
      "area_name" = "area_name",  # Keep as standard
      "msa_title" = "area_name",
      "msa_name" = "area_name",
      "occupation_title" = "occ_title",
      "occupation_code" = "occ_code",
      "job_title" = "occ_title",
      "employment_1000" = "employment",
      "emp_1000" = "employment",
      "hourly_mean" = "h_mean",
      "annual_mean" = "a_mean",
      "mean_hourly" = "h_mean",
      "mean_annual" = "a_mean"
    )
    
    # Apply column name harmonization
    for (old_name in names(column_mapping)) {
      if (old_name %in% names(df)) {
        standard_name <- column_mapping[[old_name]]
        if (old_name != standard_name) {
          # Rename the column
          names(df)[names(df) == old_name] <- standard_name
          cat("  Renamed '", old_name, "' to '", standard_name, "'\n")
        }
      }
    }
    
    cat("Successfully processed", year, ":", nrow(df), "rows x", ncol(df), "columns\n")
    
    # Show key columns for verification
    key_cols <- names(df)[grepl("area|occ|employment|wage|state", names(df))]
    if (length(key_cols) > 0) {
      cat("Key columns:", paste(head(key_cols, 8), collapse = ", "), "\n")
    }
    
    # Check for state information
    if ("prim_state" %in% names(df)) {
      state_counts <- table(df$prim_state)
      cat("State distribution:", paste(names(state_counts), "=", state_counts, collapse = ", "), "\n")
    }
    
    cat("Year", year, "processing complete!\n\n")
    return(df)
    
  }, error = function(e) {
    cat("Error reading", year, ":", e$message, "\n\n")
    return(NULL)
  })
}

# Process all years
cat("=== PROCESSING ALL YEARS ===\n")
all_data_list <- map(years, process_year_correct_path)

# Remove NULL entries
successful_data <- compact(all_data_list)
successful_years <- map_int(successful_data, ~unique(.x$survey_year))

cat("=== PROCESSING SUMMARY ===\n")
cat("Successfully processed years:", paste(successful_years, collapse = ", "), "\n")
cat("Number of datasets:", length(successful_data), "\n")

if (length(successful_data) == 0) {
  cat("ERROR: No data files were successfully processed!\n")
  cat("Check the diagnostic output above to see what files exist.\n")
  stop("No data to process")
}

# Show sample from first dataset
if (length(successful_data) > 0) {
  sample_data <- successful_data[[1]]
  cat("\nSample from first dataset:\n")
  cat("Columns:", paste(names(sample_data)[1:min(10, ncol(sample_data))], collapse = ", "), "\n")
  
  # Check for area information
  area_cols <- names(sample_data)[grepl("area", names(sample_data), ignore.case = TRUE)]
  if (length(area_cols) > 0) {
    cat("Area columns found:", paste(area_cols, collapse = ", "), "\n")
    sample_areas <- unique(sample_data[[area_cols[1]]])[1:5]
    cat("Sample area codes:", paste(sample_areas, collapse = ", "), "\n")
  }
}

# Combine all datasets
cat("\n=== COMBINING ALL DATASETS ===\n")

# Get all unique column names
all_columns <- unique(unlist(map(successful_data, names)))
cat("Total unique columns after harmonization:", length(all_columns), "\n")

# Show some examples of harmonized columns
harmonized_examples <- intersect(all_columns, c("area_name", "occ_group", "occ_title", "occ_code", "h_mean", "a_mean"))
if (length(harmonized_examples) > 0) {
  cat("Examples of harmonized columns present:", paste(harmonized_examples, collapse = ", "), "\n")
}

# Standardize columns and fix data type conflicts
standardize_columns <- function(df, target_columns) {
  # Add missing columns with NA
  missing_cols <- setdiff(target_columns, names(df))
  df[missing_cols] <- NA
  
  # Convert problematic columns to character to avoid type conflicts
  # Common OEWS columns that have type conflicts across years
  char_columns <- c("hourly", "annual", "employment", "wage", "mean", "median", 
                    "pct10", "pct25", "pct75", "pct90", "h_pct10", "h_pct25", 
                    "h_median", "h_pct75", "h_pct90", "a_pct10", "a_pct25", 
                    "a_median", "a_pct75", "a_pct90")
  
  for (col in char_columns) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  
  # Also convert any column that contains wage/employment/hour data to character
  wage_cols <- names(df)[grepl("wage|employment|hour|annual|pct|mean|median", names(df), ignore.case = TRUE)]
  for (col in wage_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  
  # Reorder to match target columns
  df[target_columns]
}

cat("Standardizing column structure and fixing data types...\n")

# Before standardizing, let's check for type conflicts
cat("Checking for potential data type conflicts...\n")
for (i in 1:length(successful_data)) {
  year <- successful_data[[i]]$survey_year[1]
  cols_with_types <- sapply(successful_data[[i]], class)
  problematic_cols <- cols_with_types[names(cols_with_types) %in% c("hourly", "annual", "employment")]
  if (length(problematic_cols) > 0) {
    cat("Year", year, "- problematic column types:", paste(names(problematic_cols), ":", problematic_cols, collapse = ", "), "\n")
  }
}

standardized_data <- map(successful_data, ~standardize_columns(.x, all_columns))

# Combine all years
combined_data <- bind_rows(standardized_data)
cat("Combined dataset before filtering:", nrow(combined_data), "rows x", ncol(combined_data), "columns\n")

# Filter for Virginia data only (prim_state = "VA")
cat("\n=== FILTERING FOR VIRGINIA DATA (prim_state = 'VA') ===\n")

# Check if prim_state column exists
prim_state_cols <- names(combined_data)[grepl("prim_state|state", names(combined_data), ignore.case = TRUE)]
cat("State-related columns found:", paste(prim_state_cols, collapse = ", "), "\n")

if ("prim_state" %in% names(combined_data)) {
  # Show sample values in prim_state column
  unique_states <- unique(combined_data$prim_state)
  cat("Unique states in data:", paste(head(unique_states, 10), collapse = ", "), "\n")
  
  # Filter for Virginia only
  va_data <- combined_data[combined_data$prim_state == "VA" & !is.na(combined_data$prim_state), ]
  cat("Virginia data after filtering:", nrow(va_data), "rows x", ncol(va_data), "columns\n")
  
  if (nrow(va_data) == 0) {
    cat("WARNING: No Virginia data found! Check if prim_state values include 'VA'\n")
    cat("All prim_state values:", paste(unique(combined_data$prim_state), collapse = ", "), "\n")
    # Keep all data for now
    combined_data <- combined_data
  } else {
    # Use Virginia data only
    combined_data <- va_data
    cat("Successfully filtered to Virginia data only\n")
  }
} else {
  cat("WARNING: prim_state column not found!\n")
  cat("Available columns containing 'state':", paste(prim_state_cols, collapse = ", "), "\n")
  
  # Try alternative state column names
  if (length(prim_state_cols) > 0) {
    state_col <- prim_state_cols[1]
    cat("Trying to use column:", state_col, "\n")
    unique_states <- unique(combined_data[[state_col]])
    cat("Unique values in", state_col, ":", paste(head(unique_states, 10), collapse = ", "), "\n")
    
    # Filter for Virginia
    va_data <- combined_data[combined_data[[state_col]] == "VA" & !is.na(combined_data[[state_col]]), ]
    if (nrow(va_data) > 0) {
      combined_data <- va_data
      cat("Filtered using", state_col, "- Virginia data:", nrow(combined_data), "rows\n")
    }
  }
}

# Upload to S3 bucket
s3 <- paws::s3()

temp_file <- tempfile(fileext = ".rds")
write_rds(combined_data, temp_file)
s3$put_object(
  Bucket = "hda-data-hub",
  Key = "bls/va_oews.rds",
  Body = temp_file
)
file.remove(temp_file)

