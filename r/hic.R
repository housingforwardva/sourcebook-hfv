# Load necessary packages 
library(readxl) 
library(janitor) 
library(tidyverse)

# URL for the .xlsx file
url <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2024-HIC-Counts-by-CoC.xlsx"

# Create a file path to download to
download_path <- "data/HIC-Counts-by-CoC.xlsx"
dir.create("data", showWarnings = FALSE)
download.file(url, download_path, mode = "wb")

# Function to read a sheet and add year column
read_hic_sheet <- function(sheet_name, file_path) {
  # Skip first row which contains merged cells with section headers
  sheet_data <- read_excel(file_path, sheet = sheet_name, skip = 1) %>%
    # Add a year column based on sheet name
    mutate(Year = as.numeric(sheet_name)) %>%
    # Convert all columns to numeric except for the 1st column and Year
    mutate(across(-c(1, Year), as.numeric))
  
  return(sheet_data)
}

# Get all sheet names (excluding "Revisions" sheet which has different format)
sheet_names <- excel_sheets(download_path)
sheet_names <- sheet_names[sheet_names != "Revisions"]

# Read all sheets and combine
all_hic_data <- map_df(sheet_names, ~read_hic_sheet(.x, download_path))

# Combine duplicate columns using coalesce
duplicate_names <- names(all_hic_data)[duplicated(names(all_hic_data))]

for(dup_name in unique(duplicate_names)) {
  # Find all instances of this column name
  dup_indices <- which(names(all_hic_data) == dup_name)
  
  # Combine using coalesce (takes first non-NA value)
  all_hic_data[[dup_name]] <- do.call(coalesce, all_hic_data[dup_indices])
  
  # Remove the duplicate columns (keep only the first)
  all_hic_data <- all_hic_data[, -dup_indices[-1]]
}

# Rearrange columns to put Year first
all_hic_data <- all_hic_data %>%
  select(Year, everything())

# Before cleaning, get the original name of the second column (should be "CoC Number")
coc_column_original <- names(all_hic_data)[2]  # CoC is now second column
cat("Original CoC column name:", coc_column_original, "\n")

# Clean column names
all_hic_data <- all_hic_data %>% 
  clean_names() %>%
  # Standardize missing values (may be represented as "." in some sheets)
  mutate(across(where(is.character), ~na_if(.x, ".")))

# After cleaning, get the new name of the second column (CoC) and year column
coc_column_cleaned <- names(all_hic_data)[2]  # CoC is now second column
year_column <- "year"  # This should be the cleaned name after janitor::clean_names()
cat("Cleaned CoC column name:", coc_column_cleaned, "\n")
cat("Year column name:", year_column, "\n")

# Verify the year column exists
if(!year_column %in% names(all_hic_data)) {
  stop("Year column not found! Check column names: ", paste(names(all_hic_data)[1:10], collapse=", "))
}

# Convert numeric columns back to numeric type
# These are typically columns with rates, counts, or percentages
all_hic_data <- all_hic_data %>%
  mutate(
    # Make sure year is numeric
    year = as.numeric(year),
    # Convert columns that should be numeric back to numeric
    across(
      # Select columns that contain certain patterns in their names
      contains(c("beds", "units", "rate", "participation", "percentage")), 
      ~as.numeric(.x)
    )
  )

# Filter to keep only Virginia CoCs (those starting with "VA-")
virginia_hic_data <- all_hic_data %>%
  filter(str_detect(!!sym(coc_column_cleaned), "^VA-"))

# Preview the Virginia data - showing CoC, Year and a sample metric
sample_beds_column <- names(virginia_hic_data)[grep("total_year_round_beds_es_th_sh", names(virginia_hic_data))[1]]
cat("First few rows of Virginia data with Year column:\n")
virginia_preview <- virginia_hic_data %>% 
  select(year, !!sym(coc_column_cleaned), !!sym(sample_beds_column)) %>%
  arrange(year, !!sym(coc_column_cleaned))
print(head(virginia_preview, 10))

# Check the range of years in the data
year_range <- range(virginia_hic_data$year, na.rm = TRUE)
cat("Year range in the data:", year_range[1], "to", year_range[2], "\n")

# Count records by year
cat("Number of Virginia records by year:\n")
virginia_hic_data %>%
  count(year) %>%
  arrange(year) %>%
  print(n = nrow(.))

# Save the Virginia-only data
write_csv(virginia_hic_data, "data/virginia_hic_data.csv")

# Show dimensions of the Virginia dataset
cat("Virginia data dimensions: ", dim(virginia_hic_data)[1], "rows x", 
    dim(virginia_hic_data)[2], "columns\n")

# Summary of Virginia CoCs in the dataset
cat("Virginia CoCs in the dataset:\n")
virginia_hic_data %>%
  count(!!sym(coc_column_cleaned)) %>%
  arrange(!!sym(coc_column_cleaned)) %>%
  print(n = nrow(.))

write_rds(virginia_hic_data, "data/rds/va_hic_data.rds")


library(tidyverse)
library(readxl)

# Read the Excel file and get all sheet names
file_path <- "data/2007-2024-HIC-Counts-by-CoC.xlsx"
sheet_names <- excel_sheets(file_path)

# Remove the "Revisions" sheet if it exists, keep only year sheets
year_sheets <- sheet_names[!sheet_names %in% c("Revisions")]

# Function to standardize column names across years
standardize_column_names <- function(data, year) {
  # First, standardize the CoC column name
  col_names <- names(data)
  if("CoC" %in% col_names) {
    names(data)[col_names == "CoC"] <- "CoC Number"
    cat(sprintf("Year %d: Renamed 'CoC' to 'CoC Number'\n", year))
  }
  
  # Create mapping from old names to new names
  # Based on the pattern: pre-2014 vs 2014+
  column_mapping <- if(year < 2014) {
    # Pre-2014 naming pattern to post-2013 naming pattern
    c(
      "Total Year-Round ES Beds" = "Total Year-Round Beds (ES)",
      "Total Year-Round TH Beds" = "Total Year-Round Beds (TH)", 
      "Total Year-Round SH Beds" = "Total Year-Round Beds (SH)",
      "Total Year-Round PSH Beds" = "Total Year-Round Beds (PSH)",
      "Total Year-Round RRH Beds" = "Total Year-Round Beds (RRH)",
      "Total Year-Round OPH Beds" = "Total Year-Round Beds (OPH)",
      
      # Non-DV beds
      "Total Non-DV Year-Round ES Beds" = "Total Non-DV Year-Round Beds (ES)",
      "Total Non-DV Year-Round TH Beds" = "Total Non-DV Year-Round Beds (TH)",
      "Total Non-DV Year-Round SH Beds" = "Total Non-DV Year-Round Beds (SH)",
      "Total Non-DV Year-Round PSH Beds" = "Total Non-DV Year-Round Beds (PSH)",
      "Total Non-DV Year-Round RRH Beds" = "Total Non-DV Year-Round Beds (RRH)",
      "Total Non-DV Year-Round OPH Beds" = "Total Non-DV Year-Round Beds (OPH)",
      
      # HMIS beds
      "Total HMIS Year-Round ES Beds" = "Total HMIS Year-Round Beds (ES)",
      "Total HMIS Year-Round TH Beds" = "Total HMIS Year-Round Beds (TH)",
      "Total HMIS Year-Round SH Beds" = "Total HMIS Year-Round Beds (SH)",
      "Total HMIS Year-Round PSH Beds" = "Total HMIS Year-Round Beds (PSH)",
      "Total HMIS Year-Round RRH Beds" = "Total HMIS Year-Round Beds (RRH)",
      "Total HMIS Year-Round OPH Beds" = "Total HMIS Year-Round Beds (OPH)",
      
      # Participation rates
      "HMIS Participation Rate for Year-Round ES Beds" = "HMIS Participation Rate for Year-Round Beds (ES)",
      "HMIS Participation Rate for Year-Round TH Beds" = "HMIS Participation Rate for Year-Round Beds (TH)",
      "HMIS Participation Rate for Year-Round SH Beds" = "HMIS Participation Rate for Year-Round Beds (SH)",
      "HMIS Participation Rate for Year-Round PSH Beds" = "HMIS Participation Rate for Year-Round Beds (PSH)",
      "HMIS Participation Rate for Year-Round RRH Beds" = "HMIS Participation Rate for Year-Round Beds (RRH)",
      "HMIS Participation Rate for Year-Round OPH Beds" = "HMIS Participation Rate for Year-Round Beds (OPH)"
    )
  } else {
    # Post-2013: no mapping needed, names are already standardized
    character(0)  # Empty mapping
  }
  
  # Apply the mapping
  current_names <- names(data)
  new_names <- current_names
  
  for(old_name in names(column_mapping)) {
    if(old_name %in% current_names) {
      new_names[current_names == old_name] <- column_mapping[old_name]
      cat(sprintf("Year %d: Renamed '%s' to '%s'\n", year, old_name, column_mapping[old_name]))
    }
  }
  
  names(data) <- new_names
  return(data)
}

# Function to remove duplicate columns (keep first instance only)
remove_duplicate_columns <- function(data) {
  col_names <- names(data)
  
  # Clean column names by removing Excel's automatic duplicate suffixes (...13, ...15, etc.)
  clean_col_names <- str_replace(col_names, "\\.{3}\\d+$", "")
  
  # Find which columns are actually duplicates after cleaning
  duplicated_clean <- duplicated(clean_col_names)
  
  if(any(duplicated_clean)) {
    duplicate_names <- unique(clean_col_names[duplicated_clean])
    cat("Found duplicate columns after cleaning:", paste(duplicate_names, collapse = ", "), "\n")
    
    # Keep only the first instance of each column name
    data <- data[, !duplicated_clean]
    
    # Update the column names to the cleaned versions
    names(data) <- clean_col_names[!duplicated_clean]
    
    cat("Removed", sum(duplicated_clean), "duplicate columns\n")
  } else {
    # Even if no duplicates, still clean the names
    names(data) <- clean_col_names
  }
  
  return(data)
}

# Function to read a single sheet and add year column
read_sheet_with_year <- function(sheet_name, file_path) {
  # Read the sheet, skipping the first row (which contains category headers)
  # Read all columns as character first to avoid type conflicts
  data <- read_excel(file_path, sheet = sheet_name, skip = 1, col_types = "text")
  
  # Clean column names to handle potential variations
  names(data) <- str_trim(names(data))  # Remove leading/trailing whitespace
  names(data) <- str_replace_all(names(data), "\\s+", " ")  # Normalize multiple spaces
  
  # Add a year column
  year_num <- as.numeric(sheet_name)
  data$year <- year_num
  
  # Remove duplicate columns (keep first instance only)
  data <- remove_duplicate_columns(data)
  
  # Standardize column names based on year
  data <- standardize_column_names(data, year_num)
  
  return(data)
}

# Function to discover column name patterns (for analysis)
discover_column_patterns <- function() {
  # Read a sample of sheets to show column patterns
  sample_pre_2014 <- read_excel(file_path, sheet = "2010", skip = 1, col_types = "text")
  sample_post_2013 <- read_excel(file_path, sheet = "2020", skip = 1, col_types = "text")
  
  names(sample_pre_2014) <- str_trim(names(sample_pre_2014))
  names(sample_post_2013) <- str_trim(names(sample_post_2013))
  
  cat("COLUMN NAME PATTERN ANALYSIS\n")
  cat("============================\n\n")
  
  cat("Sample 2010 (pre-2014) bed columns:\n")
  pre_bed_cols <- names(sample_pre_2014)[str_detect(names(sample_pre_2014), "Year-Round.*Beds$")]
  cat(paste(head(pre_bed_cols, 10), collapse = "\n"), "\n\n")
  
  cat("Sample 2020 (post-2013) bed columns:\n")
  post_bed_cols <- names(sample_post_2013)[str_detect(names(sample_post_2013), "Year-Round Beds \\(")]
  cat(paste(head(post_bed_cols, 10), collapse = "\n"), "\n\n")
}

# Analyze column consistency across years
analyze_column_consistency <- function(sheet_list) {
  # Get column names for each sheet
  all_columns <- map(sheet_list, names)
  
  # Get all unique column names across all sheets
  all_unique_cols <- unique(unlist(all_columns))
  
  # For each column, count in how many sheets it appears and track which sheets
  col_analysis <- map_dfr(all_unique_cols, function(col_name) {
    sheets_with_col <- names(all_columns)[map_lgl(all_columns, function(sheet_cols) {
      col_name %in% sheet_cols
    })]
    
    tibble(
      column_name = col_name,
      appears_in_sheets = length(sheets_with_col),
      total_sheets = length(sheet_list),
      consistency_rate = length(sheets_with_col) / length(sheet_list),
      present_in_years = paste(sort(sheets_with_col), collapse = ", "),
      missing_from_years = paste(sort(setdiff(names(all_columns), sheets_with_col)), collapse = ", ")
    )
  }) %>%
    arrange(desc(appears_in_sheets), column_name)
  
  return(col_analysis)
}

# After analyzing consistency, create the final dataset with selected columns only
create_selected_dataset <- function(sheet_list) {
  # Select only the columns we want: CoC Number, year, and columns starting with "Total Year-Round Beds"
  selected_data <- map(sheet_list, function(sheet) {
    col_names <- names(sheet)
    
    # Find columns we want to keep
    keep_cols <- c(
      "CoC Number",  # Always keep CoC identifier
      "year",        # Always keep year
      col_names[str_starts(col_names, "Total Year-Round Beds")]  # Any column starting with this pattern
    )
    
    # Only keep columns that actually exist in this sheet
    existing_cols <- keep_cols[keep_cols %in% col_names]
    
    return(sheet %>% select(all_of(existing_cols)))
  })
  
  # Combine all sheets
  combined <- bind_rows(selected_data)
  
  return(combined)
}

# MAIN EXECUTION
# ==============

# First, discover column patterns
cat("Discovering column name patterns...\n\n")
discover_column_patterns()

# Read all sheets with standardized column names
cat("\nReading and standardizing all sheets...\n")
all_sheets_data <- map(year_sheets, ~read_sheet_with_year(.x, file_path))
names(all_sheets_data) <- year_sheets

# Analyze consistency after standardization
cat("\nAnalyzing column consistency after standardization...\n")
column_analysis <- analyze_column_consistency(all_sheets_data)

# Show the analysis
cat("\nColumn Consistency Analysis:\n")
cat("===========================\n\n")

# Columns present in ALL sheets
consistent_cols <- column_analysis %>% 
  filter(consistency_rate == 1) %>%
  pull(column_name)

cat("Columns present in ALL", length(year_sheets), "sheets (", length(consistent_cols), "columns):\n")
cat(paste(consistent_cols, collapse = "\n"), "\n\n")

# Columns missing from some sheets
inconsistent_cols <- column_analysis %>%
  filter(consistency_rate < 1) %>%
  arrange(desc(appears_in_sheets))

cat("Columns with inconsistent presence:\n")
print(inconsistent_cols %>% select(column_name, appears_in_sheets, total_sheets, missing_from_years))

# Show detailed breakdown for the most common inconsistent columns
cat("\nDetailed breakdown of top inconsistent columns:\n")
top_inconsistent <- inconsistent_cols %>% 
  filter(appears_in_sheets >= length(year_sheets) * 0.5) %>%  # Show columns present in at least 50% of sheets
  head(10)

if(nrow(top_inconsistent) > 0) {
  for(i in 1:nrow(top_inconsistent)) {
    col_info <- top_inconsistent[i, ]
    cat(sprintf("\n'%s':\n", col_info$column_name))
    cat(sprintf("  Present in %d/%d sheets: %s\n", 
                col_info$appears_in_sheets, 
                col_info$total_sheets,
                col_info$present_in_years))
    if(nzchar(col_info$missing_from_years)) {
      cat(sprintf("  Missing from: %s\n", col_info$missing_from_years))
    }
  }
}

# Create dataset with only selected columns
cat("\n", rep("=", 50), "\n")
cat("CREATING FINAL DATASET WITH SELECTED COLUMNS\n")
cat("(CoC Number, year, and columns starting with 'Total Year-Round Beds')\n")
cat(rep("=", 50), "\n\n")

combined_data <- create_selected_dataset(all_sheets_data)

# Show what columns we ended up with
cat("Selected columns in final dataset:\n")
cat(paste(names(combined_data), collapse = "\n"), "\n\n")

# Filter for Virginia entries (CoC Number starts with "VA")
virginia_data <- combined_data %>%
  filter(str_starts(`CoC Number`, "VA")) |> 
  select(-3, -c(10:14))

write_rds(virginia_data, "data/rds/hic_va_data.rds")

# Convert numeric columns back to numeric
# This handles the "." values and other non-numeric entries by converting them to NA
# Keep CoC Number and year as character/numeric respectively
numeric_columns <- virginia_data %>%
  select(-`CoC Number`, -year) %>%
  names()

virginia_data <- virginia_data %>%
  mutate(across(all_of(numeric_columns), ~as.numeric(as.character(.x))))

# View the results
cat("Dataset Summary:\n")
cat("================\n")
cat("Total rows in combined dataset:", nrow(combined_data), "\n")
cat("Virginia rows:", nrow(virginia_data), "\n")
cat("Years covered:", min(virginia_data$year), "to", max(virginia_data$year), "\n")
cat("Selected columns used:", ncol(combined_data), "\n\n")

# Show Virginia CoC numbers to verify
virginia_cocs <- virginia_data %>%
  distinct(`CoC Number`) %>%
  arrange(`CoC Number`)

cat("Virginia CoC Numbers found:\n")
print(virginia_cocs)

# Optional: Save the Virginia data to a CSV file
write_csv(virginia_data, "virginia_hic_data_selected_columns.csv")

# View first few rows of Virginia data
cat("\nFirst few rows of Virginia data:\n")
head(virginia_data)