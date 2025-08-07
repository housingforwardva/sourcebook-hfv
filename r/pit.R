# Install required packages
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("velofrog/readxlsb")

# Load necessary packages
library(readxlsb)
library(tidyverse)
library(janitor)

# URL for the .xlsb file
url <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2024-PIT-Counts-by-CoC.xlsb"

# Create a temporary file to download to
temp_file <- tempfile(fileext = ".xlsb")

# Download the file
download.file(url, temp_file, mode = "wb")

# Years to process
years <- as.character(2007:2024)

# Function to process each sheet
process_sheet <- function(yr, file_path) {
  tryCatch({
    # Read the sheet
    sheet_data <- read_xlsb(file_path, sheet = yr)
    
    # Clean column names
    sheet_data <- sheet_data %>% clean_names()
    
    # Find CoC number column
    coc_col <- grep("coc.*number|co_c.*number", colnames(sheet_data), ignore.case = TRUE, value = TRUE)
    
    if (length(coc_col) > 0) {
      
      data <- sheet_data 
      
      if (nrow(data) > 0) {
        # Add year
        data$year <- yr
        return(data)
      }
    }
    return(NULL)
  }, error = function(e) {
    message("Error processing sheet ", yr, ": ", e$message)
    return(NULL)
  })
}

# Process all sheets
all_data <- lapply(years, process_sheet, file_path = temp_file)
all_data <- all_data[!sapply(all_data, is.null)]

# If we have data, handle it properly
if (length(all_data) > 0) {
  # First, identify common columns across all data frames
  common_cols <- Reduce(intersect, lapply(all_data, colnames))
  
  # Function to standardize column types - convert all numeric-like columns to numeric
  standardize_types <- function(df, cols) {
    df_subset <- df[, cols, drop = FALSE]
    
    # For each column, try to convert to numeric if it makes sense
    for (col in colnames(df_subset)) {
      # Skip columns that are clearly not numeric
      if (!grepl("number|name|category|type|year", col, ignore.case = TRUE)) {
        # Try to convert to numeric, handling NAs appropriately
        df_subset[[col]] <- as.numeric(as.character(df_subset[[col]]))
      }
    }
    
    return(df_subset)
  }
  
  # Apply type standardization to all data frames
  standardized_data <- lapply(all_data, standardize_types, cols = common_cols)
  
  # Now bind rows should work
  combined_data <- bind_rows(standardized_data)
  
  # Find columns related to homeless counts for pivoting
  homeless_cols <- grep("homeless|chroni", colnames(combined_data), value = TRUE)
  
  if (length(homeless_cols) > 0) {
    # Create longer version with proper naming
    full_set_longer <- combined_data %>%
      pivot_longer(
        cols = all_of(homeless_cols),
        names_to = "category",
        values_to = "value"
      ) %>%
      mutate(category = case_when(
        grepl("overall_homeless$", category) ~ "Overall Homeless",
        grepl("sheltered_total_homeless$", category) ~ "Total Sheltered Homeless",
        grepl("unsheltered_homeless$", category) ~ "Total Unsheltered Homeless",
        grepl("overall_homeless_individuals$", category) ~ "Overall Homeless Individuals",
        grepl("overall_homeless_family_households$", category) ~ "Overall Homeless Family Households",
        grepl("sheltered_total_homeless_family_households$", category) ~ "Overall Sheltered Homeless Family Households",
        grepl("unsheltered_homeless_family_households$", category) ~ "Overall Unsheltered Homeless Family Households",
        grepl("overall_chronically_homeless_individuals$", category) ~ "Overall Chronically Homeless Individuals", 
        grepl("sheltered_total_chronically_homeless_individuals$", category) ~ "Total Sheltered Chronically Homeless Individuals",
        TRUE ~ category
      ))
    
    # View summary of the data
    print(dim(full_set_longer))
    print(head(full_set_longer))
    
    # Save the result
    # write_csv(full_set_longer, "data/pit_data_longer.csv")
    # write_csv(combined_data, "data/pit_data.csv")
    
    full_set_longer
    
    combined_data
    
    print("Data successfully processed and saved!")
  } else {
    print("No homeless-related columns found for pivoting")
  }
  
  # Upload to S3 bucket
  s3 <- paws::s3()
  
  # Upload full_set_longer
  temp_file_longer <- tempfile(fileext = ".rds")
  write_rds(full_set_longer, temp_file_longer)
  
  s3$put_object(
    Bucket = "hda-data-hub",
    Key = "hud/pit_data_longer.rds",
    Body = temp_file_longer
  )
  
  # Upload combined_data
  temp_file_combined <- tempfile(fileext = ".rds")
  write_rds(combined_data, temp_file_combined)
  
  s3$put_object(
    Bucket = "hda-data-hub",
    Key = "hud/pit_data_combined.rds",
    Body = temp_file_combined
  )
  
  # Clean up temporary files
  file.remove(temp_file_longer)
  file.remove(temp_file_combined)
  
  print("Both datasets successfully uploaded to S3!")