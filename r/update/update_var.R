library(paws)
library(tidyverse)
library(readxl)

s3 <- s3()

# Get the S3 object
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "var/full_data.rds"
)


var_data <- readRDS(rawConnection(s3_response$Body))

update_var <- function(file_name, update_quarter) {
  
  update <- paste0("data/raw/", file_name, ".xlsx")

  locality <- read_excel(update,
                              sheet = "CityCounty SFCT",
                              skip = 4) %>% 
  mutate(quarter = update_quarter,
         .before = 1) %>% 
  mutate(geography = "Locality",
         .before = 1) %>% 
  rename(name = 'City County')
  
  msa <- read_excel(update,
                         sheet = "MSA SFCT",
                              skip = 4) %>% 
  mutate(quarter = update_quarter,
         .before = 1)  %>% 
  mutate(geography = "MSA",
         .before = 1) %>% 
  rename(name = 'MSA') %>% 
  filter(!name == "Grand Total")
  
  full_data <- bind_rows(locality, msa) %>% 
  select(geography,
         quarter,
         name,
         units = 'Units',
         med_price = 'Median Price',
         med_dom = 'Median DOM',
         med_asratio = 'Median A/S Ratio') %>% 
  mutate(name = str_replace_all(name, "Grand Total", "Virginia")) %>% 
  mutate(geography = 
           case_when(
             name == "Virginia" ~ "State",
             TRUE ~ geography
           ))
}

data_update <- update_var("var_2025-Q2", "2025 Q2")

updated_var <- bind_rows(var_data, data_update)

# Upload to S3 bucket
s3 <- paws::s3()

temp_file <- tempfile(fileext = ".rds")

write_rds(updated_var, temp_file)

s3$put_object(
  Bucket = "hda-data-hub",
  Key = "var/full_data.rds",
  Body = temp_file
)

file.remove(temp_file)
