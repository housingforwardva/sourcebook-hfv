# Census Data Processing Script ------------------------------------------------
# Author: Eric Mai
# Purpose: Process multiple ACS tables for Virginia housing analysis
# Date: 2025-08-06

# Load required packages
library(tidyverse)
library(tidycensus)
library(fredr)
library(lubridate)

source("r/helper_functions.R")

# CLEAN ACS VARIABLES ----------------------------------------------------------

## TABLE B11001 - Household Type (including Living Alone) ----------------------

b11001_vars <- load_table_vars("B11001", 2023) %>% 
  separate(label, into = c("est", "total", "family", "type", "sub"), sep = "!!") %>% 
  mutate(race = hh_race(concept)) %>% 
  mutate(race = case_when(
    race == "Including Living" ~ "All Households",
    TRUE ~ race
  )) %>% 
  select(variable = name, family, type, sub, race) %>% 
  mutate(sub = case_when(
    str_ends(variable, "004") ~ "All other family",
    str_ends(variable, "003") ~ "Married-couple family",
    str_ends(variable, "002") ~ "All family households",
    str_ends(variable, "001") ~ "All households",
    str_ends(variable, "007") ~ "All nonfamily households",
    str_ends(variable, "008") ~ "Nonfamily - living alone",
    str_ends(variable, "003") ~ "Nonfamily - not living alone",
    str_ends(variable, "005") ~ "Other family - single male",
    str_ends(variable, "006") ~ "Other family - single female",
    TRUE ~ sub
  )) %>% 
  mutate(type = case_when(
    sub == "All households" ~ "All households",
    sub == "All family households" ~ "All family households",
    sub == "All nonfamily households" ~ "All nonfamily households",
    TRUE ~ type
  )) %>% 
  mutate(family = case_when(
    is.na(family) ~ "All households",
    TRUE ~ family
  )) %>% 
  mutate(across(2:3, .fns = ~str_remove_all(.x, ":")))

## TABLE B25003 - Tenure of Occupied Housing Units -----------------------------

 b25003_vars <- load_table_vars("B25003", 2023) %>% 
  separate(label, into = c("est", "total", "tenure"), sep = "!!") %>% 
  mutate(race = tenure_race(concept)) %>% 
  mutate(race = case_when(
    race == "Tenure" ~ "All households",
    TRUE ~ race
  )) %>% 
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter",
    TRUE ~ "All households"
  )) %>% 
  select(variable = name, tenure, race)
 
 ## TABLE B25004 - Vacancy Status -----------------------------------
 
 b25004_vars <- load_table_vars("B25004", 2023) %>% 
   separate(label, into = c("est", "total", "vacancy"), sep = "!!") %>% 
   select(variable = name, vacancy) %>% 
   drop_na()
   

 ## TABLE B25007 - Tenure by Householder Age -----------------------------------

b25007_vars <- load_table_vars("B25007", 2023) %>% 
  separate(label, into = c("est", "total", "tenure", "age"), sep = "!!") %>% 
  mutate(age = str_remove_all(age, "Householder ")) %>% 
  mutate(age = case_when(
    is.na(age) ~ "All ages",
    TRUE ~ age
  )) %>% 
  mutate(tenure = case_when(
    tenure == "Owner occupied:" ~ "Homeowner",
    tenure == "Renter occupied:" ~ "Renter",
    TRUE ~ "All households"
  )) %>% 
  select(variable = name, tenure, age) 

## TABLE B25007 - Tenure by Household Size -------------------------------------

b25009_vars <- load_table_vars("B25009", 2023) %>% 
  separate(label, into = c("est", "total", "tenure", "hhsize"), sep = "!!") %>% 
  mutate(tenure = case_when(
    tenure == "Owner occupied:" ~ "Homeowner",
    tenure == "Renter occupied:" ~ "Renter",
    TRUE ~ "All households"
  )) %>% 
  mutate(hhsize = str_remove_all(hhsize, " household")) %>% 
  mutate(hhsize = case_when(
    is.na(hhsize) ~ "All sizes",
    TRUE ~ hhsize
  )) %>% 
  select(variable = name, tenure, hhsize)

## TABLE B25007 - Tenure by Average Household Size -----------------------------

b25010_vars <- load_table_vars("B25010", 2023) %>% 
  separate(label, into = c("est", "avg", "total", "tenure"), sep = "!!")%>% 
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter",
    TRUE ~ "All households"
  )) %>%  select(variable = name, tenure)


## TABLE B11012 - Households by Type -------------------------------------------

b11012_vars <- load_table_vars("B11012", 2023) %>% 
  separate(label, into = c("est", "total", "type", "sub"), sep = "!!") %>% 
  select(variable = name, type, sub) %>% 
  mutate(sub = case_when(
    str_ends(variable, "001") ~ "All households",
    str_ends(variable, "002") ~ "All married-couple households",
    str_ends(variable, "005") ~ "All cohabitating couple households",
    str_ends(variable, "008") ~ "All single-female households",
    str_ends(variable, "013") ~ "All single-male households",
    TRUE ~ sub
  )) %>% 
  mutate(type = str_remove_all(type, ":"),
         type = case_when(
           is.na(type) ~ "All households",
           TRUE ~ type
         ))

## TABLE B09021 - Living Arrangements of Adults --------------------------------

b09021_vars <- load_table_vars("B09021", 2023) %>% 
  separate(label, into = c("est", "tot", "age", "type"), sep = "!!") %>% 
  select(variable = name, age, type) %>% 
  mutate(
    across(.fns = ~str_remove_all(.x, ":")),
    age = str_remove_all(age, " years")
  ) %>%
  mutate(
    type = case_when(
      variable %in% paste0("B09021_00", 2:7) ~ age,
      TRUE ~ type
    ),
    age = case_when(
      variable %in% paste0("B09021_00", 2:7) ~ "All ages",
      TRUE ~ age
    )
  ) %>%
  filter(if_all(c(age, type), ~ !is.na(.x))) %>% 
  mutate(
    type = case_when(
      str_detect(type, "Householder living") ~ "Lives with married or unmarried partner",
      type == "Child of householder" ~ "Lives with parent(s)",
      type == "Other relatives" ~ "Lives with other relative(s)",
      type == "Other nonrelatives" ~ "Lives with other nonrelative(s)",
      TRUE ~ type
    )
  )

## TABLE B25042 - Tenure by Bedrooms -------------------------------------------

b25042_vars <- load_table_vars("B25042", 2023) %>% 
  separate(label, into = c("est", "total", "tenure", "br"), sep = "!!") %>% 
  select(variable = name, tenure, br) %>% 
  mutate(tenure = case_when(
    tenure == "Owner occupied:" ~ "Homeowner",
    tenure == "Renter occupied:" ~ "Renter",
    TRUE ~ "All households"
  )) %>% 
  mutate(br = case_when(
    is.na(br) ~ "All bedrooms",
    TRUE ~ br
  ))
  

## TABLE B25032 - Structure Type by Tenure -------------------------------------

b25032_vars <- load_table_vars("B25032_", 2023) %>% 
  separate(label, into = c("est", "total", "tenure", "structure"), sep = "!!") %>% 
  select(variable.= name, tenure, structure) %>% 
  mutate(structure = case_when(
    is.na(structure) ~ "All units",
    TRUE ~ structure
  )) %>% 
  mutate(tenure = case_when(
    str_detect(tenure, "Renter") ~ "Renter",
    str_detect(tenure, "Owner") ~ "Homeowner",
    TRUE ~ "All households"
  ))
  

## TABLE B25127 - Tenure by Year Structure Built By Units ----------------------

b25127_vars <- load_table_vars("B25127", 2023) %>% 
  separate(label, into = c("est", "total", "tenure", "yrbuilt", "structure"), sep = "!!") %>% 
  select(variable = name, tenure, yrbuilt, structure) %>% 
  mutate(across(2:3, .fns = ~str_remove_all(.x, ":"))) %>% 
  drop_na()
  

## TABLE B25063 - Gross Rent ---------------------------------------------------

b25063_vars <- load_table_vars("B25063", 2023) %>% 
  separate(label, into = c("est", "total", "cash", "rent"), sep = "!!") %>% 
  select(variable = name, cash, rent)%>% 
  mutate(rent = case_when(
    str_ends(variable, "027") ~ "No cash rent",
    TRUE ~ rent
  )) %>% 
  select(variable, rent) %>% 
  drop_na()%>% 
  mutate(rent_range = case_when(
    rent %in% c("Less than $100", "$100 to $149", "$150 to $199", "$200 to $249", 
                    "$250 to $299", "$300 to $349", "$350 to $399", "$400 to $449", 
                    "$450 to $499") ~ "Less than $500",
    rent %in% c("$500 to $549", "$550 to $599", "$600 to $649", 
                    "$650 to $699", "$700 to $749") ~ "$500 to $749",
    rent %in% c("$750 to $799", "$800 to $849", "$850 to $899", 
                    "$900 to $949", "$950 to $999") ~ "$750 to $999",
    rent == "$1,000 to $1,249" ~ "$1,000 to $1,249",
    rent == "$1,250 to $1,499" ~ "$1,250 to $1,499",
    rent == "$1,500 to $1,999" ~ "$1,500 to $1,999",
    rent == "No cash rent" ~ "No cash rent",
    TRUE ~ "$2,000 or more"
      ))
  

## TABLE B25118 - Tenure by Household Income  ----------------------------------

b25118_vars <- load_table_vars("B25118", 2010) %>% 
  separate(label, into = c("est", "total", "tenure", "income"), sep = "!!") %>% 
  drop_na() %>% 
  select(variable = name, tenure, income) %>% 
  mutate(tenure = case_when(
    str_detect(tenure, "Owner") ~ "Homeowner",
    TRUE ~ "Renter"
  )) %>% 
  mutate(income_range = case_when(
      income %in% c("Less than $5,000", "$5,000 to $9,999") ~ "Under $10,000",
      income %in% c("$10,000 to $14,999", "$15,000 to $19,999") ~ "$10,000 to $19,999",
      income %in% c("$20,000 to $24,999", "$25,000 to $34,999") ~ "$20,000 to $34,999",
      income == "$35,000 to $49,999" ~ "$35,000 to $49,999",
      income == "$50,000 to $74,999" ~ "$50,000 to $74,999",
      income %in% c("$75,000 to $99,999", "$100,000 to $149,999") ~ "$75,000 to $149,999",
      income == "$150,000 or more" ~ "$150,000 or more"
  )) 
  
  
## TABLE B25014 - Tenure by Occupants per Bedroom  -----------------------------

b25014_vars <- load_table_vars("B25014_", 2023) %>% 
  separate(label, into = c("est", "total", "tenure", "occupants"), sep = "!!") %>% 
  select(variable = name,tenure, occupants) %>% 
  drop_na() %>% 
  mutate(
    across(.fns = ~str_remove_all(.x, ":")),
    tenure = str_remove_all(tenure, " occupied"),
    occupants = str_remove_all(occupants, " occupants per room")
  ) %>% 
  mutate(
    overcrowded = case_when(
      occupants %in% c("0.50 or less", "0.51 to 1.00") ~ "Not overcrowded",
      occupants == "1.01 to 1.50" ~ "Overcrowded",
      TRUE ~ "Very overcrowded"
    )
  )
  
## TABLE B17001 - Poverty Status  ----------------------------------------------

b17001_vars <- load_table_vars("B17001", 2023) %>% 
  separate(label, into = c("est", "total", "poverty", "sex", "age"), sep = "!!") %>% 
  mutate(race = poverty_race(concept)) %>% 
  mutate(race = case_when(
    race == "Poverty Status In The Past 12 Months By Sex By Age" ~ "All households",
    TRUE ~ race
  )) %>% 
  select(variable = name, sex, age, race) %>% 
  drop_na() %>% 
  mutate(across(.fns = ~str_remove_all(.x, ":")))

  
## TABLE B25106 - Cost Burden ACS ----------------------------------------------

b25106_vars <- load_table_vars("B25106", 2023) %>% 
  separate(label, into = c("est", "total", "tenure", "income", "cb"), sep = "!!") %>% 
  select(variable = name, tenure, income, cb) %>% 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) %>% 
  drop_na() %>% 
  mutate(tenure = case_when(
    str_detect(tenure, "Owner") ~ "Homeowner",
    str_detect(tenure, "Renter") ~ "Renter",
    TRUE ~ tenure
  )) %>% 
  mutate(cost_burden = case_when(
    cb == "30 percent or more" ~ "Cost-burdened",
    TRUE ~ "Not cost-burdened"
  ))


## TABLE B19049 - Median Household Income by Age -------------------------------

b19049_vars <- load_table_vars("B19049", 2023) %>% 
  separate(label, into = c("est", "med", "total", "age"), sep = "!!") %>% 
  select(variable = name, age) %>% 
  drop_na() %>% 
  mutate(age = str_remove_all(age, "Householder "),
         age = str_to_sentence(age))
  

## TABLE B25119 - Median Household Income by Tenure ----------------------------

b25119_vars <- load_table_vars("B25119", 2023) %>% 
  separate(label, into = c("est", "med", "total", "tenure"), sep = "!!") %>% 
  select(variable = name, tenure) %>% 
  drop_na() %>% 
  mutate(tenure = case_when(
    str_detect(tenure, "Owner") ~ "Homeowner",
    str_detect(tenure, "Renter") ~ "Renter",
  ))

## TABLE B19013B-H - Median Household Income by Race -------------------------------


b19013_vars <- load_table_vars("B19013", 2023) %>% 
  select(variable = name, concept) %>% 
  mutate(race = str_extract(concept, "[^(]+(?=\\)$)")) %>% 
  mutate(race = str_remove_all(race, " Alone Householder")) %>% 
  mutate(race = str_remove_all(race, " Householder")) %>% 
  mutate(race = case_when(
    race == "in 2023 Inflation-Adjusted Dollars" ~ "All households",
    TRUE ~ race
  )) %>% 
  select(variable, race)
  

## TABLE B18107 - Sex by age by independent living difficulty ------------------




# PULL RAW ACS DATA ------------------------------------------------------------

tables <- c(
  "B11001",  # Household Type (including Living Alone)
  paste0("B11001", LETTERS[2:8]),  # Race variants B-H
  "B25003",  # Tenure of Occupied Housing Units  
  paste0("B25003", LETTERS[2:8]),  # Race variants B-H
  "B25004",  # Vacancy Status
  "B25007",  # Tenure by Householder Age
  "B25009",  # Tenure by Household Size  
  "B25010",  # Tenure by Average Household Size
  "B11012",  # Households by Type
  "B09021",  # Living Arrangements of Adults
  "B25042",  # Tenure by Bedrooms
  "B25032",  # Structure Type by Tenure
  "B25127",  # Tenure by Year Structure Built By Units
  "B25063",  # Gross Rent
  "B25118",  # Tenure by Household Income
  "B25014",  # Tenure by Occupants per Bedroom
  "B17001",  # Poverty Status
  paste0("B17001", LETTERS[2:8]),  # Race variants B-H
  "B25106",  # Cost Burden ACS
  "B19049",  # Median Household Income by Age
  "B25119",  # Median Household Income by Tenure
  "B19013",  # Median Household Income by Race
  paste0("B19013", LETTERS[2:8])   # Race variants B-H
)

years <- 2010:2023
survey = "acs5"

table_data <- map(tables, ~ get_va_acs(.x, "county", years, survey)) %>%
  set_names(tables)

# Process all table data with corresponding variables
# Combine race variants for B11001 (B11001, B11001B-H)
b11001_data <- bind_rows(
  table_data$B11001,
  table_data$B11001B, table_data$B11001C, table_data$B11001D,
  table_data$B11001E, table_data$B11001F, table_data$B11001G, table_data$B11001H
) %>% 
  left_join(b11001_vars, by = "variable")

# Combine race variants for B25003 (B25003, B25003B-H)
b25003_data <- bind_rows(
  table_data$B25003,
  table_data$B25003B, table_data$B25003C, table_data$B25003D,
  table_data$B25003E, table_data$B25003F, table_data$B25003G, table_data$B25003H
) %>% 
  left_join(b25003_vars, by = "variable")

b25004_data <- table_data$B25004 %>% 
  left_join(b25004_vars, by = "variable")

b25007_data <- table_data$B25007 %>% 
  left_join(b25007_vars, by = "variable")

b25009_data <- table_data$B25009 %>% 
  left_join(b25009_vars, by = "variable")

b25010_data <- table_data$B25010 %>% 
  left_join(b25010_vars, by = "variable")

b11012_data <- table_data$B11012 %>% 
  left_join(b11012_vars, by = "variable")

b09021_data <- table_data$B09021 %>% 
  left_join(b09021_vars, by = "variable")

b25042_data <- table_data$B25042 %>% 
  left_join(b25042_vars, by = "variable")

b25032_data <- table_data$B25032 %>% 
  left_join(b25032_vars, by = "variable")

b25127_data <- table_data$B25127 %>% 
  left_join(b25127_vars, by = "variable")

b25063_data <- table_data$B25063 %>% 
  left_join(b25063_vars, by = "variable")

b25118_data <- table_data$B25118 %>% 
  left_join(b25118_vars, by = "variable")

b25014_data <- table_data$B25014 %>% 
  left_join(b25014_vars, by = "variable")

# Combine race variants for B17001 (B17001, B17001B-H)
b17001_data <- bind_rows(
  table_data$B17001,
  table_data$B17001B, table_data$B17001C, table_data$B17001D,
  table_data$B17001E, table_data$B17001F, table_data$B17001G, table_data$B17001H
) %>% 
  left_join(b17001_vars, by = "variable")

b25106_data <- table_data$B25106 %>% 
  left_join(b25106_vars, by = "variable")

b19049_data <- table_data$B19049 %>% 
  left_join(b19049_vars, by = "variable")

b25119_data <- table_data$B25119 %>% 
  left_join(b25119_vars, by = "variable")

# Combine race variants for B19013 (B19013, B19013B-H)
b19013_data <- bind_rows(
  table_data$B19013,
  table_data$B19013B, table_data$B19013C, table_data$B19013D,
  table_data$B19013E, table_data$B19013F, table_data$B19013G, table_data$B19013H
) %>% 
  left_join(b19013_vars, by = "variable")
  
