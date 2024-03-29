library(devtools)

install_github("keberwein/blscrapeR")

library(tidyverse)
library(blscrapeR)
library(tigris)


# The following pulls all localities in Virginia to help iterate through BLS series.
# The TIGRIS package allows us to do this and pull FIPS codes.

virginia <- list_counties("VA")

# The following creates an additional column of series ids based on BLS labels. The
# mutate() function creates a new column called 'seriesid' while the paste0() function
# concatenates the necessary pieces of the series id without spaces.

virginia <- virginia %>% mutate(seriesid = paste0("LAUCN51", county_code, "0000000005"), #employment
                                rateseries = paste0("LAUCN51", county_code, "0000000003"), #unemployment rate
                                lfseries = paste0("LAUCN51", county_code, "0000000006"), #labor force
                                unemployedseries = paste0("LAUCN51", county_code, "0000000004") # unemployment
)

# Only the 'seriesid' field is needed to pull from the BLS API, therefore we need to
# create a list of series ids for all 136 Virginia localities. 

list <- list(virginia$seriesid)
ratelist <- list(virginia$rateseries)
lflist <- list(virginia$lfseries)
unemploymentlist <- list(virginia$unemployedseries)

# The blscrapeR does not accept a list in its query pull and requires a vector. Therefore, 
# unlist() is used in order to simplifies the list to a vector.

list <- unlist(list)   
ratelist <- unlist(ratelist)
lflist <- unlist(lflist)
unemploymentlist <- unlist(unemploymentlist)

# The BLS API has a series query limit of 50, but there are 136 localities (series ids)
# to pull. So we need to split the vector into chunks using the split() function.

list <- split(list, ceiling(seq_along(list)/50))
ratelist <- split(ratelist, ceiling(seq_along(ratelist)/50))
lflist <- split(lflist, ceiling(seq_along(lflist)/50))
unemploymentlist <- split(unemploymentlist, ceiling(seq_along(unemploymentlist)/50))


# A function is needed that will run through the bls_api() using the chunked vector we
# created previously. This function will be utilized in map_dfr() to combine the three 
# iterations together into a single table

bls_employment_pull <- function(list){
  bls_api(list,
          startyear = 2008, endyear = 2023, Sys.getenv("BLS_KEY")) %>%
    dateCast()
}

va_employed <- map_dfr(list, bls_employment_pull)

bls_unemployment_rate_pull <- function(ratelist){
  bls_api(ratelist,
          startyear = 2008, endyear = 2023, Sys.getenv("BLS_KEY")) %>%
    dateCast()
}

va_unemployment_rate <- map_dfr(ratelist, bls_unemployment_rate_pull)

bls_laborforce_pull <- function(lflist){
  bls_api(lflist,
          startyear = 2008, endyear = 2023, Sys.getenv("BLS_KEY")) %>%
    dateCast()
}

va_laborforce <- map_dfr(lflist, bls_laborforce_pull)

bls_unemployment_pull <- function(unemploymentlist){
  bls_api(unemploymentlist,
          startyear = 2008, endyear = 2023, Sys.getenv("BLS_KEY")) %>%
    dateCast()
}

va_unemployment <- map_dfr(unemploymentlist, bls_unemployment_pull)


# Now that a data frame is created with all localities and years needed, we need to create
# an additional column for the FIPS code of each locality, so that we can join to a table 
# of locality names later.

fip_function <- function(x) {
  x %>%
    mutate(fips = substr(seriesID, 6, 10))
}

result <- list(va_employed, va_unemployment_rate, va_laborforce, va_unemployment) %>% 
  lapply(fip_function)

names(result) = c('va_employed', 'va_unemployment_rate', 'va_laborforce', 'va_unemployment')

list2env(result, envir = .GlobalEnv)


# Write to rds.

write_rds(va_employed, "data/va_employment_level.rds")
write_rds(va_unemployment_rate, "data/va_unemployment_rate.rds")
write_rds(va_laborforce, "data/va_laborforce_level.rds")
write_rds(va_unemployment, "data/va_unemployment_level.rds")