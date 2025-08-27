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

write_rds(var_data, "shiny/04-homeownership/var/data.rds")


