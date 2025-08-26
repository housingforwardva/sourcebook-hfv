library(tidyverse)
library(paws)
library(arrow)

s3 <- s3()

# Debug: Check what we actually downloaded
s3_response <- s3$get_object(
  Bucket = "hda-data-hub",
  Key = "cfpb/hmda_va_clean.parquet"
)

# Check size
print(paste("Downloaded size:", length(s3_response$Body), "bytes"))

# Check if it starts with PAR1 (parquet magic bytes)
if(length(s3_response$Body) >= 4) {
  first_bytes <- rawToChar(s3_response$Body[1:4], multiple = TRUE)
  print(paste("First 4 bytes:", paste(first_bytes, collapse = "")))
}

# Check if it ends with PAR1
if(length(s3_response$Body) >= 4) {
  last_bytes <- rawToChar(tail(s3_response$Body, 4), multiple = TRUE)
  print(paste("Last 4 bytes:", paste(last_bytes, collapse = "")))
}

# Read all the raw bytes from the connection
raw_data <- s3_response$Body$read()

# Write to temp file
temp_file <- tempfile(fileext = ".parquet")
writeBin(raw_data, temp_file)

# Read the parquet file
hmda_data <- read_parquet(temp_file)

# Clean up
unlink(temp_file)

# Check your data
glimpse(hmda_data)