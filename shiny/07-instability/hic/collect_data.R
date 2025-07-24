library(tidyverse)


hic <- read_rds("data/rds/va_hic_data.rds") 

|>
  pivot_longer(cols = -c(1, year),  # exclude first column and year column
               names_to = "type",
               values_to = "value",
               values_transform = as.numeric) 


library(stringdist)

unique_types <- unique(hic$type)

# Function to find similar strings
find_similar <- function(strings, max_dist = 2) {
  similar_pairs <- data.frame()
  
  for(i in 1:(length(strings)-1)) {
    for(j in (i+1):length(strings)) {
      dist <- stringdist(strings[i], strings[j], method = "lv")  # Levenshtein distance
      if(dist <= max_dist) {
        similar_pairs <- rbind(similar_pairs, 
                              data.frame(string1 = strings[i], 
                                        string2 = strings[j], 
                                        distance = dist))
      }
    }
  }
  return(similar_pairs)
}

# Find strings within 2 character changes of each other
similar_matches <- find_similar(unique_types, max_dist = 2)
View(similar_matches)


hic_clean <- hic |> 
  mutate(type = str_remove(type, "_\\d+$")) |> 
  group_by(co_c_number, year, type) |> 
  summarise(value = sum(as.numeric(value)))
