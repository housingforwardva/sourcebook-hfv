library(tidyverse)

rent <- read_rds("data/rds/b25063.rds") |> 
  mutate(rent = case_when(
    rent %in% c("Less than $100", "$100 to $149", "$150 to $199", "$200 to $249", "$250 to $299", "$300 to $349", "$350 to $399", "$400 to $449", "$450 to $499") ~ "Under $500",
    rent %in% c("$500 to $549", "$550 to $599", "$600 to $649", "$650 to $699", "$700 to $749", "$750 to $799") ~ "$500-$799",
    rent %in% c("$800 to $899", "$900 to $999") ~ "$800-$999",
    rent == "$1,000 to $1,249" ~ "$1,000-$1,249",
    rent %in% c("$1,250 to $1,499", "$1,500 to $1,999") ~ "$1,250-$1,999",
    rent %in% c("$2,000 to $2,499", "$2,500 to $2,999", "$3,000 to $3,499", "$3,500 or more") ~ "$2,000+",
    rent == "All" & cash == "No cash rent" ~ "No cash rent"
  )) |>
  mutate(rent = factor(rent, levels = c("No cash rent", "Under $500", "$500-$799", "$800-$999", "$1,000-$1,249", "$1,250-$1,999", "$2,000+")))

lookup <- read_csv("data/local_lookup.csv") |> 
  mutate(fips = fips_full) 


rent <- rent |> 
  mutate(fips = as.numeric(fips)) |> 
  left_join(lookup, by = "fips") |> 
  drop_na(rent)


va_rent <- rent |> 
  group_by(year, cash, rent) |> 
  summarise(estimate = sum(estimate)) |> 
  filter(year == 2023)

cbsa_rent <- rent |> 
  group_by(year, cbsa_title, rent) |> 
  summarise(estimate = sum(estimate)) |> 
  filter(cbsa_title == "Richmond, VA") |> 
  filter(year == 2023)


juris_rent <- rent |> 
  group_by(year, name_long, rent)|> 
  summarise(estimate = sum(estimate)) |> 
  filter(name_long == "Richmond City") |> 
  filter(year == 2023)


ggplot(va_rent,
       aes(x = rent,
           y = estimate)) +
  geom_col()
  
  
ggplot(cbsa_rent,
         aes(x = rent,
             y = estimate)) +
  geom_col()
  


ggplot(juris_rent,
       aes(x = rent,
           y = estimate)) +
  geom_col()
