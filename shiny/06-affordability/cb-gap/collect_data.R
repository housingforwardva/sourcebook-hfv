library(tidyverse)


gap <- read_rds("data/rds/table18c_chas.rds") |> 
  mutate(household_income = factor(household_income, 
                                   levels = c("30% AMI or less", 
                                              "31 to 50% AMI", 
                                              "51 to 80% AMI", 
                                              "81% AMI or greater"
                                   ))) |> 
  mutate(match = factor(match, 
                        levels = c("Very affordable", 
                                    "Affordable", 
                                    "Unaffordable" 
                                   )))

lookup <- read_csv("data/local_lookup.csv") |> 
  mutate(fips = fips_full)

gap_join <- gap |> 
  left_join(lookup, by = "fips")

state <- gap_join |> 
  group_by(year, household_income, match, gapcode) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  mutate(estimate = case_when(
    gapcode == "Gap" ~ -estimate,
    TRUE ~ estimate
  ))

cbsa <- gap_join |> 
  group_by(year, cbsa_title, household_income, match, gapcode) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  mutate(estimate = case_when(
    gapcode == "Gap" ~ -estimate,
    TRUE ~ estimate
  ))


juris <- gap_join |> 
  group_by(year, name_long, household_income, match, gapcode) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  mutate(estimate = case_when(
    gapcode == "Gap" ~ -estimate,
    TRUE ~ estimate
  ))


state_select <- state |> 
  filter(year == 2016)

ggplot(state,
       aes(x = household_income,
           y = estimate, 
           fill = match)) +
  geom_col(position = "stack")


cbsa_select <- cbsa |> 
  filter(year == 2016) |> 
  filter(cbsa_title == "Charlottesville, VA")

ggplot(cbsa_select,
       aes(x = household_income,
           y = estimate, 
           fill = match)) +
  geom_col(position = "stack")

juris_select <- juris |> 
  filter(year == 2016) |> 
  filter(name_long == "Richmond City")

ggplot(juris_select,
       aes(x = household_income,
           y = estimate, 
           fill = match)) +
  geom_col(position = "stack")