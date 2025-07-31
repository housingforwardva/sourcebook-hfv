library(tidyverse)

local_rent <- read_rds("data/rds/b25064_locality.rds") |> 
  select(year, locality, estimate, adjusted) |> 
  pivot_longer(3:4,
               names_to = "Rent",
               values_to = "Estimate") |> 
  mutate(Rent = case_when(
    Rent == "estimate" ~ "Nominal dollars",
    Rent == "adjusted" ~ "Real dollars"
  )) |> 
  mutate(locality = str_remove(locality, ", Virginia"))

cbsa_rent <- read_rds("data/rds/b25064_cbsa.rds")|> 
  select(year, cbsa, estimate, adjusted) |> 
  pivot_longer(3:4,
               names_to = "Rent",
               values_to = "Estimate") |> 
  mutate(Rent = case_when(
    Rent == "estimate" ~ "Nominal dollars",
    Rent == "adjusted" ~ "Real dollars"
  ))

state_rent <- read_rds("data/rds/b25064_state.rds")|> 
  select(year, state, estimate, adjusted) |> 
  pivot_longer(3:4,
               names_to = "Rent",
               values_to = "Estimate") |> 
  mutate(Rent = case_when(
    Rent == "estimate" ~ "Nominal dollars",
    Rent == "adjusted" ~ "Real dollars"
  ))


local_select <- local_rent |> 
  filter(locality == "Richmond city") |> 
  filter(Rent == "Nominal dollars")


cbsa_select <- cbsa_rent |> 
  filter(cbsa == "Richmond, VA Metro Area") |> 
  filter(Rent == "Nominal dollars")


state_select <- state_rent |> 
  filter(state == "Virginia") |> 
  filter(Rent == "Nominal dollars")

ggplot(local_select,
       aes(x = year, 
           y = Estimate)) +
  geom_line()
