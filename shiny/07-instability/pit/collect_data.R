library(tidyverse)

# Collect Point-in-Time count data and filter for specific categories needed.
pit <- read_csv("data/pit_data_virginia_longer.csv") |> 
  select(coc_num = co_c_number, coc_name = co_c_name, year, category, value) |> 
  filter(category == "Total Sheltered Homeless" | category == "Total Unsheltered Homeless")


# Create a state only data frame that aggregates to that geographic-level.
state_pit <- pit |> 
  group_by(year, category) |> 
  summarise(value = sum(value)) |> 
  ungroup() |> 
  mutate(coc_num = "VA-000", coc_name = "Statewide", name = "Virginia")

# Create a Continuum of Care data frame that has all CoCs.
coc_pit  <- pit |> 
  mutate(name = case_when(
    coc_num == "VA-500" ~ "Greater Richmond CoC", 
    coc_num == "VA-501" ~ "Southeastern Virginia Homeless Coaltion", 
    coc_num == "VA-502" ~ "Blue Ridge Interagency Council on Homelessness", 
    coc_num == "VA-503" ~ "BEACH Community Partnership", 
    coc_num == "VA-504" ~ "Thomas Jefferson Area Coalition for the Homeless", 
    coc_num == "VA-505" ~ "Greater Virginia Peninsula Homelessness Consortium", 
    coc_num == "VA-507" ~ "Portsmouth Homeless Action Consortium", 
    coc_num == "VA-508" ~ "Central Virginia CoC", 
    coc_num == "VA-513" ~ "Western Virginia CoC", 
    coc_num == "VA-514" ~ "Fredericksburg Regional CoC", 
    coc_num == "VA-521" ~ "Virginia Balance of State", 
    coc_num == "VA-600" ~ "Arlington County CoC", 
    coc_num == "VA-601" ~ "Fairfax County Office to Prevent and End Homelessness", 
    coc_num == "VA-602" ~ "Loudoun County CoC", 
    coc_num == "VA-603" ~ "The Partnership to Prevent and End Homelessness in the City of Alexandria", 
    coc_num == "VA-604" ~ "Prince William Area CoC", 
  )) 

va_pit <- rbind(state_pit, coc_pit)

library(rmapshaper)

coc_geo <- sf::st_read("data/geo/virginia_coc.gpkg") |> 
  ms_simplify() |> 
  mutate(coc_num = COCNUM)

library(mapgl)

maplibre(
  bounds = coc_geo
) |> 
  add_fill_layer(
    id = "coc_layer",
    source = coc_geo,
    fill_color = "blue",
    fill_opacity = 0.5)

va_pit_selection <- va_pit |> 
  filter(coc_num == "VA-500")

ggplot(va_pit_selection,
aes(x = year,
  y = value, 
fill = category)) +
  geom_col(position = "stack")


