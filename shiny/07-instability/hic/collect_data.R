library(tidyverse)


hic <- read_rds("data/rds/hic_va_data.rds") |> 
  janitor::clean_names() |> 
  select(coc_num = co_c_number, year, 3:8) |> 
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
  )) |>
  pivot_longer(cols = -c(1, year, name),
               names_to = "type",
               values_to = "value",
               values_transform = as.numeric) |> 
  mutate(category = case_when(
    type == "total_year_round_beds_es" ~ "Emergency Shelter",
    type == "total_year_round_beds_th" ~ "Transitional Housing",
    type == "total_year_round_beds_sh" ~ "Safe Haven",
    type == "total_year_round_beds_rrh" ~ "Rapid Rehousing",
    type == "total_year_round_beds_psh" ~ "Permanent Supportive Housing",
    type == "total_year_round_beds_rrh" ~ "Rapid Rehousing",
    type == "total_year_round_beds_oph" ~ "Other Permanent Housing"
  )) |> 
  drop_na()

hic_va <- hic |> 
  group_by(year, type, category) |> 
  summarise(value = sum(value)) |> 
  ungroup() |> 
  mutate(coc_num = "VA-000",
         name = "Statewide")

hic_coc <- rbind(hic, hic_va)

write_rds(hic_coc, "shiny/07-instability/hic/hic_va_data.rds")

hic_selected <- hic_coc |> 
  filter(coc_num == "VA-500") 


ggplot(hic_selected, 
       aes(x = year,
           y = value,
           fill = category)) +
  geom_col(position = "stack")
