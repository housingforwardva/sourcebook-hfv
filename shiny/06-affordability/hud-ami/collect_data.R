library(tidyverse)

hud_il <- read_rds("data/rds/va_hud_ami.rds") |> 
 mutate(ami = factor(ami, levels = c("Extremely low-income",
                                     "Very low-income",
                                     "Low-income"))) |> 
  mutate(ami_pct = case_when(
    ami == "Extremely low-income" ~ "30% AMI",
    ami == "Very low-income" ~ "50% AMI",
    ami == "Low-income" ~ "80% AMI"
  ))

select_juris <- hud_il |> 
  filter(county_name == "Richmond city") |> # Drop down filter for user
  filter(hh_size == "One-person") # Drop down filter for user


ggplot(select_juris,
       aes(x = year,
           y = limit,
           fill = ami_pct)) +
  geom_col() +
  facet_wrap(~ami_pct, nrow = 1)
