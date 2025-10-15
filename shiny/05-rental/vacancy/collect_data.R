library(tidyverse)

vacancy <- read_rds("data/rds/renter_vacancy.rds")


state <- vacancy |> 
  group_by(year) |> 
  summarise(renter_occupied = sum(renter_occupied),
            for_rent_vacant = sum(for_rent_vacant),
            rented_vacant = sum(rented_vacant),
            total_units = sum(total_units)) |> 
  ungroup() |> 
  mutate(rate = (total_units - renter_occupied)/total_units)


cbsa <- vacancy |> 
  group_by(year, cbsa_title) |> 
  summarise(renter_occupied = sum(renter_occupied),
            for_rent_vacant = sum(for_rent_vacant),
            rented_vacant = sum(rented_vacant),
            total_units = sum(total_units)) |> 
  ungroup() |> 
  mutate(rate = (total_units - renter_occupied)/total_units)


juris <- vacancy |> 
  group_by(year, name_long) |> 
  summarise(renter_occupied = sum(renter_occupied),
            for_rent_vacant = sum(for_rent_vacant),
            rented_vacant = sum(rented_vacant),
            total_units = sum(total_units)) |> 
  ungroup() |> 
  mutate(rate = (total_units - renter_occupied)/total_units)


cbsa_select <- cbsa |> 
  filter(cbsa_title == "Big Stone Gap, VA")

juris_select <- juris |> 
  filter(name_long == "Richmond City")


ggplot(state,
       aes(x = year,
           y = rate)) +
  geom_line()


ggplot(cbsa_select,
       aes(x = year,
           y = rate)) +
  geom_line()


ggplot(juris_select,
       aes(x = year,
           y = rate)) +
  geom_line()