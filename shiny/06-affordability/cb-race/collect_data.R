library(tidyverse)

cb9 <- read_rds("data/rds/table9_chas.rds") |> 
  mutate(cost_burden = factor(cost_burden, 
                              levels = c("Not cost-burdened", 
                                         "No or negative income", 
                                         "Cost-burdened", 
                                         "Severely cost-burdened")))



lookup <- read_csv("data/local_lookup.csv") |> 
  mutate(fips = fips_full)

cb9_join <- cb9 |> 
  left_join(lookup, by = "fips")


state <- cb9 |> 
  group_by(year, race, cost_burden, cb_group) |> 
  summarise(estimate = sum(estimate),
            moe = sqrt(sum(moe^2, na.rm = TRUE)))


cbsa <- cb9_join |> 
  group_by(year, cbsa_title, race, cost_burden, cb_group) |> 
  summarise(estimate = sum(estimate),
            moe = sqrt(sum(moe^2, na.rm = TRUE)))


juris <- cb9_join |> 
  group_by(year, name_long, race, cost_burden, cb_group) |> 
  summarise(estimate = sum(estimate),
            moe = sqrt(sum(moe^2, na.rm = TRUE)))


state_select <- state |> 
  filter(year == 2016) |> 
  group_by(race) |> 
  mutate(percent = estimate/sum(estimate)) |> 
  ungroup() |> 
  group_by(race, year) |> 
  mutate(total_cb = sum(estimate[cb_group == "Cost-burdened"])/sum(estimate)) %>%
  ungroup()

ggplot(state_select,
       aes(x = reorder(race, -total_cb),
           y = percent,
           fill = cost_burden)) +
  geom_col()


cbsa_select <- cbsa |> 
  filter(year == 2016) |> 
  filter(cbsa_title == "Charlottesville, VA") |> 
  group_by(race) |> 
  mutate(percent = estimate/sum(estimate)) |> 
  ungroup() |> 
  group_by(race, year) |> 
  mutate(total_cb = sum(estimate[cb_group == "Cost-burdened"])/sum(estimate)) %>%
  ungroup()


ggplot(cbsa_select,
       aes(x = reorder(race, -total_cb),
           y = percent,
           fill = cost_burden)) +
  geom_col()


juris_select <- juris |> 
  filter(year == 2016) |>
  filter(name_long == "Richmond City") |> 
  group_by(race) |> 
  mutate(percent = estimate/sum(estimate)) |> 
  ungroup() |> 
  group_by(race, year) |> 
  mutate(total_cb = sum(estimate[cb_group == "Cost-burdened"])/sum(estimate)) %>%
  ungroup()

ggplot(juris_select,
       aes(x = reorder(race, -total_cb), 
           y = percent,
           fill = cost_burden)) +
  geom_col()
