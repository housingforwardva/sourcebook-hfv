library(tidyverse)

cb7 <- read_rds("data/rds/table7_chas.rds") |> 
  mutate(cost_burden = factor(cost_burden, 
                              levels = c("Not cost-burdened", 
                                         "No or negative income", 
                                         "Cost-burdened", 
                                         "Severely cost-burdened"))) |> 
  mutate(household_income = factor(household_income, 
                                   levels = c("30% AMI or less", 
                                              "31 to 50% AMI", 
                                              "51 to 80% AMI", 
                                              "81 to 100% AMI",
                                              "101% AMI or greater"
                                   )))



lookup <- read_csv("data/local_lookup.csv") |> 
  mutate(fips = fips_full)

cb7_join <- cb7 |> 
  left_join(lookup, by = "fips")


state <- cb7 |> 
  group_by(year, tenure, household_type, cost_burden, cb_group) |> 
  summarise(estimate = sum(estimate),
            moe = sqrt(sum(moe^2, na.rm = TRUE)))


cbsa <- cb7_join |> 
  group_by(year, cbsa_title, tenure, household_income, cost_burden, cb_group) |> 
  summarise(estimate = sum(estimate),
            moe = sqrt(sum(moe^2, na.rm = TRUE)))


juris <- cb7_join |> 
  group_by(year, name_long, tenure, household_income, cost_burden, cb_group) |> 
  summarise(estimate = sum(estimate),
            moe = sqrt(sum(moe^2, na.rm = TRUE)))


state_select <- state |> 
  filter(year == 2016) |> 
  filter(tenure == "Renter") |> 
  group_by(household_income) |> 
  mutate(percent = estimate/sum(estimate))

ggplot(state_select,
       aes(x = household_income,
           y = percent,
           fill = cost_burden)) +
  geom_col()


cbsa_select <- cbsa |> 
  filter(year == 2016) |> 
  filter(tenure == "Renter") |> 
  group_by(household_income) |> 
  mutate(percent = estimate/sum(estimate))

ggplot(cbsa_select,
       aes(x = household_income,
           y = percent,
           fill = cost_burden)) +
  geom_col()


juris_select <- juris |> 
  filter(year == 2016) |> 
  filter(tenure == "Renter") |> 
  group_by(household_income) |> 
  mutate(percent = estimate/sum(estimate))

ggplot(juris_select,
       aes(x = household_income,
           y = percent,
           fill = cost_burden)) +
  geom_col()





