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
  group_by(year, tenure, household_type, household_income, cost_burden, cb_group) |> 
  summarise(estimate = sum(estimate),
            moe = sqrt(sum(moe^2, na.rm = TRUE)))


cbsa <- cb7_join |> 
  group_by(year, cbsa_title, tenure, household_type, household_income, cost_burden, cb_group) |> 
  summarise(estimate = sum(estimate),
            moe = sqrt(sum(moe^2, na.rm = TRUE)))


juris <- cb7_join |> 
  group_by(year, name_long, tenure, household_type, household_income, cost_burden, cb_group) |> 
  summarise(estimate = sum(estimate),
            moe = sqrt(sum(moe^2, na.rm = TRUE)))


state_select <- state |> 
  filter(year == 2016) |> 
  filter(tenure == "Renter") |> 
  group_by(household_type) |> 
  mutate(percent = estimate/sum(estimate))

ggplot(state_select,
       aes(x = household_type,
           y = percent,
           fill = cost_burden)) +
  geom_col()


cbsa_select <- cbsa |> 
  filter(year == 2016) |> 
  filter(tenure == "Renter") |> 
  group_by(household_type) |> 
  mutate(percent = estimate/sum(estimate))

ggplot(cbsa_select,
       aes(x = household_type,
           y = percent,
           fill = cost_burden)) +
  geom_col()


juris_select <- juris |> 
  filter(year == 2016) |> 
  filter(tenure == "Renter") |> 
  group_by(household_type) |> 
  mutate(percent = estimate/sum(estimate))

ggplot(juris_select,
       aes(x = household_type,
           y = percent,
           fill = cost_burden)) +
  geom_col()


# Allow users the ability to also see a breakdown of household type and household income.

state_select <- state |> 
  filter(year == 2016) |> 
  filter(tenure == "Renter") |> 
  group_by(household_type, household_income) |> 
  mutate(percent = estimate/sum(estimate))

ggplot(state_select,
       aes(x = household_type,
           y = percent,
           fill = cost_burden)) +
  geom_col() +
  facet_wrap(~household_income, nrow = 1) +
  coord_flip()


cbsa_select <- cbsa |> 
  filter(year == 2016) |> 
  filter(tenure == "Renter") |> 
  group_by(household_type, household_income) |> 
  mutate(percent = estimate/sum(estimate))

ggplot(cbsa_select,
       aes(x = household_type,
           y = percent,
           fill = cost_burden)) +
  geom_col() +
  facet_wrap(~household_income, nrow = 1) +
  coord_flip()


juris_select <- juris |> 
  filter(year == 2016) |> 
  filter(tenure == "Renter") |> 
  group_by(household_type, household_income) |> 
  mutate(percent = estimate/sum(estimate))

filtered_juris <- ggplot(juris_select,
       aes(x = household_type,
           y = percent,
           fill = cost_burden)) +
  geom_col() +
  facet_wrap(~household_income, nrow = 1) +
  coord_flip()


test_plot <- function(data) {
  ggplot(data, aes(x = household_type, y = percent, fill = cost_burden)) +
    geom_col_interactive(
      aes(tooltip = paste("Test:", cost_burden)),
      position = "stack"
    ) +
    scale_fill_manual(
      values = c(
        "Not cost-burdened" = "#40C0C0",
        "No or negative income" = "#8B85CA",
        "Cost-burdened" = "#E0592A",
        "Severely cost-burdened" = "#B1005F"
      )
    ) +
    theme_minimal()
}

# Then test with minimal girafe options:
girafe(ggobj = test_plot(juris_select))





