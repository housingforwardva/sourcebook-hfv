# =============================================================================
# HFV SHARED COLOR SYSTEM
# Centralized color definitions for all Shiny apps
# =============================================================================

# HFV Brand Colors
hfv_colors <- list(
  sky = "#40C0C0",
  grass = "#259591",
  lilac = "#8B85CA", 
  shadow = "#011E41",
  shadow_light = "#102C54",
  berry = "#B1005F",
  desert = "#E0592A"
)

# Race/Ethnicity Colors
race_colors <- c(
  "White, Not Hispanic Or Latino" = "#40C0C0",
  "Black" = "#011E41",
  "Asian" = "#259591",
  "Some Other Race" = "#E0592A",
  "Multiracial" = "#B1005F",
  "American Indian/Alaska Native" = "#8B85CA",
  "Native Hawaiian/Pacific Islander" = "#FFC658",
  "All households" = "#FF7276"
)

# Age Group Colors
age_colors <- c(
  "17 years and under" = "#FFC658",
  "18 to 24 years" = "#E0592A",
  "25 to 34 years" = "#259591",
  "35 to 44 years" = "#40C0C0",
  "45 to 54 years" = "#8B85CA",
  "55 to 64 years" = "#B1005F",
  "65 to 74 years" = "#011E41",
  "75 years and over" = "#102C54"
)

# Housing Tenure Colors
tenure_colors <- c(
  "All households" = "#011E41",    # HFV Shadow
  "Homeowner" = "#40C0C0",         # HFV Sky - stable/positive
  "Renter" = "#B1005F"             # HFV Berry - different state
)

# Income Bracket Colors (ordered from low to high)
income_colors <- c(
  "Under $25,000" = "#B1005F",
  "$25,000 to $49,999" = "#E0592A",
  "$50,000 to $74,999" = "#FFC658",
  "$75,000 to $99,999" = "#259591",
  "$100,000 to $149,999" = "#40C0C0",
  "$150,000 and over" = "#8B85CA"
)

# Helper function to get colors by category
get_hfv_colors <- function(category = c("race", "age", "tenure", "income", "brand")) {
  category <- match.arg(category)
  
  switch(category,
    "race" = race_colors,
    "age" = age_colors, 
    "tenure" = tenure_colors,
    "income" = income_colors,
    "brand" = hfv_colors
  )
}