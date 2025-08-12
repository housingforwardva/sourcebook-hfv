library(rsconnect)

# Create a manifest file for deployment
rsconnect::writeManifest(
  appDir = "shiny/01-dem/pop-race",                      # Current directory
  appFiles = c(                      # Files to include
    "app.R",       # Main app file
    "race_ethnicity.rds",     # Data files
    "www/styles/variables.scss",
    "www/styles/responsive.scss",
    "www/styles/components.scss",
    "www/styles/hfv-theme.scss",
    "www/styles/hfv-theme.css"
  ),
  appPrimaryDoc = "app.R"  # Main file
)
