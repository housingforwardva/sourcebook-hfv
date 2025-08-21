library(rsconnect)

# Create a manifest file for deployment
rsconnect::writeManifest(
  appDir = "shiny/01-dem/total-pop-chg",                      # Current directory
  appFiles = c(                      # Files to include
    "app.R",       # Main app file
    "total_pop.rds",     # Data files
    "www/styles/hfv-theme.css"
  ),
  appPrimaryDoc = "app.R"  # Main file
)
