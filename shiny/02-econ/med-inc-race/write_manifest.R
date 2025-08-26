library(rsconnect)

# Create a manifest file for deployment
rsconnect::writeManifest(
  appDir = "shiny/02-econ/med-inc-race",                      # Current directory
  appFiles = c(                      # Files to include
    "app.R",       # Main app file
    "data.rds",     # Data files
    "www/styles/hfv-theme.css"
  ),
  appPrimaryDoc = "app.R"  # Main file
)
