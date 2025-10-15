library(rsconnect)

# Create a manifest file for deployment
rsconnect::writeManifest(
  appDir = "shiny/01-dem/living-arr",                      # Current directory
  appFiles = c(                      # Files to include
    "app.R",       # Main app file
    "b09021_data.rds"     # Data files
  ),
  appPrimaryDoc = "app.R"  # Main file
)
