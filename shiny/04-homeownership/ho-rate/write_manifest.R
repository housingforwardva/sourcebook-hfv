library(rsconnect)

# Create a manifest file for deployment
rsconnect::writeManifest(
  appDir = "shiny/04-homeownership/ho-rate",                      # Current directory
  appFiles = c(                      # Files to include
    "app.R",       # Main app file
    "va_co_shape.rds",     # Data files
    "trend_data.rds", 
    "tract_data_simplified.rds",
    "www/styles/hfv-theme.css"
  ),
  appPrimaryDoc = "app.R"  # Main file
)
