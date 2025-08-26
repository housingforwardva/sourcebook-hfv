library(rsconnect)

# Create a manifest file for deployment
rsconnect::writeManifest(
  appDir = "shiny/05-rental/med-rent",                      # Current directory
  appFiles = c(                      # Files to include
    "app.R",       # Main app file
    "data.rds",     # Data files
    "www/styles/hfv-theme.css"
  ),
  appPrimaryDoc = "app.R"  # Main file
)
