library(rsconnect)
# Create a manifest file for deployment
rsconnect::writeManifest(
  appDir = "shiny/pop_change",                      # Current directory
  appFiles = c(                      # Files to include
    "app.R",       # Main app file
    "pop_change.rds",     # Data files
    "www/hfv_logo.png"           # Assets
  ),
  appPrimaryDoc = "app.R"  # Main file
)
