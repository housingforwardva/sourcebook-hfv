library(rsconnect)

# Create a manifest file for deployment
rsconnect::writeManifest(
  appDir = "shiny/01-dem/pop-change",                      # Current directory
  appFiles = c(                      # Files to include
    "app.R",       # Main app file
    "pop_change.rds"     # Data files
  ),
  appPrimaryDoc = "app.R"  # Main file
)
