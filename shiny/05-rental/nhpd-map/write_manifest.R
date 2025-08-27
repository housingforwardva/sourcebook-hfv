library(rsconnect)

# Create a manifest file for deployment
rsconnect::writeManifest(
  appDir = "shiny/05-rental/nhpd-map",                      # Current directory
  appFiles = c(                      # Files to include
    "app.R",       # Main app file
    "va_co_shape.rds",     # Data files
    "data.rds",
    "www/styles/hfv-theme.css"
  ),
  appPrimaryDoc = "app.R"  # Main file
)
