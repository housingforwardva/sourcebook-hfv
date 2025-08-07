library(rsconnect)cla
# Create a manifest file for deployment

rsconnect::writeManifest(
  appDir = "shiny/01-dem/total-pop", # Current directory
  appFiles = c(                      # Files to include
    "app.R",       # Main app file
    "total_pop.rds",     # Data files
    "www/hfv_logo.png",
    "www/styles/variables.scss",
    "www/styles/responsive.scss",
    "www/styles/components.scss",
    "www/styles/hfv-theme.scss",
    "www/styles/hfv-theme.css"
  ),
  appPrimaryDoc = "app.R"  # Main file
)
