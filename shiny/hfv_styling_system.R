# =============================================================================
# HFV SHARED STYLING IMPLEMENTATION EXAMPLE
# How to use the new shared CSS architecture in your Shiny apps
# =============================================================================

library(shiny)
library(bslib)
library(sass)

# -----------------------------------------------------------------------------
# STEP 1: COMPILE SCSS TO CSS (Run once when styles change)
# -----------------------------------------------------------------------------

compile_hfv_styles <- function() {
  # Compile SCSS to CSS
  sass(
    list(
      # Main theme file imports all others
      sass_file("www/styles/hfv-theme.scss")
    ),
    output = "www/styles/hfv-theme.css",
    options = sass_options(
      output_style = "compressed",  # Minify for production
      source_map_embed = FALSE      # Disable source maps for production
    )
  )
  
  message("✅ HFV styles compiled successfully!")
}

# Run this once to compile your styles
# compile_hfv_styles()

# -----------------------------------------------------------------------------
# STEP 2: SHARED HFV THEME CONFIGURATION
# -----------------------------------------------------------------------------

# HFV Color Palette (centralized)
hfv_colors <- list(
  sky = "#40C0C0",           # Primary teal
  grass = "#259591",         # Dark teal/success
  lilac = "#8B85CA",         # Purple/info
  shadow = "#011E41",        # Dark navy/secondary
  shadow_light = "#102C54",  # Lighter navy
  berry = "#B1005F",         # Magenta/danger
  desert = "#E0592A"         # Orange/warning
)

# Create standardized bslib theme
create_hfv_theme <- function(bootstrap_version = 5) {
  bs_theme(
    version = bootstrap_version,
    bg = "#ffffff",
    fg = "#333333",
    primary = hfv_colors$sky,
    secondary = hfv_colors$shadow,
    success = hfv_colors$grass,
    info = hfv_colors$lilac,
    warning = hfv_colors$desert,
    danger = hfv_colors$berry,
    base_font = font_google("Open Sans"),
    heading_font = font_google("Poppins"),
    font_scale = 0.8
  )
}

# -----------------------------------------------------------------------------
# STEP 3: SHARED COMPONENT FUNCTIONS
# -----------------------------------------------------------------------------

# Standard HFV header component
hfv_header <- function(title, logo_url = NULL, compact = FALSE) {
  # Default logo if none provided
  if (is.null(logo_url)) {
    logo_url <- "https://housingforwardva.org/wp-content/uploads/2025/05/HousingForward-VA-Logo-Files-Icon-One-Color-RGB.png"
  }
  
  header_class <- if (compact) "hfv-header hfv-header--compact" else "hfv-header"
  
  div(
    class = header_class,
    img(src = logo_url, alt = "HousingForward VA Logo", class = "hfv-logo"),
    h4(title, class = "hfv-title")
  )
}

# Standard HFV sidebar component
hfv_sidebar <- function(..., source_text = NULL) {
  sidebar_content <- list(...)
  
  # Add source information if provided
  if (!is.null(source_text)) {
    sidebar_content <- append(sidebar_content, list(
      hr(class = "hfv-sidebar__divider"),
      div(
        class = "hfv-sidebar__source",
        p(source_text)
      )
    ))
  }
  
  div(class = "hfv-sidebar", sidebar_content)
}

# Standard HFV card component
hfv_card <- function(title = NULL, ..., variant = NULL) {
  card_class <- "hfv-card"
  if (!is.null(variant)) {
    card_class <- paste(card_class, paste0("hfv-card--", variant))
  }
  
  card_content <- list(...)
  
  if (!is.null(title)) {
    card_content <- c(
      list(div(class = "hfv-card__header", h5(title, class = "hfv-card__title"))),
      list(div(class = "hfv-card__body", card_content))
    )
  } else {
    card_content <- list(div(class = "hfv-card__body", card_content))
  }
  
  div(class = card_class, card_content)
}

# Chart container with loading state
hfv_chart_container <- function(output_id, type = "plot", height = NULL) {
  container_class <- paste("hfv-chart-container", paste0("hfv-chart-container--", type))
  
  container_style <- NULL
  if (!is.null(height)) {
    container_style <- paste0("height: ", height, ";")
  }
  
  div(
    class = container_class,
    style = container_style,
    # Loading indicator
    div(
      id = paste0(output_id, "_loading"),
      class = "hfv-chart-loading",
      div(class = "hfv-spinner")
    ),
    # Actual chart output
    if (type == "map") {
      maplibreOutput(output_id, height = "100%")
    } else {
      plotlyOutput(output_id, height = "100%")
    }
  )
}

# -----------------------------------------------------------------------------
# STEP 4: EXAMPLE APP USING NEW SHARED SYSTEM
# -----------------------------------------------------------------------------

# Example UI using the new shared components
example_ui <- function() {
  page_fillable(
    # Apply HFV theme and include compiled CSS
    theme = create_hfv_theme(),
    
    # Include compiled CSS file
    tags$head(
      tags$link(rel = "stylesheet", href = "styles/hfv-theme.css"),
      # Mobile viewport
      tags$meta(
        name = "viewport", 
        content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
      )
    ),
    
    # Use shared container class
    div(
      class = "hfv-container",
      
      # Use shared header component
      hfv_header("Building Permits Dashboard", compact = TRUE),
      
      # Layout using bslib
      layout_columns(
        col_widths = c(3, 9),
        gap = "10px",
        
        # Sidebar using shared component
        hfv_sidebar(
          h5("Filters", class = "hfv-sidebar__title"),
          
          div(
            class = "hfv-sidebar__section",
            selectInput(
              "metric", 
              "Select Metric:",
              choices = list("Units" = "units", "Buildings" = "bldgs", "Value" = "value"),
              selected = "units"
            )
          ),
          
          div(
            class = "hfv-sidebar__section",
            checkboxGroupInput(
              "types", 
              "Building Types:",
              choices = list("1-unit" = "1-unit", "2-4 units" = "2-4 units", "5+ units" = "5+ units"),
              selected = c("1-unit", "2-4 units", "5+ units")
            )
          ),
          
          source_text = "Source: U.S. Census Bureau Building Permits Survey"
        ),
        
        # Main content area
        div(
          # Navigation tabs with shared styling
          navset_tab(
            nav_panel(
              "State Overview",
              hfv_chart_container("state_plot", type = "plot")
            ),
            nav_panel(
              "Metro Areas", 
              hfv_chart_container("metro_plot", type = "plot")
            ),
            nav_panel(
              "Map View",
              hfv_chart_container("map_plot", type = "map")
            )
          )
        )
      )
    )
  )
}

# Example server function
example_server <- function(input, output, session) {
  # Hide loading indicators when plots are ready
  observe({
    shinyjs::hide("state_plot_loading")
    shinyjs::hide("metro_plot_loading")
    shinyjs::hide("map_plot_loading")
  })
  
  # Example plot output
  output$state_plot <- renderPlotly({
    # Your plot code here
    p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
      geom_point(color = hfv_colors$sky) +
      theme_minimal() +
      labs(title = "Example Plot")
    
    ggplotly(p)
  })
  
  # Add more outputs as needed
}

# -----------------------------------------------------------------------------
# STEP 5: MIGRATION HELPER FUNCTIONS
# -----------------------------------------------------------------------------

# Function to help migrate existing apps
migrate_app_styling <- function(app_dir) {
  cat("🔄 Migrating app styling in:", app_dir, "\n")
  
  # Check if app.R exists
  app_file <- file.path(app_dir, "app.R")
  if (!file.exists(app_file)) {
    stop("❌ No app.R file found in: ", app_dir)
  }
  
  # Read current app.R
  app_content <- readLines(app_file)
  
  # Backup original
  backup_file <- file.path(app_dir, "app.R.backup")
  writeLines(app_content, backup_file)
  cat("📁 Backup created:", backup_file, "\n")
  
  # TODO: Add migration logic to:
  # 1. Replace inline CSS with shared class references
  # 2. Update color references to use hfv_colors
  # 3. Standardize layout components
  # 4. Add shared CSS include
  
  cat("✅ Migration complete! Review changes and test thoroughly.\n")
}

# Function to validate HFV color usage
validate_hfv_colors <- function(app_dir) {
  cat("🎨 Validating color usage in:", app_dir, "\n")
  
  app_file <- file.path(app_dir, "app.R")
  if (!file.exists(app_file)) {
    stop("❌ No app.R file found in: ", app_dir)
  }
  
  content <- readLines(app_file)
  
  # Check for hardcoded colors that should use variables
  hardcoded_colors <- c("#40C0C0", "#259591", "#8B85CA", "#011E41", "#102C54", "#B1005F", "#E0592A")
  
  issues <- c()
  for (i in seq_along(content)) {
    line <- content[i]
    for (color in hardcoded_colors) {
      if (grepl(color, line, fixed = TRUE)) {
        issues <- c(issues, paste0("Line ", i, ": Found hardcoded color ", color))
      }
    }
  }
  
  if (length(issues) > 0) {
    cat("⚠️  Color issues found:\n")
    cat(paste(issues, collapse = "\n"), "\n")
    cat("\n💡 Consider using hfv_colors$[color_name] instead.\n")
  } else {
    cat("✅ No hardcoded colors found!\n")
  }
  
  invisible(issues)
}

# -----------------------------------------------------------------------------
# STEP 6: USAGE INSTRUCTIONS
# -----------------------------------------------------------------------------

# To use this new system in your apps:
#
# 1. Compile the SCSS (run once):
#    compile_hfv_styles()
#
# 2. In your app.R, replace existing theme setup with:
#    theme = create_hfv_theme()
#    tags$head(tags$link(rel = "stylesheet", href = "styles/hfv-theme.css"))
#
# 3. Replace inline CSS with shared classes:
#    - Replace custom headers with: hfv_header("Your Title")
#    - Replace custom sidebars with: hfv_sidebar(your_content)  
#    - Replace custom cards with: hfv_card("Title", your_content)
#    - Replace chart containers with: hfv_chart_container("plot_id")
#
# 4. Use consistent container structure:
#    div(class = "hfv-container", your_content)
#
# 5. Replace hardcoded colors with hfv_colors$color_name
#
# 6. Test responsive behavior on different screen sizes
#
# 7. Validate with: validate_hfv_colors("path/to/your/app")

# Run the example app
# shinyApp(ui = example_ui(), server = example_server)