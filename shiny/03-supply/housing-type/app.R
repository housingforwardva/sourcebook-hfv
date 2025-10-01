library(shiny)
library(tidyverse)
library(ggiraph)     # For interactive ggplots
library(here)        # For here() function in file paths
library(grid)        # For grobs
library(png)         # For reading PNG files
library(bslib)       # For modern UI components
library(cowplot)     # For adding logo to plots
library(scales)      # For number_format
library(shinyjs)     # For dynamic UI updates
library(magick)      # For image handling
library(sass)        # For SCSS compilation
library(gdtools)
library(gfonts)

# =============================================================================
# HFV STYLING SYSTEM INTEGRATION
# =============================================================================

# Register Google Fonts for ggiraph plots and system
register_gfont("Open Sans")
register_gfont("Poppins")

# Register fonts with systemfonts using Google Fonts URLs
tryCatch({
  # For local development and server rendering, we'll use fallback fonts
  # The web fonts are handled by the HTML dependencies in girafe
  message("Google Fonts registered for web rendering")
}, error = function(e) {
  message("Font registration warning: ", e$message)
})

# Compile HFV styles if needed (for deployment compatibility)
compile_hfv_styles_if_needed <- function() {
  css_file <- "www/styles/hfv-theme.css"
  scss_file <- "www/styles/hfv-theme.scss"
  
  # Only compile if CSS doesn't exist or SCSS is newer
  if (!file.exists(css_file) || 
      (file.exists(scss_file) && file.mtime(scss_file) > file.mtime(css_file))) {
    
    message("🔄 Compiling HFV styles...")
    
    # Ensure the CSS directory exists
    dir.create(dirname(css_file), recursive = TRUE, showWarnings = FALSE)
    
    # Compile SCSS to CSS
    tryCatch({
      sass(
        list(sass_file(scss_file)),
        output = css_file,
        options = sass_options(
          output_style = "expanded",
          source_map_embed = FALSE
        )
      )
      message("✅ HFV styles compiled successfully!")
    }, error = function(e) {
      warning("❌ Failed to compile SCSS: ", e$message)
      warning("📝 Using fallback inline styles...")
    })
  }
  
  return(file.exists(css_file))
}

# Create HFV bslib theme (colors are defined in SCSS files)
hfv_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#333333", 
  primary = "#40C0C0",
  secondary = "#011E41",
  success = "#259591",
  info = "#8B85CA",
  warning = "#E0592A",
  danger = "#B1005F",
  base_font = "Open Sans, Helvetica Neue, Helvetica, Arial, sans-serif",
  heading_font = "Poppins, Helvetica Neue, Helvetica, Arial, sans-serif",
  font_scale = 0.8
)

# Define UI
ui <- function(request) {
  page_fillable(
  theme = hfv_theme,
  useShinyjs(), # Initialize shinyjs

  # Main container using HFV classes
  div(
    class = "hfv-container",
    
    # Header using HFV styling
    div(
      class = "hfv-header",
      h4("Housing Type Distribution", class = "hfv-title")
    ),

    # Layout using bslib layout_columns
    layout_columns(
      col_widths = c(
        lg = c(3, 9),
        md = c(4, 8), 
        sm = 12
      ),
      gap = "16px",
      
      # Sidebar Panel with HFV styling
      div(
        class = "hfv-sidebar",
        
        h5("Dashboard Controls", 
           class = "text-primary", style = "margin-bottom: 16px;"),
        
        # Year selector
        div(
          style = "margin-bottom: 16px;",
          selectInput("year", "Select Year:", choices = NULL, width = "100%", selectize = FALSE)
        ),
        
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "U.S. Census Bureau, American Community Survey 5-Year Estimates, Table B25032",
            style = "margin-bottom: 0;"
          )
        )
      ),
        
      # Main Panel
      div(
        class = "hfv-chart-container",
        style = "height: 450px; margin-top: 16px;",
        girafeOutput("plot", height = "100%")
      )
    )
  )
  )
}

# Server function
server <- function(input, output, session) {

  # Read in data
  b25032 <- read_rds(here("data", "rds", "b25032.rds"))

  # Get current geography from URL
  current_geo <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    list(
      type = query$geo %||% "state",
      cbsa = query$cbsa,
      locality = query$locality
    )
  })

  # Initialize year dropdown
  observe({
    updateSelectInput(session, "year",
                      choices = 2010:2023,
                      selected = 2023)
  })

  # Process data for state level
  state_housing <- b25032 %>%
    group_by(year, tenure, type) %>%
    summarise(estimate = sum(estimate), .groups = "drop") %>%
    group_by(year, tenure) %>%
    mutate(percent = estimate/sum(estimate)) %>%
    group_by(year) %>%
    mutate(percent_total = estimate/sum(estimate))

  # Process data for CBSA level
  cbsa_housing <- b25032 %>%
    group_by(year, cbsa_title, tenure, type) %>%
    summarise(estimate = sum(estimate), .groups = "drop") %>%
    group_by(year, cbsa_title, tenure) %>%
    mutate(percent = estimate/sum(estimate)) %>%
    group_by(year, cbsa_title) %>%
    mutate(percent_total = estimate/sum(estimate))

  # Process data for local level
  local_housing <- b25032 %>%
    group_by(year, name_long, tenure, type) %>%
    mutate(percent = estimate/sum(estimate)) %>%
    group_by(year, name_long) %>%
    mutate(percent_total = estimate/sum(estimate))
  
  # Single reactive for filtered data based on current geography
  filtered_data <- reactive({
    req(input$year)
    geo <- current_geo()

    if (geo$type == "state") {
      state_housing %>%
        filter(year == input$year)
    } else if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      cbsa_housing %>%
        filter(year == input$year,
               cbsa_title == geo$cbsa)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      local_housing %>%
        filter(year == input$year,
               name_long == geo$locality)
    } else {
      NULL
    }
  })
  
  # Create title text
  title_text <- reactive({
    geo <- current_geo()
    if (geo$type == "state") {
      paste("Virginia Housing Type Distribution -", input$year)
    } else if (geo$type == "cbsa") {
      paste("Housing Type Distribution -", geo$cbsa, "-", input$year)
    } else {
      paste("Housing Type Distribution -", geo$locality, "-", input$year)
    }
  })
  
  # Create a simplified plotting approach to completely avoid theme conflicts
  create_plot <- function(data) {
    # Add tooltip text to the data
    data <- data %>%
      mutate(tooltip = paste0(
        "Type: ", type, "\n",
        "Tenure: ", tenure, "\n",
        "Percentage: ", scales::percent(percent_total, accuracy = 0.1), "\n",
        "Count: ", format(estimate, big.mark = ",")
      ))
    
    # Create a pure, base ggplot with no theme customizations that could cause conflicts
    p <- ggplot(data, 
                aes(x = reorder(type, -percent_total), 
                    y = percent_total, 
                    fill = tenure)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = type),
        position = "dodge"
      ) +
      geom_text(aes(label = scales::percent(percent_total, accuracy = 1), 
                    color = tenure),
                position = position_dodge(width = 0.9),
                hjust = -0.2,
                size = 3.5) + # Reduced text size slightly for compact view
      facet_wrap(~tenure) +
      # Use Housing Forward Virginia colors
      scale_fill_manual(values = c("Owner" = "#40C0C0", "Renter" = "#011E41")) +
      scale_color_manual(values = c("Owner" = "#40C0C0", "Renter" = "#011E41")) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                         expand = expansion(mult = c(0, 0.3))) +
      labs(
        title = title_text(),
        caption = " " # Add empty caption to leave space for logo
      ) +
      # Use minimal theme with standardized styling
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        strip.text = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(hjust = 0.1),
        panel.spacing.x = unit(15, "pt"),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 30, 5) # Extra bottom margin for logo
      )
    
    # Add logo directly using external URL
    logo_url <- "https://housingforwardva.org/wp-content/uploads/2024/08/HousingForward-VA-Logo-Files-Horizontal-Gradient-RGB.png"
    
    # Add logo to the plot using the URL
    p_with_logo <- ggdraw(p) +
      draw_image(
        logo_url, # Use URL directly
        x = 0.85, # Horizontal position (right side)
        y = 0.05, # Vertical position (bottom)
        width = 0.15,
        height = 0.15
      )
    
    return(p_with_logo)
  }
  
  # Convert to interactive girafe for each plot
  create_interactive_plot <- function(plot_obj) {
    girafe(
      ggobj = plot_obj,
      width_svg = 8,
      height_svg = 5,
      options = list(
        opts_hover(css = "fill-opacity:0.8;"),
        opts_tooltip(
          opacity = 0.9,
          css = "background-color:#011E41;color:white;padding:8px;border-radius:3px;",
          use_fill = TRUE
        ),
        opts_sizing(rescale = TRUE),
        opts_toolbar(hidden = c("lasso_select", "lasso_deselect"))
      ),
      fonts = list(
        addGFontHtmlDependency(family = "Open Sans"),
        addGFontHtmlDependency(family = "Poppins")
      )
    )
  }
  
  # Render the plot
  output$plot <- renderGirafe({
    data <- filtered_data()
    req(data)
    suppressWarnings(create_interactive_plot(create_plot(data)))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")