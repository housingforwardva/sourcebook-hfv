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
      h4("Total Population Change", class = "hfv-title")
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
        
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "U.S. Census Bureau, Population Estimates Program and Decennial Census.",
            style = "margin-bottom: 0;"
          )
        )
      ),
        
      # Main Panel with single plot
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
  # Load the data
  total_pop <- reactive({
    read_rds("./total_pop.rds")
  })
  
  # Shared function for calculating population changes
  calculate_pop_changes <- function(data) {
    data %>% 
      mutate(diff = value - lag(value),
             diff = replace_na(diff, 0)) %>% 
      mutate(run_diff = cumsum(diff)) %>% 
      filter(run_diff != 0) %>% 
      mutate(pct = run_diff/first(value))
  }
  
  # Pre-compute datasets
  state_data <- reactive({
    total_pop() %>% 
      group_by(year, counttype) %>% 
      summarise(value = sum(value), .groups = "drop") %>% 
      ungroup() %>% 
      calculate_pop_changes()
  })
  
  cbsa_data <- reactive({
    total_pop() %>% 
      group_by(year, cbsa_title, counttype) %>% 
      summarise(value = sum(value), .groups = "drop")
  })
  
  # Get current geography from URL
  current_geo <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    list(
      type = query$geo %||% "state",
      cbsa = query$cbsa,
      locality = query$locality
    )
  })
  
  # Single reactive for filtered data based on current geography
  filtered_data <- reactive({
    geo <- current_geo()
    
    if (geo$type == "state") {
      state_data()
    } else if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      cbsa_data() %>%
        filter(cbsa_title == geo$cbsa) %>% 
        group_by(year, counttype) %>% 
        summarise(value = sum(value), .groups = "drop") %>% 
        ungroup() %>% 
        calculate_pop_changes()
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      total_pop() %>%
        filter(name_long == geo$locality) %>% 
        calculate_pop_changes()
    } else {
      NULL
    }
  }) 
  
  # Create title text
  title_text <- reactive({
    geo <- current_geo()
    if (geo$type == "state") {
      "Virginia Population Change Since 2010"
    } else if (geo$type == "cbsa") {
      paste("Population Change Since 2010 in", geo$cbsa)
    } else {
      paste("Population Change Since 2010 in", geo$locality)
    }
  })
  
  # Function to create population change plots
  create_pop_change_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Year: ", year, "\n",
        "Change: ", scales::percent(pct, accuracy = 0.1)
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = year,
                    y = pct,
                    group = 1)) +
      geom_area(fill = "#011E41", alpha = 0.3) +
      geom_line(color = "#011E41", linewidth = 1) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = year),
        size = 3,
        color = "#011E41"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = title_text,
        caption = " ", # Add empty caption to leave space for logo
        y = "Population Change (%)",
        x = "Year"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.minor = element_blank(),
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
  
  # Render single plot based on current geography
  output$plot <- renderGirafe({
    data <- filtered_data()
    req(data)
    suppressWarnings(create_interactive_plot(create_pop_change_plot(data, title_text())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}



# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")