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
# Population by Age Visualization
# =============================================================================



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

# Define HFV color palette (matching SCSS variables)
hfv_colors <- list(
  sky = "#40C0C0",           # Primary teal
  grass = "#259591",         # Dark teal 
  lilac = "#8B85CA",         # Purple
  shadow = "#011E41",        # Dark navy
  shadow_light = "#102C54",  # Lighter navy
  berry = "#B1005F",         # Magenta
  desert = "#E0592A"         # Orange
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
      h4("Population by Age", class = "hfv-title")
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
          selectInput("year", "Select Year:", 
                      choices = 2010:2023, 
                      selected = 2023, 
                      width = "100%",
                      selectize = FALSE)
        ),
        
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "U.S. Census Bureau, Population Estimates Program and Decennial Census",
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
# Streamlined server function with reduced redundancy
server <- function(input, output, session) {
  # Load the data (only once)
  pop_age <- reactive({
    readRDS("pop_age.rds")
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
  
  # Define age group order (move to global or make reactive if it might change)
  age_order <- c("Under 10", "10 to 17", "18 to 24", "25 to 29", "30 to 34", 
                 "35 to 44", "45 to 54", "55 to 64", "65 to 74", "75 and over")
  
  
  # Single reactive for filtered data based on current geography
  filtered_data <- reactive({
    req(input$year)
    geo <- current_geo()
    
    base_data <- pop_age() %>%
      filter(year == input$year) %>%
      mutate(agegroup = factor(agegroup, levels = age_order))
    
    if (geo$type == "state") {
      base_data %>%
        group_by(agegroup) %>%
        summarise(value = sum(value), .groups = "drop")
    } else if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      base_data %>%
        filter(cbsa_title == geo$cbsa) %>%
        group_by(agegroup) %>%
        summarise(value = sum(value), .groups = "drop")
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      base_data %>%
        filter(name_long == geo$locality)
    } else {
      NULL
    }
  })
  
  # Single function to create all plots
  create_age_plot <- function(data, title_text, subtitle_text = NULL) {
    req(nrow(data) > 0)
    
    # Add tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Age Group: ", agegroup, "\n",
        "Population: ", format(value, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data, aes(x = agegroup, y = value, fill = agegroup)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = agegroup),
        position = "stack"
      ) +
      scale_fill_manual(values = c(
        "Under 10" = hfv_colors$sky,
        "10 to 17" = hfv_colors$grass,
        "18 to 24" = hfv_colors$lilac,
        "25 to 29" = hfv_colors$shadow_light,
        "30 to 34" = hfv_colors$shadow,
        "35 to 44" = hfv_colors$berry,
        "45 to 54" = "#D3447E",
        "55 to 64" = hfv_colors$desert,
        "65 to 74" = "#F08A65",
        "75 and over" = "#FAC172"
      )) +
      scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
      labs(
        title = title_text,
        subtitle = subtitle_text %||% paste("Year:", input$year),
        caption = " ",
        y = "Population",
        x = "Age Group"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 30, 5)
      )
    
    # Add logo (single implementation)
    add_hfv_logo(p)
  }
  
  # Helper function for logo (extracted to reduce duplication)
  add_hfv_logo <- function(plot) {
    logo_url <- "https://housingforwardva.org/wp-content/uploads/2024/08/HousingForward-VA-Logo-Files-Horizontal-Gradient-RGB.png"
    
    ggdraw(plot) +
      draw_image(
        logo_url,
        x = 0.85, y = 0.05,
        width = 0.15, height = 0.15
      )
  }
  
  # Single function to create interactive plots
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
        opts_sizing(rescale = TRUE)
      )
    )
  }
  
  # Render single plot based on current geography
  output$plot <- renderGirafe({
    data <- filtered_data()
    req(data)
    geo <- current_geo()
    
    title <- if (geo$type == "state") {
      "Virginia Population by Age Group"
    } else if (geo$type == "cbsa") {
      paste("Population by Age Group in", geo$cbsa, "Metro")
    } else {
      paste("Population by Age Group in", geo$locality)
    }
    
    plot <- create_age_plot(data, title)
    create_interactive_plot(plot)
  })
  
  # Mobile optimization
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")