# Household Composition Visualization ------------------------------------------
# This app visualizes household composition data across Virginia
# with tabs for statewide, CBSA, and locality views

library(shiny)
library(tidyverse)
library(ggtext)
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

# Define HFV color palette
hfv_colors <- list(
  sky = "#40C0C0",
  grass = "#259591",
  lilac = "#8B85CA",
  shadow = "#011E41",
  shadow_light = "#102C54", # Lighter shade of shadow color
  berry = "#B1005F",
  desert = "#E0592A"
)



# UI
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
      h4("Household Composition", class = "hfv-title")
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
        
        # Year selector (common to all tabs)
        div(
          style = "margin-bottom: 15px;",
          selectInput(
            "selected_year",
            "Select Year:",
            choices = NULL,
            width = "100%",
            selectize = FALSE
          )
        ),
        
        
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "U.S. Census Bureau, 5-Year American Community Survey 5-year estimates, Table B11021.",
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

# Server
server <- function(input, output, session) {
  
  # Load the data with error handling
  if (!file.exists("b11012_data.rds")) {
    stop("Data file 'b11012_data.rds' not found. Please ensure it exists in the app directory.")
  }
  
  hh_type <- read_rds("b11012_data.rds")
  
  # Get current geography from URL
  current_geo <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    list(
      type = query$geo %||% "state",
      cbsa = query$cbsa,
      locality = query$locality
    )
  })
  
  # Create year list
  year_list <- sort(unique(hh_type$year), decreasing = TRUE)
  
  
  # Initialize dropdowns
  observe({
    updateSelectInput(session, "selected_year", 
                      choices = year_list,
                      selected = year_list[1])
  })
  
  # Create reactive expression for selected year
  selected_year <- reactive({
    req(input$selected_year)  # Ensure input exists
    input$selected_year
  })
  
  # Single reactive for filtered data based on current geography
  filtered_data <- reactive({
    req(selected_year())
    geo <- current_geo()
    year_selected <- selected_year()
    
    if (geo$type == "state") {
      result <- hh_type %>% 
        filter(year == year_selected) %>%
        group_by(type, sub) %>% 
        summarise(estimate = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
        group_by(type) %>%
        mutate(
          total_by_type = sum(estimate),
          percent = estimate / total_by_type,
          rank_within_type = rank(percent, ties.method = "first")
        ) %>%
        ungroup()
    } else if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      result <- hh_type %>% 
        filter(year == year_selected, cbsa_title == geo$cbsa) %>%
        group_by(type, sub) %>% 
        summarise(estimate = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
        group_by(type) %>%
        mutate(
          total_by_type = sum(estimate),
          percent = estimate / total_by_type,
          rank_within_type = rank(percent, ties.method = "first")
        ) %>%
        ungroup()
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      result <- hh_type %>% 
        filter(year == year_selected, name_long == geo$locality) %>%
        group_by(type, sub) %>% 
        summarise(estimate = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
        group_by(type) %>%
        mutate(
          total_by_type = sum(estimate),
          percent = estimate / total_by_type,
          rank_within_type = rank(percent, ties.method = "first")
        ) %>%
        ungroup()
    } else {
      return(NULL)
    }
    
    return(result)
  })
  
  # Generate title text
  title_text <- "<b><span style='color:#011E41'>Householder with no partner</span></b> and 
<b><span style='color:#40C0C0'>Married or cohabitating couple</span></b>"
  
  # Function to create interactive plots (consolidated)
  create_interactive_plot <- function(data, subtitle_text) {
    p <- ggplot(data,
                aes(x = reorder(sub, rank_within_type),
                    y = percent,
                    fill = type)) + 
      geom_col_interactive(
        aes(tooltip = paste0(sub, ": ", scales::percent(percent, accuracy = 0.1))),
        hover_nearest = TRUE
      ) +
      # Match text color to bar fill color
      geom_text_interactive(
        aes(label = scales::percent(percent, accuracy = 1),
            color = type,
            tooltip = paste0("Count: ", format(estimate, big.mark = ","))),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3.5
      ) +
      # Make sure text colors match fill colors
      scale_color_manual(values = c(hfv_colors$shadow, hfv_colors$sky)) +
      scale_fill_manual(values = c(hfv_colors$shadow, hfv_colors$sky)) +
      labs(title = title_text,
           subtitle = subtitle_text,
           caption = " ", # Empty caption to leave space for logo
           x = NULL,
           y = "Percent of Households") +
      scale_y_continuous(labels = scales::percent_format()) +
      # Hide the color legend since it's redundant with the fill legend
      guides(color = "none") +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title = element_markdown(),
        plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 30, 5) # Extra bottom margin for logo
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      facet_grid(cols = vars(type), scales = "free_x", space = "free") 
    
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
    
    girafe(
      ggobj = p_with_logo,
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
    geo <- current_geo()
    year_selected <- selected_year()
    
    subtitle <- if (geo$type == "state") {
      paste("Virginia:", year_selected)
    } else if (geo$type == "cbsa") {
      paste(geo$cbsa, ":", year_selected)
    } else {
      paste(geo$locality, ":", year_selected)
    }
    
    create_interactive_plot(data, subtitle)
  })
  
  
  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")