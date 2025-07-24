library(shiny)
library(tidyverse)
library(ggiraph)     # For interactive ggplots
library(systemfonts) # For font_google
library(here)        # For here() function in file paths
library(grid)        # For grobs
library(png)         # For reading PNG files
library(bslib)       # For modern UI components
library(cowplot)     # For adding logo to plots
library(scales)      # For formatting scales
library(lubridate)
library(zoo)

# Define HFV color palette
hfv_colors <- list(
  sky = "#40C0C0",
  grass = "#259591",
  lilac = "#8B85CA", 
  shadow = "#011E41",
  shadow_light = "#102C54",  # Lighter shade of shadow color
  berry = "#B1005F",
  desert = "#E0592A"
)

# Create a Bootstrap theme
hfv_theme <- bs_theme(
  version = 5,                        # Use Bootstrap 5
  bg = "#ffffff",                     # Background color
  fg = "#333333",                     # Text color
  primary = hfv_colors$sky,           # Primary color
  secondary = hfv_colors$shadow,      # Secondary color
  success = hfv_colors$grass,         # Success color
  info = hfv_colors$lilac,            # Info color
  warning = hfv_colors$desert,        # Warning color
  danger = hfv_colors$berry,          # Danger color
  base_font = font_google("Open Sans"),
  heading_font = font_google("Poppins"),
  font_scale = 0.8                    # Compact the text more for small window
)

# Load data outside of server
hpi <- read_rds(here("data", "rds", "hpi.rds")) |> 
  mutate(date = as.Date(as.yearqtr(date, format = "%Y Q%q"))) |> 
  select(geography, name, date, hpi) |> 
  filter(!is.na(hpi))

# Create lists for filters
cbsa_list <- sort(unique(hpi$name[hpi$geography == "CBSA"]))

# Define UI
ui <- page_fluid(
  theme = hfv_theme,
  
  # Add CSS with iframe optimization
  tags$head(
    tags$style(HTML(
      "
      /* Base styles */
      body, html {
        margin: 0;
        padding: 0;
        height: auto;
        overflow-x: hidden;
      }
      
      /* Iframe optimization for 800x500 dimensions */
      @media (max-height: 600px) {
        .hfv-container {
          padding: 10px !important;
          margin: 0 auto !important;
          max-height: 500px !important;
          overflow: hidden !important;
        }
        
        body, html {
          overflow: hidden !important;
        }
      }
      "
    ))
  ),
  
  # Fixed dimensions container
  tags$div(
    class = "hfv-container",
    style = "width: 800px; height: 500px; margin: 0 auto; overflow: hidden; padding: 10px;",
    
    # Header with logo and title
    div(
      style = "display: flex; align-items: center; margin-bottom: 10px; border-bottom: 2px solid #40C0C0; padding-bottom: 5px;",
      img(src = "https://housingforwardva.org/wp-content/uploads/2025/05/HousingForward-VA-Logo-Files-Icon-One-Color-RGB.png", 
          height = "30px", style = "margin-right: 10px;"),
      h4("Housing Price Index Analysis", style = "margin: 0; color: #011E41;")
    ),
    
    # Main content area with reduced margins
    div(
      style = "height: 435px; overflow: hidden;",
      
      # Use layout_columns for a compact layout
      layout_columns(
        col_widths = c(3, 9),
        gap = "10px",
        
        # Sidebar Panel (more compact) - now with the lighter background color
        card(
          height = "435px",
          padding = "8px",  # Reduced padding for compactness
          margin = 0,
          full_screen = FALSE,
          style = "background-color: #E8EDF2;",  # Light shade derived from shadow color
          
          # Geography selectors with minimal height
          div(
            style = "margin-bottom: 0;",
            conditionalPanel(
              condition = "input.tabs == 'cbsa'",
              selectInput("cbsa_select", "Metro Area:", 
                          choices = cbsa_list,
                          selected = if("Richmond, VA" %in% cbsa_list) "Richmond, VA" else cbsa_list[1],
                          width = "100%", 
                          selectize = FALSE)
            )
          ),
          
          # Horizontal line
          hr(style = "margin: 3px 0;"),
          
          # Tooltip info
          div(
            style = "margin-top: 5px; margin-bottom: 5px; font-size: 10px;",
            p("Hover over points to see details", style = "margin-bottom: 5px;"),
            verbatimTextOutput("hover_info", placeholder = TRUE)
          ),
          
          # Source information
          div(
            style = "font-size: 10px; color: #666; margin-top: 2px;",
            p(
              "Source: Federal Housing Finance Agency (FHFA) Housing Price Index.",
              style = "margin-bottom: 0;"
            )
          )
        ),
        
        # Main Panel (tabs)
        card(
          height = "435px",
          padding = 0,
          margin = 0,
          full_screen = FALSE,
          
          navset_tab(
            id = "tabs",
            nav_panel(
              title = "State", 
              value = "state",
              padding = 5,
              girafeOutput("state_plot", height = "390px")
            ),
            nav_panel(
              title = "Metro Area", 
              value = "cbsa",
              padding = 5,
              girafeOutput("cbsa_plot", height = "390px")
            ),
            nav_panel(
              title = "Nonmetro", 
              value = "nonmetro",
              padding = 5,
              girafeOutput("nonmetro_plot", height = "390px")
            )
          )
        )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Create filtered datasets
  state_data <- reactive({
    hpi |> 
      filter(geography == "State")
  })
  
  filtered_cbsa <- reactive({
    req(input$cbsa_select)
    hpi |> 
      filter(geography == "CBSA",
             name == input$cbsa_select)
  })
  
  nonmetro_data <- reactive({
    hpi |> 
      filter(geography == "Nonmetro")
  })
  
  # Plot titles
  state_title <- reactive({
    "Housing Price Index in Virginia"
  })
  
  cbsa_title <- reactive({
    paste("Housing Price Index in", input$cbsa_select)
  })
  
  nonmetro_title <- reactive({
    "Housing Price Index in Nonmetropolitan Virginia"
  })
  
  # Function to create interactive line plots
  create_line_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Get latest value for label
    latest_data <- data |> 
      filter(date == max(date, na.rm = TRUE))
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Date: ", year(date), " Q", quarter(date), "\n",
        "HPI: ", round(hpi, 2)
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(
                  x = date,
                  y = hpi,
                  group = 1
                )) +
      geom_line_interactive(
        aes(tooltip = tooltip),
        color = hfv_colors$sky,
        linewidth = 1.2
      ) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = paste(date, hpi)),
        color = hfv_colors$sky,
        size = 2
      ) +
      # Add label for latest value
      geom_text(data = latest_data, 
                aes(label = round(hpi, 1)),
                hjust = -0.3, vjust = 0.5, 
                color = hfv_colors$shadow) +
      labs(
        title = title_text,
        y = "Housing Price Index",
        x = "Year",
        caption = " " # Add empty caption to leave space for logo
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 15, 15, 5) # Extra right margin for labels and bottom for logo
      ) +
      scale_x_date(date_labels = "%Y", date_breaks = "5 years")
    
    # Add logo to the plot
    logo_path <- "www/hfv_logo.png"
    p_with_logo <- cowplot::ggdraw(p) +
      cowplot::draw_image(logo_path, 
                          x = 0.8, y = -0.05, 
                          width = 0.15, height = 0.15)
    
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
        opts_toolbar(hidden = c("lasso_deselect", "lasso_select")),
        opts_sizing(rescale = TRUE)
      )
    )
  }
  
  # Render the plots
  output$state_plot <- renderGirafe({
    create_interactive_plot(create_line_plot(state_data(), state_title()))
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_line_plot(filtered_cbsa(), cbsa_title()))
  })
  
  output$nonmetro_plot <- renderGirafe({
    create_interactive_plot(create_line_plot(nonmetro_data(), nonmetro_title()))
  })
  
  # Handle hover info for all plots
  get_hover_data <- reactive({
    if (input$tabs == "state") {
      data <- state_data()
      geo_name <- "Virginia"
    } else if (input$tabs == "cbsa") {
      data <- filtered_cbsa()
      geo_name <- input$cbsa_select
    } else { # nonmetro
      data <- nonmetro_data()
      geo_name <- "Nonmetropolitan Virginia"
    }
    
    list(
      data = data,
      geo_name = geo_name
    )
  })
  
  # Display hover information
  output$hover_info <- renderText({
    hover_data <- get_hover_data()
    data <- hover_data$data
    
    # If there's no hover data, show a placeholder message
    if (is.null(data) || nrow(data) == 0) {
      return("Hover over a point for details")
    }
    
    geo_name <- hover_data$geo_name
    
    # Format some example hover data for display
    if (nrow(data) > 0) {
      # Take the latest data point as an example
      example <- data |> filter(date == max(date, na.rm = TRUE))
      
      paste0(
        geo_name, "\n",
        "Latest HPI: ", round(example$hpi[1], 1), "\n",
        "Date: ", year(example$date[1]), " Q", quarter(example$date[1])
      )
    } else {
      "Hover over a point for details"
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

