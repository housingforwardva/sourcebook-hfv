# Overcrowding Visualization ---------------------------------------------------

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
ui <- page_fillable(
  theme = hfv_theme,
  useShinyjs(), # Initialize shinyjs

  # Main container using HFV classes
  div(
    class = "hfv-container",
    
    # Header using HFV styling
    div(
      class = "hfv-header",
      h4("Household Overcrowding by Tenure", class = "hfv-title")
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
        
        # Display options
        div(
          style = "margin-bottom: 16px;",
          radioButtons("displayType", "Display:", 
                       choices = c("Percent" = "percent", "Count" = "count"),
                       selected = "percent",
                       inline = TRUE)
        ),
        
        # Geography selectors
        div(
          style = "margin-bottom: 16px;",
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            selectInput("cbsa", "Metro Area:", choices = NULL, width = "100%", selectize = FALSE)
          ),
          conditionalPanel(
            condition = "input.tabs == 'local'",
            selectInput("locality", "Locality:", choices = NULL, width = "100%", selectize = FALSE)
          )
        ),
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "U.S. Census Bureau, 5-Year American Community Survey, Table B25014.",
            style = "margin-bottom: 8px;"
          ),
          p(
            strong("Note:"), "Very overcrowded = more than 1.5 persons per room. Overcrowded = 1.01 to 1.5 persons per room.",
            style = "margin-bottom: 0;"
          )
        )
      ),
        
      # Main Panel with tabs
      div(
        navset_tab(
          id = "tabs",
          
          nav_panel(
            title = "State",
            value = "state",
            div(
              class = "hfv-chart-container",
              style = "height: 450px; margin-top: 16px;",
              girafeOutput("state_plot", height = "100%")
            )
          ),
          
          nav_panel(
            title = "Metro Area",
            value = "cbsa", 
            div(
              class = "hfv-chart-container",
              style = "height: 450px; margin-top: 16px;",
              girafeOutput("cbsa_plot", height = "100%")
            )
          ),
          
          nav_panel(
            title = "Locality",
            value = "local",
            div(
              class = "hfv-chart-container",
              style = "height: 450px; margin-top: 16px;",
              girafeOutput("local_plot", height = "100%")
            )
          )
        )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  # Load the data
  b25014 <- reactive({
    # Load data and convert "Owner" tenure to "Homeowner"
    readRDS("b25014_data.rds") %>%
      mutate(tenure = case_when(
        tenure == "Owner" ~ "Homeowner",
        TRUE ~ tenure
      ))
  })
  
  # Pre-compute datasets
  state_data <- reactive({
    b25014() %>% 
      group_by(year, tenure, overcrowded) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, tenure) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      ungroup() %>%
      filter(overcrowded != "Not overcrowded")
  })
  
  cbsa_data <- reactive({
    b25014() %>% 
      group_by(year, cbsa_title, tenure, overcrowded) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, cbsa_title, tenure) %>% 
      mutate(percent = estimate/sum(estimate)) %>% 
      ungroup() %>%
      filter(overcrowded != "Not overcrowded")
  })
  
  locality_data <- reactive({
    b25014() %>% 
      group_by(year, name_long, tenure, overcrowded) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, name_long, tenure) %>% 
      mutate(percent = estimate/sum(estimate)) %>% 
      ungroup() %>%
      filter(overcrowded != "Not overcrowded")
  })
  
  # Get available CBSAs and localities
  cbsa_list <- reactive({
    sort(unique(cbsa_data()$cbsa_title))
  })
  
  locality_list <- reactive({
    sort(unique(locality_data()$name_long))
  })
  
  # Initialize dropdowns
  observe({
    # CBSAs
    updateSelectInput(session, "cbsa", 
                      choices = cbsa_list(),
                      selected = if("Richmond, VA" %in% cbsa_list()) "Richmond, VA" else cbsa_list()[1])
    
    # Localities
    updateSelectInput(session, "locality", 
                      choices = locality_list(),
                      selected = if("Richmond City" %in% locality_list()) "Richmond City" else locality_list()[1])
  })
  
  # Filter data for state
  filtered_state <- reactive({
    req(input$year)
    
    state_data() %>%
      filter(year == input$year)
  })
  
  # Filter data for selected CBSA
  filtered_cbsa <- reactive({
    req(input$cbsa, input$year)
    
    cbsa_data() %>%
      filter(cbsa_title == input$cbsa,
             year == input$year)
  })
  
  # Filter data for selected locality
  filtered_locality <- reactive({
    req(input$locality, input$year)
    
    locality_data() %>%
      filter(name_long == input$locality,
             year == input$year)
  })
  
  # Function to create interactive faceted bar chart
  create_overcrowding_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Check if we have data for both tenure types
    tenure_count <- length(unique(data$tenure))
    
    # Determine whether to use percent or count
    if (input$displayType == "percent") {
      plot_data <- data %>%
        mutate(value = percent,
               tooltip = paste0(
                 "Tenure: ", tenure, "\n",
                 "Category: ", overcrowded, "\n",
                 "Percent: ", scales::percent(percent, accuracy = 0.1)
               ))
      y_label <- "Percent of Households"
      y_scale <- scale_y_continuous(labels = scales::percent_format(), limits = c(0, .05))
    } else {
      plot_data <- data %>%
        mutate(value = estimate,
               tooltip = paste0(
                 "Tenure: ", tenure, "\n",
                 "Category: ", overcrowded, "\n",
                 "Households: ", format(estimate, big.mark = ",")
               ))
      y_label <- "Number of Households"
      y_scale <- scale_y_continuous(labels = scales::number_format(big.mark = ","))
    }
    
    # Color by overcrowding category rather than tenure
    overcrowding_colors <- c(
      "Overcrowded" = "#E0592A",
      "Very overcrowded" = "#B1005F"
    )
    
    # Create base plot with facets by tenure
    p <- ggplot(plot_data, 
                aes(x = overcrowded,
                    y = value,
                    fill = overcrowded)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = interaction(tenure, overcrowded)),
        position = "stack",
        width = 0.7
      ) +
      facet_wrap(~ tenure, nrow = 1, scales = "free_x") +
      scale_fill_manual(values = overcrowding_colors) +
      y_scale +
      labs(
        title = title_text,
        subtitle = paste("Year:", input$year),
        caption = " ", # Add empty caption to leave space for logo
        y = y_label,
        x = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        strip.background = element_rect(fill = "#102C54"),
        strip.text = element_text(color = "white", face = "bold"),
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
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
  
  # Set plot titles
  state_title <- reactive({
    "Virginia Household Overcrowding by Tenure"
  })
  
  cbsa_title <- reactive({
    paste("Household Overcrowding by Tenure in", input$cbsa)
  })
  
  locality_title <- reactive({
    paste("Household Overcrowding by Tenure in", input$locality)
  })
  
  # Render the plots
  output$state_plot <- renderGirafe({
    create_interactive_plot(create_overcrowding_plot(filtered_state(), state_title()))
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_overcrowding_plot(filtered_cbsa(), cbsa_title()))
  })
  
  output$local_plot <- renderGirafe({
    create_interactive_plot(create_overcrowding_plot(filtered_locality(), locality_title()))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
