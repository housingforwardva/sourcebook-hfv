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
library(gdtools)

# =============================================================================
# MEDIAN HOUSEHOLD INCOME BY RACE VISUALIZATION
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

# =============================================================================
# LOAD DATA OUTSIDE SERVER
# =============================================================================

 data <- read_rds("./data.rds")

  state_inc_data <- data  |> 
    filter(geography == "state")
  
  cbsa_inc_data <- data  |> 
    filter(geography == "cbsa")
  
  locality_inc_data <- data  |> 
    filter(geography == "county")
  
  # Create color vector for races
  race_colors <- c(
    "White, non-Hispanic" = "#40C0C0",
    "Black" = "#259591",
    "Asian" = "#011E41",
    "Hispanic or Latino" = "#E0592A",
    "Multiracial" = "#B1005F",
    "American Indian and Alaska Native" = "#8B85CA",
    "Native Hawaiian and Other Pacific Islander" = "#FFC658",
    "Some Other Race" = "#FF7276"
  )
  
  # Get available options
  state_list <- sort(unique(state_inc_data$NAME))
  
  cbsa_list <- sort(unique(cbsa_inc_data$NAME))
  
  locality_list <- sort(unique(locality_inc_data$NAME))
  
  year_list <- sort(unique(state_inc_data$year), decreasing = TRUE)


# =============================================================================
# USER INTERFACE
# =============================================================================


ui <- page_fillable(
  theme = hfv_theme,
  includeCSS("www/styles/hfv-theme.css"),  # Add custom theme css
  useShinyjs(), # Initialize shinyjs

  # Main container using HFV classes
  div(
    class = "hfv-container",
    
    # Header using HFV styling
    div(
      class = "hfv-header",
      h4("Median Household Income by Race/Ethnicity", class = "hfv-title")
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
        
        # Show inflation-adjusted option
        div(
          style = "margin-bottom: 16px;",
          checkboxInput("adjusted", "Show Inflation-Adjusted", FALSE)
        ),
        
        # Geography selectors
        div(
          style = "margin-bottom: 16px;",
          conditionalPanel(
            condition = "input.tabs == 'state'",
            selectInput("state_select", "Select State:", choices = NULL, width = "100%", selectize = FALSE)
          ),
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            selectInput("cbsa_select", "Metro Area:", choices = NULL, width = "100%", selectize = FALSE)
          ),
          conditionalPanel(
            condition = "input.tabs == 'local'",
            selectInput("locality_select", "Locality:", choices = NULL, width = "100%", selectize = FALSE)
          )
        ),
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "U.S. Census Bureau, American Community Survey 5-Year Estimates, Table B19013",
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
# =============================================================================
# SERVER FUNCTION
# =============================================================================

server <- function(input, output, session) {

  
  # Initialize dropdowns
  observe({
    # Years
    updateSelectInput(session, "year", 
                      choices = year_list,
                      selected = max(year_list))
    
    # States
    updateSelectInput(session, "state_select", 
                      choices = state_list,
                      selected = if("Virginia" %in% state_list) "Virginia" else state_list[1])
    
    # CBSAs
    updateSelectInput(session, "cbsa_select", 
                      choices = cbsa_list,
                      selected = if("Richmond, VA Metro Area" %in% cbsa_list) "Richmond, VA Metro Area" else cbsa_list[1])
    
    # Localities
    updateSelectInput(session, "locality_select", 
                      choices = locality_list,
                      selected = if("Richmond city" %in% locality_list) "Richmond city" else locality_list[1])
  })
  
  # Get filtered data based on selected tab and inputs
  filtered_state <- reactive({
    req(input$state_select, input$year)
    
    state_inc_data %>%
      filter(
        NAME == input$state_select,
        year == input$year
      ) |> 
      drop_na()
  })
  
  filtered_cbsa <- reactive({
    req(input$cbsa_select, input$year)
    
    cbsa_inc_data %>%
      filter(
        NAME == input$cbsa_select,
        year == input$year
      )|> 
      drop_na()
  })
  
  filtered_locality <- reactive({
    req(input$locality_select, input$year)
    
    locality_inc_data %>%
      filter(
        NAME == input$locality_select,
        year == input$year
      )|> 
      drop_na()
  })
  
  # Plot titles
  state_title <- reactive({
    paste("Median Household Income by Race/Ethnicity in", input$state_select, "(", input$year, ")")
  })
  
  cbsa_title <- reactive({
    paste("Median Household Income by Race/Ethnicity in", input$cbsa_select, "(", input$year, ")")
  })
  
  locality_title <- reactive({
    paste("Median Household Income by Race/Ethnicity in", input$locality_select, "(", input$year, ")")
  })
  
  # Y-axis label based on inflation adjustment
  y_label <- reactive({
    if(input$adjusted) {
      "Median Household Income (Inflation-Adjusted)"
    } else {
      "Median Household Income"
    }
  })
  
  # Function to create interactive bar plots
  create_bar_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Select which value to plot based on checkbox
    value_col <- if(input$adjusted) "adjusted" else "estimate"
    
    # Filter out NA values
    plot_data <- data %>% 
      # Use the value column to order the races
      mutate(race = factor(race, levels = race[order(get(value_col))]))
    
    # Add tooltips
    plot_data <- plot_data %>%
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", race, "\n",
        "Income: ", scales::dollar(get(value_col))
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(
                  x = race,
                  y = .data[[value_col]],
                  fill = race)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = race)
      ) +
      # Add the value labels at the end of each bar
      geom_text(aes(label = scales::dollar(.data[[value_col]], accuracy = 1)),
                hjust = -0.2,
                color = "#333333",
                size = 3) +
      # Set the fill colors
      scale_fill_manual(values = race_colors) +
      # Extend the plot area to make room for labels
      coord_flip(clip = "off") +
      # Format y-axis with dollar signs
      scale_y_continuous(
        labels = scales::dollar_format(),
        limits = function(x) c(0, max(x) * 1.2)  # Add 20% headroom for labels
      ) +
      labs(
        title = title_text,
        caption = " ",  # Empty caption for logo space
        x = NULL,
        y = y_label()
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 20, 30, 5)  # Extra right margin for labels, bottom for logo
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
  
  # Render plots
  output$state_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_bar_plot(filtered_state(), state_title())))
  })
  
  output$cbsa_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_bar_plot(filtered_cbsa(), cbsa_title())))
  })
  
  output$local_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_bar_plot(filtered_locality(), locality_title())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)