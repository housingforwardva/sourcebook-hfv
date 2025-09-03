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
library(gfonts)

# =============================================================================
# Housing Units by Structure Type Visualization
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

# =============================================================================
# LOAD DATA OUTSIDE SERVER
# ============================================================================= 
# Load the data (only once)
b25127 <- read_rds("./data.rds")

# Define structure order
structure_order <- c("1, detached or attached", "2 to 4", "5 to 19", "20 to 49", 
                     "50 or more", "Mobile home, boat, RV, van, etc.")

# Pre-compute datasets  
state_data <- b25127 %>% 
  group_by(year, tenure, structure) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>% 
  group_by(year) %>% 
  mutate(percent = estimate/sum(estimate)) %>%
  ungroup() %>% 
  mutate(structure = factor(structure, levels = structure_order))

cbsa_data <- b25127 %>% 
  group_by(year, cbsa_title, tenure, structure) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>% 
  group_by(year, cbsa_title) %>% 
  mutate(percent = estimate/sum(estimate)) %>% 
  ungroup() %>% 
  mutate(structure = factor(structure, levels = structure_order))

locality_data <- b25127 %>% 
  group_by(year, name_long, tenure, structure) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>% 
  group_by(year, name_long) %>% 
  mutate(percent = estimate/sum(estimate)) %>% 
  ungroup() %>% 
  mutate(structure = factor(structure, levels = structure_order))

# Get available choices
cbsa_list <- sort(unique(cbsa_data$cbsa_title))
locality_list <- sort(unique(locality_data$name_long))

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
      h4("Housing Units by Structure Type", class = "hfv-title")
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
        
        # Year select
        div(
          style = "margin-bottom: 16px;",
          selectInput("year", "Select Year:", 
                      choices = 2017:2023, 
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
            "U.S. Census Bureau, American Community Survey, Table B25127",
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
    # CBSAs
    updateSelectInput(session, "cbsa", 
                      choices = cbsa_list,
                      selected = if("Richmond, VA" %in% cbsa_list) "Richmond, VA" else cbsa_list[1])
    
    # Localities
    updateSelectInput(session, "locality", 
                      choices = locality_list,
                      selected = if("Richmond City" %in% locality_list) "Richmond City" else locality_list[1])
  })
  
  # Filter data for state
  filtered_state <- reactive({
    req(input$year)
    
    state_data %>%
      filter(year == input$year)
  })
  
  # Filter data for selected CBSA
  filtered_cbsa <- reactive({
    req(input$cbsa, input$year)
    
    cbsa_data %>%
      filter(cbsa_title == input$cbsa,
             year == input$year)
  })
  
  # Filter data for selected locality
  filtered_locality <- reactive({
    req(input$locality, input$year)
    
    locality_data %>%
      filter(name_long == input$locality,
             year == input$year)
  })
  
  # Function to create interactive stacked bar chart
  create_structure_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Determine whether to use percent or count
    if (input$displayType == "percent") {
      plot_data <- data %>%
        mutate(value = percent,
               tooltip = paste0(
                 "Structure Type: ", structure, "\n",
                 "Tenure: ", tenure, "\n",
                 "Percent: ", scales::percent(percent, accuracy = 0.1)
               ))
      y_label <- "Percent of Housing Units"
      y_scale <- scale_y_continuous(labels = scales::percent_format())
    } else {
      plot_data <- data %>%
        mutate(value = estimate,
               tooltip = paste0(
                 "Structure Type: ", structure, "\n",
                 "Tenure: ", tenure, "\n",
                 "Units: ", format(estimate, big.mark = ",")
               ))
      y_label <- "Number of Housing Units"
      y_scale <- scale_y_continuous(labels = scales::number_format(big.mark = ","))
    }
    
    # Create base plot
    p <- ggplot(plot_data, 
                aes(x = structure,
                    y = value,
                    fill = tenure)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = interaction(structure, tenure)),
        position = "stack"
      ) +
      scale_fill_manual(values = c(
        "Homeowner" = hfv_colors$shadow,
        "Renter" = hfv_colors$sky
      )) +
      y_scale +
      labs(
        title = title_text,
        subtitle = paste("Year:", input$year),
        caption = " ", # Add empty caption to leave space for logo
        y = y_label,
        x = NULL,
        fill = "Tenure"
      ) +
      coord_flip() +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 15, 5) # Extra bottom margin for logo
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
    "Virginia Housing Units by Structure Type"
  })
  
  cbsa_title <- reactive({
    paste("Housing Units by Structure Type in", input$cbsa)
  })
  
  locality_title <- reactive({
    paste("Housing Units by Structure Type in", input$locality)
  })
  
  # Render the plots
  output$state_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_structure_plot(filtered_state(), state_title())))
  })
  
  output$cbsa_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_structure_plot(filtered_cbsa(), cbsa_title())))
  })
  
  output$local_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_structure_plot(filtered_locality(), locality_title())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
