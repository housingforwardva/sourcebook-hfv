library(shiny)
library(tidyverse)
library(readxl)
library(stringr)
library(cowplot)
library(mapgl)
library(sf)
library(air)
library(here)
library(bslib)
library(shinyjs)
library(ggiraph)

# =============================================================================
# NATIONAL HOUSING PRESERVATION DATABASE MAP
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

va_subsidies <- read_rds("./data.rds") |> 
  filter(subsidy_status == "Active/Inconclusive")
  
cbsa_list <- sort(unique(va_subsidies$cbsa_title))
  
locality_list <- sort(unique(va_subsidies$name_long))


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
      h4("Federally Assisted Rental Housing", class = "hfv-title")
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
        
        h5("Filters", 
           class = "text-primary", style = "margin-bottom: 16px;"),
        
        
        # Geography selectors
        div(
          style = "margin-bottom: 16px;",
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            selectInput("cbsa", "Metro Area:", choices = cbsa_list, width = "100%", selectize = FALSE)
          ),
          conditionalPanel(
            condition = "input.tabs == 'local'",
            selectInput("locality", "Locality:", choices = locality_list, width = "100%", selectize = FALSE)
          )
        ),
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "National Housing Preservation Database.",
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

# Streamlined server function with reduced redundancy
server <- function(input, output, session) {

  
  # Initialize dropdowns
  observe({
    cbsa_choices <- cbsa_list
    locality_choices <- locality_list
    
    updateSelectInput(session, "cbsa", 
                      choices = cbsa_choices,
                      selected = if("Richmond, VA" %in% cbsa_choices) "Richmond, VA" else cbsa_choices[1])
    
    updateSelectInput(session, "locality", 
                      choices = locality_choices,
                      selected = if("Richmond City" %in% locality_choices) "Richmond City" else locality_choices[1])
  })
  
  # Single reactive for filtered data by geography type and year
  filtered_data <- reactive({
    
    base_data <- va_subsidies 
    
    # Return a list with all three data types
    list(
      state = base_data %>%
        group_by(subsidy_name, subsidy_status) %>%
        summarise(value = sum(assisted_units, na.rm = TRUE), .groups = "drop") |> 
        mutate(tooltip =
        paste0(
          "Subsidy Name: ", subsidy_name, "\n",
          "Assisted units: ", value
        ))|> 
          group_by(subsidy_name) %>%
          mutate(max_value_per_subsidy = max(value)) %>%
          ungroup() ,
      
      cbsa = if (!is.null(input$cbsa)) {
        base_data %>%
          filter(cbsa_title == input$cbsa) %>%
          group_by(cbsa_title, subsidy_name, subsidy_status) %>%
          summarise(value = sum(assisted_units, na.rm = TRUE), .groups = "drop")|> 
        mutate(tooltip =
        paste0(
          "Subsidy Name: ", subsidy_name, "\n",
          "Assisted units: ", value
        ))|> 
          group_by(subsidy_name) %>%
          mutate(max_value_per_subsidy = max(value)) %>%
          ungroup() 
      } else NULL,
      
      locality = if (!is.null(input$locality)) {
        base_data %>%
          filter(name_long == input$locality)  %>%
          group_by(name_long, subsidy_name, subsidy_status) %>%
          summarise(value = sum(assisted_units,na.rm = TRUE), .groups = "drop")|> 
        mutate(tooltip =
        paste0(
          "Subsidy Name: ", subsidy_name, "\n",
          "Assisted units: ", value
        )) |> 
          group_by(subsidy_name) %>%
          mutate(max_value_per_subsidy = max(value)) %>%
          ungroup() 
          
      } else NULL
    )
  })
  
  # Single function to create all plots
  create_subsidy_plot <- function(data, title_text, subtitle_text = NULL) {
    req(nrow(data) > 0)
    
    # Add tooltips
    plot_data <- data 
    
    # Create base plot
    p <- ggplot(plot_data, aes(x = reorder(subsidy_name, max_value_per_subsidy), y = value, fill = subsidy_status)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = subsidy_name),
        position = "stack"
      ) +
      scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
      labs(
        title = title_text,
        caption = " "
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 0.5),
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
      opts_selection(type = "none")
    )
  )
}
  
  # Render plots using the streamlined approach
  output$state_plot <- renderGirafe({
    data <- filtered_data()$state
    req(data)
    plot <- create_subsidy_plot(data, "Federally-Assisted Rental Housing in Virginia")
    create_interactive_plot(plot)
  })
  
  output$cbsa_plot <- renderGirafe({
    data <- filtered_data()$cbsa
    req(data, input$cbsa)
    title <- paste("Federally-Assisted Rental Housing in", input$cbsa, "Metro")
    plot <- create_subsidy_plot(data, title)
    create_interactive_plot(plot)
  })
  
  output$local_plot <- renderGirafe({
    data <- filtered_data()$locality
    req(data, input$locality)
    title <- paste("Federally-Assisted Rental Housing in", input$locality)
    plot <- create_subsidy_plot(data, title)
    create_interactive_plot(plot)
  })
  
  # Mobile optimization
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)