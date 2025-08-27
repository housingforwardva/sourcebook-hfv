library(shiny)
library(tidyverse)
library(bslib)       # For modern UI components
library(scales)      # For number_format
library(shinyjs)     # For dynamic UI updates

# =============================================================================
# HOUSING DATA VALUE BOXES APP
# =============================================================================

# Define HFV color palette
hfv_colors <- list(
  sky = "#40C0C0",
  grass = "#259591",
  lilac = "#8B85CA", 
  shadow = "#011E41",
  shadow_light = "#102C54",
  berry = "#B1005F",
  desert = "#E0592A"
)

# Create HFV bslib theme
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
  font_scale = 0.9
)

# =============================================================================
# LOAD DATA OUTSIDE SERVER
# =============================================================================

# Load data (assuming same data structure as the shinydashboard version)
var_data <- read_rds("data.rds")

# Get available options
state_list <- unique(sort(var_data$name[var_data$geography == "State"]))
msa_list <- unique(sort(var_data$name[var_data$geography == "MSA"]))
locality_list <- unique(sort(var_data$name[var_data$geography == "Locality"]))

# Define latest quarter
latest_quarter <- max(var_data$quarter)

# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- page_fillable(
  theme = hfv_theme,
  includeCSS("www/styles/hfv-theme.css"),  # Include external HFV theme CSS
  useShinyjs(),
  
  # Main container using HFV classes
  div(
    class = "hfv-container",
    
    # Header using HFV styling
    div(
      class = "hfv-header",
      h3("Housing Market Dashboard", class = "hfv-title")
    ),
    
    # Filter section using HFV sidebar class
    div(
      class = "hfv-sidebar",
      h5("Select Geography", class = "hfv-sidebar__title"),
      
      div(
        class = "hfv-sidebar__section",
        fluidRow(
          column(4,
            div(
              class = "hfv-form-group",
              tags$label("Geography Type:", class = "hfv-form-label"),
              selectInput("geo_type", NULL,
                choices = c("State", "MSA", "Locality"),
                selected = "State",
                width = "100%")
            )
          ),
          column(8,
            div(
              class = "hfv-form-group",
              tags$label("Location:", class = "hfv-form-label"),
              selectInput("geo_name", NULL,
                choices = NULL,
                width = "100%")
            )
          )
        )
      ),
      
      # Data source using HFV sidebar source class
      div(
        class = "hfv-sidebar__source",
        p(
          strong("Data Source:"), br(),
          "Virginia Association of REALTORS"
        )
      )
    ),
    
    # Value boxes section using HFV value box classes
    fluidRow(
      column(3,
        div(
          class = "hfv-value-box hfv-value-box--danger",
          div(
            class = "hfv-value-box__inner",
            div(
              class = "hfv-value-box__value",
              textOutput("latest_quarter", inline = TRUE)
            ),
            div(
              class = "hfv-value-box__label",
              "Latest Quarter"
            )
          ),
          div(
            class = "hfv-value-box__icon",
            icon("calendar")
          )
        )
      ),
      
      column(3,
        div(
          class = "hfv-value-box hfv-value-box--info",
          div(
            class = "hfv-value-box__inner",
            div(
              class = "hfv-value-box__value",
              textOutput("units_sold", inline = TRUE)
            ),
            div(
              class = "hfv-value-box__label",
              "Units Sold"
            )
          ),
          div(
            class = "hfv-value-box__icon",
            icon("home")
          )
        )
      ),
      
      column(3,
        div(
          class = "hfv-value-box hfv-value-box--success",
          div(
            class = "hfv-value-box__inner",
            div(
              class = "hfv-value-box__value",
              textOutput("median_price", inline = TRUE)
            ),
            div(
              class = "hfv-value-box__label",
              "Median Price"
            )
          ),
          div(
            class = "hfv-value-box__icon",
            icon("dollar-sign")
          )
        )
      ),
      
      column(3,
        div(
          class = "hfv-value-box hfv-value-box--secondary",
          div(
            class = "hfv-value-box__inner",
            div(
              class = "hfv-value-box__value",
              textOutput("median_dom", inline = TRUE)
            ),
            div(
              class = "hfv-value-box__label",
              "Median Days on Market"
            )
          ),
          div(
            class = "hfv-value-box__icon",
            icon("clock")
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
  
  # Observer that updates the name dropdown based on geography selection
  observe({
    selected_geo <- input$geo_type
    
    # Get filtered names based on geography type
    filtered_names <- var_data %>%
      filter(geography == selected_geo) %>%
      arrange(name) %>%
      pull(name) %>%
      unique()
    
    # Update the name dropdown
    updateSelectInput(
      session = session,
      inputId = "geo_name",
      choices = filtered_names,
      selected = if(length(filtered_names) > 0) filtered_names[1] else NULL
    )
  })
  
  # Create reactive filtered data
  dashboard_data <- reactive({
    req(input$geo_type, input$geo_name)
    
    filtered <- var_data %>% 
      filter(geography == input$geo_type,
             name == input$geo_name)
    
    validate(
      need(nrow(filtered) > 0, "Loading data...")
    )
    
    return(filtered)
  })
  
  # Create reactive values for the latest quarter data
  latest_data <- reactive({
    req(dashboard_data())
    
    latest <- dashboard_data() %>% 
      filter(quarter == latest_quarter)
    
    validate(
      need(nrow(latest) > 0, "No data for the latest quarter")
    )
    
    return(latest)
  })
  
  # Value box outputs
  output$latest_quarter <- renderText({
    as.character(latest_quarter)
  })
  
  output$units_sold <- renderText({
    tryCatch({
      req(latest_data())
      format(latest_data()$units[1], big.mark = ",", trim = TRUE)
    },
    error = function(e) {
      return("--")
    })
  })
  
  output$median_price <- renderText({
    tryCatch({
      req(latest_data())
      dollar_format()(latest_data()$med_price[1])
    },
    error = function(e) {
      return("--")
    })
  })
  
  output$median_dom <- renderText({
    tryCatch({
      req(latest_data())
      paste0(latest_data()$med_dom[1], " days")
    },
    error = function(e) {
      return("--")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)