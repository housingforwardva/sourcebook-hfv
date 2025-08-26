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
# INCOME DISTRIBUTION BY TENURE VISUALIZATION
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

# Load data
  inc_dist <- read_rds("./data.rds")
  
  # Define income order once
  income_order <- c("Under $10,000", "$10,000 to $19,999", "$20,000 to $34,999",
                    "$35,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999",
                    "$100,000 to $149,999","$150,000 or more")
  
    years <- unique(inc_dist$year)

    cbsas <- inc_dist %>%
      pull(cbsa_title) %>%
      unique() %>%
      sort()

    counties <- inc_dist %>%
      pull(name_long) %>%
      unique() %>%
      sort()
  
  # Create state-level data
  state_data <-  inc_dist %>% 
      group_by(year, tenure, income_range) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      mutate(income = factor(income_range, levels = income_order))

  # Create CBSA-level data
  cbsa_data <- inc_dist %>% 
      group_by(year, cbsa_title, tenure, income_range) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      mutate(income = factor(income_range, levels = income_order))

  
  # Create local-level data
  local_data <- inc_dist %>% 
    mutate(income = factor(income_range, levels = income_order))


# =============================================================================
# USER INTERFACE
# =============================================================================

# Define UI
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
      h4("Income Distribution by Tenure", class = "hfv-title")
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
        
        # Year select
        div(
          style = "margin-bottom: 16px;",
          selectInput("year", "Select Year:", 
                      choices = years, 
                      selected = max(years), 
                      width = "100%",
                      selectize = FALSE)
        ),
        
        # Geography selectors
        div(
          style = "margin-bottom: 16px;",
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            selectInput("cbsa", "Metro Area:", choices = cbsas, width = "100%", selectize = FALSE)
          ),
          conditionalPanel(
            condition = "input.tabs == 'local'",
            selectInput("county", "Locality:", choices = counties, width = "100%", selectize = FALSE)
          )
        ),
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "U.S. Census Bureau. American Community Survey 5-Year Estimates. Table B25118",
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
  
  # Create title text
  title_text <- reactive({
    paste("Income Distribution in", input$year)
  })

  filtered_state <- reactive({
    req(input$year)

    state_data |> 
      filter(year == input$year)
  
    }
  )

  filtered_local <- reactive({
    req(input$year)

    local_data |> 
      filter(year == input$year) |> 
      filter(name_long == input$county)
  
    }
  )

    filtered_cbsa <- reactive({
    req(input$year)

    cbsa_data |> 
      filter(year == input$year) |> 
      filter(cbsa_title == input$cbsa)
  
    }
  )

  
  # Create a plot function for income distribution
  create_plot <- function(data) {
    # Add tooltip text to the data
    data <- data %>%
      mutate(tooltip = paste0(
        "Income: ", income, "\n",
        "Tenure: ", tenure, "\n",
        "Households: ", format(estimate, big.mark = ",")
      ))
    
    # Create a pure, base ggplot with no theme customizations that could cause conflicts
    p <- ggplot(data, 
                aes(x = income, 
                    y = estimate, 
                    fill = tenure)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = paste(income, tenure)),
        position = "stack"
      ) +
      facet_wrap(~tenure, ncol = 1) +
      # Use Housing Forward Virginia colors
      scale_fill_manual(values = c("Homeowner" = "#40C0C0", "Renter" = "#011E41")) +
      scale_y_continuous(labels = scales::number_format(big.mark = ","),
                         expand = expansion(mult = c(0, 0.1))) +
      labs(
        title = title_text(),
        caption = " ", # Add empty caption to leave space for logo
        y = "Number of Households",
        x = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.minor = element_blank(),
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
  
  # Render the state plot
  output$state_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_plot(filtered_state())))
  })
  
  # Render the CBSA plot
  output$cbsa_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_plot(filtered_cbsa())))
  })
  
  # Render the local plot
  output$local_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_plot(filtered_local())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)