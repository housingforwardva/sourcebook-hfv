library(shiny)
library(tidyverse)
library(ggiraph)
library(systemfonts)
library(here)
library(grid)
library(png)
library(bslib)
library(cowplot)
library(scales)
library(shinyjs)
library(magick)
library(arrow)

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

# Define consistent colors for race_ethnicity categories
race_ethnicity_colors <- c(
  "White, non-Hispanic" = hfv_colors$shadow,
  "Black" = hfv_colors$berry,
  "Hispanic or Latino" = hfv_colors$desert,
  "Asian" = hfv_colors$grass,
  "Other Minority" = hfv_colors$lilac,
  "White Co-Applicant" = hfv_colors$shadow_light,
  "Incomplete/No Data" = "#CCCCCC"
)

# Create a Bootstrap theme
hfv_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#333333",
  primary = hfv_colors$sky,
  secondary = hfv_colors$shadow,
  success = hfv_colors$grass,
  info = hfv_colors$lilac,
  warning = hfv_colors$desert,
  danger = hfv_colors$berry,
  base_font = font_google("Open Sans"),
  heading_font = font_google("Poppins"),
  font_scale = 0.8
)

# Define UI
ui <- page_fillable(
  theme = hfv_theme,
  useShinyjs(),

  # Mobile optimization viewport
  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    )
  ),

  # CSS styles (same as reference app)
  tags$head(
    tags$style(HTML(
      "
      body, html {
        margin: 0;
        padding: 0;
        height: auto;
        overflow-x: hidden;
      }
      
      @media (max-height: 600px) {
        .hfv-container {
          padding: 10px !important;
          margin: 0 auto !important;
          max-height: 500px !important;
          overflow: hidden !important;
        }
        
        .hfv-header {
          margin-bottom: 8px !important;
        }
        
        .hfv-sidebar {
          padding: 8px !important;
        }
        
        .girafe-container {
          height: 280px !important;
          min-height: 280px !important;
        }
        
        body, html {
          overflow: hidden !important;
        }
      }
      
      .hfv-container {
        max-width: 1200px; 
        margin: 0 auto; 
        padding: 45px;
      }
      
      .hfv-header {
        display: flex; 
        align-items: center; 
        margin-bottom: 15px; 
        border-bottom: 2px solid #40C0C0; 
        padding-bottom: 8px;
      }
      
      .hfv-header img {
        height: 30px;
        margin-right: 10px;
      }
      
      .title-text {
        margin: 0; 
        color: #011E41;
        font-size: 20px;
      }
      
      .hfv-sidebar {
        background-color: #E8EDF2;
        padding: 15px;
        border-radius: 5px;
      }
      
      .girafe-container {
        width: 100%;
        height: auto;
        min-height: 350px;
        overflow: visible;
      }
      
      .girafe-container svg {
        width: 100% !important;
        height: 100% !important;
      }
      
      @media (max-width: 992px) {
        .hfv-container {
          padding: 10px;
        }
        
        .title-text {
          font-size: 18px;
        }
        
        .girafe-container {
          height: 400px;
        }
      }
      
      @media (max-width: 768px) {
        .hfv-container {
          padding: 8px;
          border-width: 1px;
        }
        
        .title-text {
          font-size: 16px;
        }
        
        .hfv-header {
          margin-bottom: 10px;
        }
        
        .hfv-sidebar {
          padding: 10px;
          margin-bottom: 10px;
        }
        
        .control-label {
          font-size: 12px;
        }
        
        .form-check-label {
          font-size: 11px;
        }
        
        .form-select {
          font-size: 12px;
        }
        
        .form-control {
          font-size: 12px;
        }
        
        .girafe-container {
          height: 350px;
        }
      }
      
      @media (max-width: 480px) {
        .hfv-container {
          padding: 5px;
        }
        
        .hfv-header img {
          height: 25px;
        }
        
        .title-text {
          font-size: 14px;
        }
        
        .hfv-sidebar {
          padding: 8px;
        }
        
        .girafe-container {
          height: 300px;
        }
        
        .nav-tabs .nav-link {
          font-size: 13px;
          padding: 6px 10px;
        }
      }
      
      /* Filter styling */
      .hfv-sidebar .control-label {
        font-size: 11px !important;
      }
      
      .hfv-sidebar .form-control,
      .hfv-sidebar .form-select {
        font-size: 11px !important;
      }
      
      /* Add space between tabs and plot */
      .nav-tabs {
        margin-bottom: 20px;
      }
      "
    ))
  ),

  # Main container
  div(
    class = "hfv-container",
    
    # Header with logo and title
    div(
      class = "hfv-header",
      img(
        src = "https://housingforwardva.org/wp-content/uploads/2025/05/HousingForward-VA-Logo-Files-Icon-One-Color-RGB.png",
        alt = "HousingForward VA Logo"
      ),
      h4("Mortgage Denial Rates by Race and Ethnicity", class = "title-text")
    ),

    # Responsive layout
    layout_columns(
      fillable = TRUE,
      col_widths = c(
        lg = c(3, 9),
        md = c(4, 8),
        sm = c(12, 12)
      ),

      # Sidebar Panel
      div(
        class = "hfv-sidebar",
        
        # Year selector
        div(
          style = "margin-bottom: 10px;",
          selectInput("year", "Year:", 
                     choices = c(2018:2024), 
                     selected = 2024, 
                     width = "100%", 
                     selectize = FALSE)
        ),
        
        # Loan purpose selector
        div(
          style = "margin-bottom: 10px;",
          selectInput("loan_purpose", "Loan Purpose:", 
                     choices = c("Home purchase", "Home improvement", "Refinancing", "Cash-out refinancing", "Other purpose"),
                     selected = "Home purchase", 
                     width = "100%", 
                     selectize = FALSE)
        ),
        
        # Occupancy type selector
        div(
          style = "margin-bottom: 10px;",
          selectInput("occupancy_type", "Occupancy Type:", 
                     choices = c("Principal residence", "Second residence", "Investment property"),
                     selected = "Principal residence", 
                     width = "100%", 
                     selectize = FALSE)
        ),
        
        # Geographic selectors (conditional)
        div(
          style = "margin-bottom: 10px;",
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            selectInput("cbsa", "Metro Area:", choices = NULL, width = "100%", selectize = FALSE)
          ),
          conditionalPanel(
            condition = "input.tabs == 'local'",
            selectInput("locality", "Locality:", choices = NULL, width = "100%", selectize = FALSE)
          )
        ),
        
        # Horizontal line
        hr(style = "margin: 3px 0;"),
        
        # Source information
        div(
          style = "font-size: 10px; color: #666; margin-top: 2px;",
          p(
            "Source: Consumer Financial Protection Bureau, Home Mortgage Disclosure Act (HMDA) data.",
            style = "margin-bottom: 0;"
          )
        )
      ),
      
      # Main Panel (tabs)
      div(
        navset_tab(
          id = "tabs",
          nav_panel(
            title = "State", 
            value = "state",
            div(class = "girafe-container", girafeOutput("state_plot", height = "100%"))
          ),
          nav_panel(
            title = "Metro Area", 
            value = "cbsa",
            div(class = "girafe-container", girafeOutput("cbsa_plot", height = "100%"))
          ),
          nav_panel(
            title = "Locality", 
            value = "local",
            div(class = "girafe-container", girafeOutput("local_plot", height = "100%"))
          )
        )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Load the data (using path relative to project root)
  local_lookup <- reactive({
    read_csv("../../data/local_lookup.csv") |> 
      mutate(fips_full = as.character(fips_full))
  })
  
  loans_race <- reactive({
    read_parquet("../../data/parquet/hmda_va_clean.parquet") |> 
      select(activity_year, lei, fips_full = county_code, race_ethnicity, action_taken, purchaser_type, loan_purpose,
             occupancy_type) |> 
      mutate(count = 1) |> 
      group_by(activity_year, fips_full, race_ethnicity, action_taken, loan_purpose, occupancy_type) |> 
      summarise(count = sum(count), .groups = "drop") %>% 
      left_join(local_lookup(), by = "fips_full") |> 
      filter(state == "Virginia")
  })
  
  # Get available CBSAs and localities
  cbsa_list <- reactive({
    loans_race() %>%
      filter(!is.na(cbsa_title)) %>%
      pull(cbsa_title) %>%
      unique() %>%
      sort()
  })
  
  locality_list <- reactive({
    loans_race() %>%
      filter(!is.na(name_long)) %>%
      pull(name_long) %>%
      unique() %>%
      sort()
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
  
  # Create state-level denial data
  state_data <- reactive({
    loans_race() |> 
      filter(activity_year == input$year) |> 
      group_by(state, race_ethnicity, loan_purpose, occupancy_type) |> 
      mutate(total = sum(count)) |> 
      filter(loan_purpose == input$loan_purpose) |> 
      filter(occupancy_type == input$occupancy_type) |> 
      group_by(race_ethnicity, action_taken, total) |> 
      summarise(count = sum(count), .groups = "drop") |> 
      mutate(rate = count/total) |> 
      filter(action_taken == "Application denied") |> 
      arrange(desc(rate))
  })
  
  # Create CBSA-level denial data
  cbsa_data <- reactive({
    req(input$cbsa)
    
    loans_race() |> 
      filter(activity_year == input$year) |> 
      group_by(cbsa_title, race_ethnicity, loan_purpose, occupancy_type) |> 
      mutate(total = sum(count)) |> 
      filter(loan_purpose == input$loan_purpose) |> 
      filter(occupancy_type == input$occupancy_type) |> 
      filter(cbsa_title == input$cbsa) |> 
      group_by(cbsa_title, race_ethnicity, action_taken, total) |> 
      summarise(count = sum(count), .groups = "drop") |> 
      mutate(rate = count/total) |> 
      filter(action_taken == "Application denied") |> 
      arrange(desc(rate))
  })
  
  # Create locality-level denial data
  locality_data <- reactive({
    req(input$locality)
    
    loans_race() |> 
      filter(activity_year == input$year) |> 
      group_by(name_long, race_ethnicity, loan_purpose, occupancy_type) |> 
      mutate(total = sum(count)) |> 
      filter(loan_purpose == input$loan_purpose) |> 
      filter(occupancy_type == input$occupancy_type) |> 
      filter(name_long == input$locality) |> 
      group_by(name_long, race_ethnicity, action_taken, total) |> 
      summarise(count = sum(count), .groups = "drop") |> 
      mutate(rate = count/total) |> 
      filter(action_taken == "Application denied") |> 
      arrange(desc(rate))
  })
  
  # Plot titles
  state_title <- reactive({
    paste("Virginia Loan Denial Rates by Race/Ethnicity -", input$year)
  })
  
  cbsa_title <- reactive({
    paste("Loan Denial Rates by Race/Ethnicity -", input$cbsa, "-", input$year)
  })
  
  locality_title <- reactive({
    paste("Loan Denial Rates by Race/Ethnicity -", input$locality, "-", input$year)
  })
  
  # Function to create denial rate plots
  create_denial_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", race_ethnicity, "\n",
        "Denial Rate: ", percent(rate, accuracy = 0.1), "\n",
        "Applications Denied: ", format(count, big.mark = ","), "\n",
        "Total Applications: ", format(total, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = reorder(race_ethnicity, rate),
                    y = rate,
                    fill = race_ethnicity)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = race_ethnicity),
        width = 0.7
      ) +
      geom_text(
        aes(label = percent(rate, accuracy = 0.1)),
        hjust = -0.1,
        size = 3,
        color = "black"
      ) +
      scale_fill_manual(values = race_ethnicity_colors, na.value = "#CCCCCC") +
      coord_flip() +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        expand = expansion(mult = c(0, 0.15))
      ) +
      labs(
        title = title_text,
        caption = " ",
        y = "Denial Rate",
        x = "Race/Ethnicity"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 20, 30, 5)
      )
    
    # Add logo
    logo_url <- "https://housingforwardva.org/wp-content/uploads/2024/08/HousingForward-VA-Logo-Files-Horizontal-Gradient-RGB.png"
    
    p_with_logo <- ggdraw(p) +
      draw_image(
        logo_url,
        x = 0.85,
        y = 0.05,
        width = 0.15,
        height = 0.15
      )
    
    return(p_with_logo)
  }
  
  # Convert to interactive girafe
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
      )
    )
  }
  
  # Render the plots
  output$state_plot <- renderGirafe({
    create_interactive_plot(create_denial_plot(state_data(), state_title()))
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_denial_plot(cbsa_data(), cbsa_title()))
  })
  
  output$local_plot <- renderGirafe({
    create_interactive_plot(create_denial_plot(locality_data(), locality_title()))
  })
  
  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)