library(shiny)
library(tidyverse)
library(ggiraph)     # For interactive ggplots
library(systemfonts) # For font_google
library(here)        # For here() function in file paths
library(grid)        # For grobs
library(png)         # For reading PNG files
library(bslib)       # For modern UI components
library(cowplot)     # For adding logo to plots
library(scales)      # For number formatting
library(shinyjs)     # For dynamic UI updates
library(magick)      # For image handling
library(forcats)

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

# Define UI
ui <- page_fillable(
  theme = hfv_theme,
  useShinyjs(), # Initialize shinyjs
  
  # MOBILE OPTIMIZATION #1: Add the viewport meta tag for mobile devices
  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    )
  ),
  
  # MOBILE OPTIMIZATION #2: Add CSS with media queries for responsive design
  tags$head(
    tags$style(HTML(
      "
      /* Base styles for all screen sizes */
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
      
      /* Container styles */
      .hfv-container {
        max-width: 1200px; 
        margin: 0 auto; 
        padding: 45px;
      }
      
      /* Header styles */
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
      
      /* Sidebar panel styles */
      .hfv-sidebar {
        background-color: #E8EDF2;
        padding: 15px;
        border-radius: 5px;
      }
      
      /* Plot container styles */
      .girafe-container {
        width: 100%;
        height: 450px;
        overflow: visible;
      }
      
      .girafe-container svg {
        width: 100% !important;
        height: 100% !important;
      }
      
      /* MOBILE OPTIMIZATION #3: Medium-sized screens (tablets, smaller laptops) */
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
      
      /* MOBILE OPTIMIZATION #4: Small screens (large phones, small tablets) */
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
      
      /* MOBILE OPTIMIZATION #5: Extra-small screens (phones) */
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
    "
    ))
  ),
  
  # Main container with responsive padding
  div(
    class = "hfv-container",
    
    # Header with logo and title
    div(
      class = "hfv-header",
      img(
        src = "https://housingforwardva.org/wp-content/uploads/2025/05/HousingForward-VA-Logo-Files-Icon-One-Color-RGB.png",
        alt = "HousingForward VA Logo"
      ),
      h4("Housing Supply and Demand Gap", class = "title-text")
    ),
    
    # MOBILE OPTIMIZATION #6: Responsive grid layout with different column widths for different screen sizes
    layout_columns(
      fillable = TRUE,
      col_widths = c(
        # For larger screens (lg and up): sidebar takes 25% width, main content takes 75%
        lg = c(3, 9),
        # For medium screens (md): sidebar takes 33% width, main content takes 67%
        md = c(4, 8),
        # For small screens (sm and xs): full width stacked layout
        sm = c(12, 12)
      ),
      
      # Sidebar Panel
      div(
        class = "hfv-sidebar",
        
        # Year select
        div(
          style = "margin-bottom: 15px;",
          selectInput(
            "year",
            "Select Year:",
            choices = NULL,
            selected = NULL,
            width = "100%",
            selectize = TRUE
          )
        ),
        
        # Geography selectors
        div(
          style = "margin-bottom: 15px;",
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            selectInput(
              "cbsa",
              "Metro Area:",
              choices = NULL,
              width = "100%",
              selectize = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.tabs == 'local'",
            selectInput(
              "locality",
              "Locality:",
              choices = NULL,
              width = "100%",
              selectize = TRUE
            )
          )
        ),
        
        # Horizontal line
        hr(style = "margin: 15px 0;"),
        
        # Source information
        div(
          style = "font-size: 10px; color: #666; margin-top: 8px;",
          p("Source: U.S. Department of Housing and Urban Development (HUD), Comprehensive Housing Affordability Strategy (CHAS) data.")
        )
      ),
      
      # Main Panel (tabs)
      div(
        style = "width: 100%;",
        
        navset_tab(
          id = "tabs",
          nav_panel(
            title = "State",
            value = "state",
            padding = 5,
            # MOBILE OPTIMIZATION #7: Direct plot output without uiOutput wrappers
            div(class = "girafe-container", girafeOutput("state_plot"))
          ),
          nav_panel(
            title = "Metro Area",
            value = "cbsa",
            padding = 5,
            div(class = "girafe-container", girafeOutput("cbsa_plot"))
          ),
          nav_panel(
            title = "Locality",
            value = "local",
            padding = 5,
            div(class = "girafe-container", girafeOutput("local_plot"))
          )
        )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Define the desired factor order
  match_order <- c("Very affordable", "Affordable", "Unaffordable")
  
  # Load the data
  gap <- reactive({
    read_rds(here("data", "rds", "table18c_chas.rds")) %>% 
      mutate(
        household_income = factor(household_income, 
                                  levels = c("30% AMI or less", 
                                             "31 to 50% AMI", 
                                             "51 to 80% AMI", 
                                             "81% AMI or greater")),
        # Apply factor ordering immediately when loading data
        match = factor(match, levels = match_order)
      )
  })
  
  # Load lookup table
  lookup <- reactive({
    read_csv(here("data", "local_lookup.csv")) %>% 
      mutate(fips = fips_full)
  })
  
  # Join data with lookup
  gap_join <- reactive({
    gap() %>% 
      left_join(lookup(), by = "fips")
  })
  
  # Pre-compute state, CBSA, and local data 
  state_data <- reactive({
    gap_join() %>% 
      group_by(year, household_income, match, gapcode) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      mutate(
        estimate = case_when(
          gapcode == "Gap" ~ -estimate,
          TRUE ~ estimate
        ),
        # Ensure factor order is preserved after grouping
        match = factor(match, levels = match_order)
      )
  })
  
  cbsa_data <- reactive({
    gap_join() %>% 
      group_by(year, cbsa_title, household_income, match, gapcode) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      mutate(
        estimate = case_when(
          gapcode == "Gap" ~ -estimate,
          TRUE ~ estimate
        ),
        # Ensure factor order is preserved after grouping
        match = factor(match, levels = match_order)
      )
  })
  
  local_data <- reactive({
    gap_join() %>% 
      group_by(year, name_long, household_income, match, gapcode) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      mutate(
        estimate = case_when(
          gapcode == "Gap" ~ -estimate,
          TRUE ~ estimate
        ),
        # Ensure factor order is preserved after grouping
        match = factor(match, levels = match_order)
      )
  })
  
  # Get available years
  observe({
    years <- unique(gap()$year)
    updateSelectInput(session, "year", 
                      choices = sort(years, decreasing = TRUE),
                      selected = max(years))
  })
  
  # Get available CBSAs
  cbsa_list <- reactive({
    cbsa_data() %>% 
      filter(year == input$year) %>%
      pull(cbsa_title) %>%
      unique() %>%
      sort()
  })
  
  locality_list <- reactive({
    local_data() %>% 
      filter(year == input$year) %>%
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
  
  # Filter data for plots - simplified since factor order is now set earlier
  filtered_state <- reactive({
    req(input$year)
    
    state_data() %>%
      filter(year == input$year)
  })
  
  filtered_cbsa <- reactive({
    req(input$cbsa, input$year)
    
    cbsa_data() %>%
      filter(cbsa_title == input$cbsa,
             year == input$year)
  })
  
  filtered_local <- reactive({
    req(input$locality, input$year)
    
    local_data() %>%
      filter(name_long == input$locality,
             year == input$year)
  })
  
  # Function to create plots
  create_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Add tooltips to the data
    plot_data <- data %>%
      mutate(
        abs_estimate = abs(estimate),
        tooltip = paste0(
          "Income Level: ", household_income, "\n",
          "Affordability: ", match, "\n",
          "Type: ", gapcode, "\n",
          "Units: ", format(abs_estimate, big.mark = ",")
        )
      )
    
    # Create base plot
    p <- ggplot(plot_data, 
                aes(x = household_income,
                    y = estimate,
                    fill = match,
                    group = match)) +  # ADD group = match for ggiraph
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = paste(household_income, match)),
        position = "stack"
      ) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      scale_fill_manual(
        values = c(
          "Very affordable" = hfv_colors$sky,
          "Affordable" = hfv_colors$grass,
          "Unaffordable" = hfv_colors$berry
        ),
        limits = match_order,  # Use limits instead of breaks to control both legend and plot order
        drop = FALSE  # Don't drop unused levels
      ) +
      scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
      labs(
        title = title_text,
        subtitle = paste("Year:", input$year, "| Negative values represent housing gap"),
        caption = " ", # Add empty caption to leave space for logo
        y = "Number of Housing Units",
        x = "Household Income Level"
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
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
        opts_sizing(rescale = TRUE)
      )
    )
  }
  
  # Render the state plot
  output$state_plot <- renderGirafe({
    create_interactive_plot(create_plot(filtered_state(), "Virginia Housing Supply and Demand Gap"))
  })
  
  # Render the CBSA plot
  output$cbsa_plot <- renderGirafe({
    title_text <- paste("Housing Supply and Demand Gap in", input$cbsa)
    create_interactive_plot(create_plot(filtered_cbsa(), title_text))
  })
  
  # Render the local plot
  output$local_plot <- renderGirafe({
    title_text <- paste("Housing Supply and Demand Gap in", input$locality)
    create_interactive_plot(create_plot(filtered_local(), title_text))
  })
  
  # MOBILE OPTIMIZATION #8: Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)