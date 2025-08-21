# Household Size Visualization -------------------------------------------------

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
# HOUSEHOLD SIZE VISUALIZATION
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


# =============================================================================
# LOAD DATA OUTSIDE SERVER
# ============================================================================= 

  # Load the data
  hh_size <- read_rds("b25009_data.rds")
  
  # Create a list of all unique CBSAs and localities in Virginia
  cbsa_list <- sort(unique(hh_size$cbsa_title))

  locality_list <- sort(unique(hh_size$name_long))

  year_list <- sort(unique(hh_size$year))

  
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
      h4("Household Distribution by Size", class = "hfv-title")
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
        
        # Common filters for all tabs
        div(
          style = "margin-bottom: 16px;",
          selectInput("tenure", "Tenure:", 
                      choices = c("All", "Homeowner", "Renter"),
                      selected = "All",
                      width = "100%", 
                      selectize = FALSE)
        ),
        
        # Year range selectors
        div(
          style = "margin-bottom: 16px;",
          layout_columns(
            col_widths = c(6, 6),
            gap = "2px",
            selectInput("year_start", "Start Year:", 
                        choices = NULL, 
                        selected = NULL, 
                        width = "100%",
                        selectize = FALSE),
            selectInput("year_end", "End Year:", 
                        choices = NULL, 
                        selected = NULL, 
                        width = "100%",
                        selectize = FALSE)
          )
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
            "U.S. Census Bureau, American Community Survey 5-Year Estimates, Table B25009",
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

# Server function
server <- function(input, output, session) {

  # Initialize dropdowns
  observe({
    years <- year_list

    updateSelectInput(session, "year_start", 
                      choices = years,
                      selected = min(years))
    updateSelectInput(session, "year_end", 
                      choices = years,
                      selected = max(years))
    
    updateSelectInput(session, "cbsa", 
                      choices = cbsa_list,
                      selected = if("Richmond, VA" %in% cbsa_list) "Richmond, VA" else cbsa_list[1])
    
    updateSelectInput(session, "locality", 
                      choices = locality_list,
                      selected = if("Richmond City" %in% locality_list) "Richmond City" else locality_list[1])
  })
  
  # Ensure end year is not earlier than start year
  observe({
    req(input$year_start, input$year_end)
    if (!is.null(input$year_start) && !is.null(input$year_end)) {
      if (as.numeric(input$year_start) > as.numeric(input$year_end)) {
        updateSelectInput(session, "year_end", selected = input$year_start)
      }
    }
  })
  
  # Pre-process data - Locality data
  locality_size <- reactive({
    hh_size %>% 
      # First aggregate by household size categories to handle multiple "4 or more person" entries
      group_by(year, name_long, hhsize, tenure) %>%
      summarise(estimate = sum(estimate), .groups = "drop") %>%
      pivot_wider(
        id_cols = c(year, name_long, hhsize),
        names_from = tenure,
        values_from = estimate
      ) %>% 
      mutate(All = Renter + Homeowner) %>% 
      pivot_longer(
        cols = c(Renter, Homeowner, All),
        names_to = "tenure",
        values_to = "estimate"
      ) %>%
      arrange(name_long, tenure, year) %>% 
      group_by(name_long, tenure, hhsize) %>% 
      mutate(pct_change = (estimate - lag(estimate))/lag(estimate)) %>% 
      group_by(year, name_long, tenure) %>% 
      mutate(percent = estimate/sum(estimate)) %>% 
      ungroup()
  })
  
  # CBSA data  
  cbsa_size <- reactive({
    hh_size %>% 
      # First aggregate by household size categories to handle multiple "4 or more person" entries
      group_by(year, cbsa_title, tenure, hhsize) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      pivot_wider(
        id_cols = c(year, cbsa_title, hhsize),
        names_from = tenure,
        values_from = estimate
      ) %>% 
      mutate(All = Renter + Homeowner) %>% 
      pivot_longer(
        cols = c(Renter, Homeowner, All),
        names_to = "tenure",
        values_to = "estimate"
      ) %>% 
      arrange(cbsa_title, tenure, year) %>% 
      group_by(cbsa_title, tenure, hhsize) %>% 
      mutate(pct_change = (estimate - lag(estimate))/lag(estimate)) %>% 
      group_by(year, cbsa_title, tenure) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      ungroup()
  })
  
  # State data
  state_size <- reactive({
    hh_size %>%
      # First aggregate by household size categories to handle multiple "4 or more person" entries
      group_by(year, tenure, hhsize) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>%  
      pivot_wider(
        id_cols = c(year, hhsize),
        names_from = tenure,
        values_from = estimate
      ) %>% 
      mutate(All = Renter + Homeowner) %>% 
      pivot_longer(
        cols = c(Renter, Homeowner, All),
        names_to = "tenure",
        values_to = "estimate"
      ) %>%
      arrange(tenure, year) %>% 
      group_by(tenure, hhsize) %>% 
      mutate(pct_change = (estimate - lag(estimate))/lag(estimate)) %>% 
      group_by(year, tenure) %>% 
      mutate(percent = estimate/sum(estimate)) %>% 
      ungroup()
  })
  
  # Create filtered datasets based on user selections
  state_filtered <- reactive({
    req(input$year_start, input$year_end, input$tenure)
    years <- c(input$year_start, input$year_end)
    
    # Only modify years if start and end are the same
    if(input$year_start == input$year_end) {
      years_list <- year_list()
      # If same year is selected for both, add another year to compare with
      if(as.numeric(input$year_start) == min(years_list)) {
        # If it's the earliest year, use the next available year
        if(length(years_list) > 1) {
          next_year <- sort(years_list)[2]
          years <- c(input$year_start, next_year)
        }
      } else {
        # Otherwise use the previous year
        year_pos <- which(years_list == input$year_start)
        if(length(year_pos) > 0 && year_pos > 1) {
          prev_year <- sort(years_list)[year_pos - 1]
          years <- c(prev_year, input$year_start)
        }
      }
    }
    
    # Filter the data
    filtered_data <- state_size() %>%
      filter(tenure == input$tenure,
             year %in% years) %>%
      mutate(year = as.character(year)) %>%
      arrange(year, hhsize)
    
    # Add percent change if we have multiple years
    if(length(unique(filtered_data$year)) > 1) {
      filtered_data <- filtered_data %>%
        group_by(hhsize) %>%
        mutate(pct_change = ifelse(year == max(year), 
                                   (estimate - estimate[year == min(year)]) / 
                                     estimate[year == min(year)],
                                   NA_real_)) %>%
        ungroup()
    } else {
      filtered_data <- filtered_data %>%
        mutate(pct_change = NA_real_)
    }
    
    # Add tooltip data
    filtered_data %>%
      mutate(tooltip = paste0(
        "Household Size: ", hhsize, "\n",
        "Year: ", year, "\n",
        "Households: ", format(estimate, big.mark = ","), 
        ifelse(!is.na(pct_change) & year == max(year), 
               paste0("\nChange: ", scales::percent(pct_change, accuracy = 0.1)), 
               "")
      ))
  })
  
  cbsa_filtered <- reactive({
    req(input$year_start, input$year_end, input$cbsa, input$tenure)
    years <- c(input$year_start, input$year_end)
    
    # Only modify years if start and end are the same
    if(input$year_start == input$year_end) {
      years_list <- year_list()
      # If same year is selected for both, add another year to compare with
      if(as.numeric(input$year_start) == min(years_list)) {
        # If it's the earliest year, use the next available year
        if(length(years_list) > 1) {
          next_year <- sort(years_list)[2]
          years <- c(input$year_start, next_year)
        }
      } else {
        # Otherwise use the previous year
        year_pos <- which(years_list == input$year_start)
        if(length(year_pos) > 0 && year_pos > 1) {
          prev_year <- sort(years_list)[year_pos - 1]
          years <- c(prev_year, input$year_start)
        }
      }
    }
    
    # Filter the data
    filtered_data <- cbsa_size() %>%
      filter(cbsa_title == input$cbsa,
             tenure == input$tenure,
             year %in% years) %>%
      mutate(year = as.character(year)) %>%
      arrange(year, hhsize)
    
    # Add percent change if we have multiple years
    if(length(unique(filtered_data$year)) > 1) {
      filtered_data <- filtered_data %>%
        group_by(hhsize) %>%
        mutate(pct_change = ifelse(year == max(year), 
                                   (estimate - estimate[year == min(year)]) / 
                                     estimate[year == min(year)],
                                   NA_real_)) %>%
        ungroup()
    } else {
      filtered_data <- filtered_data %>%
        mutate(pct_change = NA_real_)
    }
    
    # Add tooltip data
    filtered_data %>%
      mutate(tooltip = paste0(
        "Household Size: ", hhsize, "\n",
        "Year: ", year, "\n",
        "Households: ", format(estimate, big.mark = ","), 
        ifelse(!is.na(pct_change) & year == max(year), 
               paste0("\nChange: ", scales::percent(pct_change, accuracy = 0.1)), 
               "")
      ))
  })
  
  locality_filtered <- reactive({
    req(input$year_start, input$year_end, input$locality, input$tenure)
    years <- c(input$year_start, input$year_end)
    
    # Only modify years if start and end are the same
    if(input$year_start == input$year_end) {
      years_list <- year_list()
      # If same year is selected for both, add another year to compare with
      if(as.numeric(input$year_start) == min(years_list)) {
        # If it's the earliest year, use the next available year
        if(length(years_list) > 1) {
          next_year <- sort(years_list)[2]
          years <- c(input$year_start, next_year)
        }
      } else {
        # Otherwise use the previous year
        year_pos <- which(years_list == input$year_start)
        if(length(year_pos) > 0 && year_pos > 1) {
          prev_year <- sort(years_list)[year_pos - 1]
          years <- c(prev_year, input$year_start)
        }
      }
    }
    
    # Filter the data
    filtered_data <- locality_size() %>%
      filter(name_long == input$locality,
             tenure == input$tenure,
             year %in% years) %>%
      mutate(year = as.character(year)) %>%
      arrange(year, hhsize)
    
    # Add percent change if we have multiple years
    if(length(unique(filtered_data$year)) > 1) {
      filtered_data <- filtered_data %>%
        group_by(hhsize) %>%
        mutate(pct_change = ifelse(year == max(year), 
                                   (estimate - estimate[year == min(year)]) / 
                                     estimate[year == min(year)],
                                   NA_real_)) %>%
        ungroup()
    } else {
      filtered_data <- filtered_data %>%
        mutate(pct_change = NA_real_)
    }
    
    # Add tooltip data
    filtered_data %>%
      mutate(tooltip = paste0(
        "Household Size: ", hhsize, "\n",
        "Year: ", year, "\n",
        "Households: ", format(estimate, big.mark = ","), 
        ifelse(!is.na(pct_change) & year == max(year), 
               paste0("\nChange: ", scales::percent(pct_change, accuracy = 0.1)), 
               "")
      ))
  })
  
  # Create title text
  title_text <- reactive({
    if (input$tabs == "state") {
      paste(input$tenure, "Households by Size in Virginia")
    } else if (input$tabs == "cbsa") {
      paste(input$tenure, "Households by Size in", input$cbsa)
    } else {
      paste(input$tenure, "Households by Size in", input$locality)
    }
  })
  
  subtitle_text <- reactive({
    earliest_year <- input$year_start
    latest_year <- input$year_end
    paste("Comparing", earliest_year, "to", latest_year)
  })
  
  # Function to create an interactive plot
  create_interactive_plot <- function(data) {
    req(nrow(data) > 0)
    
    latest_year <- max(as.character(data$year))
    
    # Create base plot
    p <- ggplot(data, aes(x = year, y = estimate, fill = year)) + 
      geom_col_interactive(aes(tooltip = tooltip, data_id = paste(hhsize, year))) +
      facet_wrap(~hhsize, nrow = 1, scales = "free_y") +
      # Add percent change text only if we have data for multiple years
      {
        if(length(unique(data$year)) > 1)
          geom_text(
            data = filter(data, year == latest_year & !is.na(pct_change)),
            aes(label = scales::percent(pct_change, accuracy = 0.1)),
            position = position_stack(),
            vjust = -0.5,
            size = 3
          )
      } +
      scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
      scale_fill_manual(values = c(hfv_colors$sky, hfv_colors$shadow)) +
      labs(
        title = title_text(),
        subtitle = subtitle_text(),
        caption = " " # Add empty caption to leave space for logo
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.5, "lines"),  # Tighter spacing for compact view
        panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 8),  # Smaller facet titles
        axis.text = element_text(size = 8),   # Smaller axis text
        axis.title = element_blank(),         # Remove axis titles
        plot.title = element_text(size = 11), # Smaller plot title
        plot.subtitle = element_text(size = 9), # Smaller subtitle
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
    
    # Return interactive plot with logo
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
  
  # Render plots
  output$state_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(state_filtered()))
  })
  
  output$cbsa_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(cbsa_filtered()))
  })
  
  output$local_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(locality_filtered()))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)