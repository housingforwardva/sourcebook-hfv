# Average Household Size Visualization -----------------------------------------

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

# Define UI
ui <- function(request) {
  page_fillable(
  theme = hfv_theme,
  useShinyjs(), # Initialize shinyjs

  # Main container using HFV classes
  div(
    class = "hfv-container",
    
    # Header using HFV styling
    div(
      class = "hfv-header",
      h4("Average Household Size Over Time", class = "hfv-title")
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
        
        # Tenure selector
        div(
          style = "margin-bottom: 16px;",
          selectInput("tenure", "Tenure:", 
                      choices = c("All", "Homeowner", "Renter"),
                      selected = "All",
                      width = "100%", 
                      selectize = FALSE)
        ),
        
        
        # Year range checkbox (optional feature)
        div(
          style = "margin-bottom: 16px;",
          checkboxInput("show_all_years", "Show All Years", value = TRUE)
        ),
        conditionalPanel(
          condition = "!input.show_all_years",
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
          )
        ),
        
        # Show trend line option
        div(
          style = "margin-bottom: 16px;",
          checkboxInput("show_trend", "Show Trend Line", value = TRUE)
        ),
        
        # Show point labels option
        div(
          style = "margin-bottom: 16px;",
          checkboxInput("show_labels", "Show Point Labels", value = FALSE)
        ),
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "U.S. Census Bureau, American Community Survey 5-Year Estimates",
            style = "margin-bottom: 0;"
          )
        )
      ),
        
      # Main Panel with single plot
      div(
        class = "hfv-chart-container",
        style = "height: 450px; margin-top: 16px;",
        girafeOutput("plot", height = "100%")
      )
    )
  )
  )
}

# Server function
server <- function(input, output, session) {
  # Load the data
  avg_size <- reactive({
    read_rds(here("data", "rds", "avg_hh_size.rds")) %>% 
      mutate(tenure = case_when(
        tenure == "Owner" ~ "Homeowner",
        TRUE ~ tenure
      ))
  })
  
  # Get available years
  year_list <- reactive({
    sort(unique(avg_size()$year))
  })
  
  # Get current geography from URL
  current_geo <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    list(
      type = query$geo %||% "state",
      cbsa = query$cbsa,
      locality = query$locality
    )
  })
  
  # Initialize dropdowns
  observe({
    # Years
    years <- year_list()
    updateSelectInput(session, "year_start", 
                      choices = years,
                      selected = min(years))
    updateSelectInput(session, "year_end", 
                      choices = years,
                      selected = max(years))
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
  
  # Single reactive for filtered data based on current geography
  filtered_data <- reactive({
    req(input$tenure)
    geo <- current_geo()
    
    if (geo$type == "state") {
      data <- avg_size() %>%
        filter(geography == "state",
               tenure == input$tenure)
    } else if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      data <- avg_size() %>%
        filter(geography == "cbsa",
               tenure == input$tenure,
               name == geo$cbsa)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      data <- avg_size() %>%
        filter(geography == "locality",
               tenure == input$tenure,
               name == geo$locality)
    } else {
      return(NULL)
    }
    
    # Apply year filter if needed
    if (!input$show_all_years) {
      req(input$year_start, input$year_end)
      data <- data %>%
        filter(year >= input$year_start, 
               year <= input$year_end)
    }
    
    # Calculate min/max points for labeling
    if (nrow(data) > 0) {
      data %>%
        mutate(label_point = year == min(year) | year == max(year) | 
                 estimate == max(estimate) | estimate == min(estimate))
    } else {
      data
    }
  })
  
  # Create title text
  title_text <- reactive({
    geo <- current_geo()
    if (geo$type == "state") {
      paste("Virginia", input$tenure, "Average Household Size")
    } else if (geo$type == "cbsa") {
      paste(input$tenure, "Average Household Size in", geo$cbsa)
    } else {
      paste(input$tenure, "Average Household Size in", geo$locality)
    }
  })
  
  # Subtitle text with year range
  subtitle_text <- reactive({
    if (input$show_all_years) {
      "All Available Years"
    } else {
      paste(input$year_start, "to", input$year_end)
    }
  })
  
  # Function to create an interactive plot
  create_interactive_plot <- function(data) {
    req(nrow(data) > 0)
    
    # Calculate y-axis limits with some padding
    y_min <- min(data$estimate) * 0.95
    y_max <- max(data$estimate) * 1.05
    
    # Add tooltips to the data
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Year: ", year, "\n",
        "Average Size: ", format(estimate, nsmall = 2)
      ))
    
    # Add margin of error to tooltip if it exists in the data
    if("moe" %in% colnames(plot_data)) {
      plot_data <- plot_data %>%
        mutate(tooltip = ifelse(
          !is.na(moe),
          paste0(tooltip, "\nMargin of Error: ±", format(moe, nsmall = 2)),
          tooltip
        ))
    }
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = year,
                    y = estimate)) +
      # Add interactive line
      geom_line(linewidth = 1, color = hfv_colors$shadow) +
      # Add interactive points
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = year),
        size = 3, 
        color = hfv_colors$shadow
      ) 
    
    # Add trend line if requested
    if (input$show_trend && nrow(data) >= 4) {
      p <- p + geom_smooth(method = "loess", 
                           se = TRUE, 
                           color = hfv_colors$sky, 
                           fill = hfv_colors$sky, 
                           alpha = 0.2)
    }
    
    # Add point labels if requested
    if (input$show_labels) {
      p <- p + geom_text(
        data = filter(plot_data, label_point),
        aes(label = format(estimate, nsmall = 2)),
        vjust = -0.8, 
        hjust = 0.5, 
        size = 3.5
      )
    }
    
    # Complete the plot
    p <- p + 
      scale_y_continuous(limits = c(y_min, y_max),
                         labels = scales::number_format(accuracy = 0.01)) +
      labs(
        title = title_text(),
        subtitle = subtitle_text(),
        x = "Year",
        y = "Average Household Size",
        caption = " " # Add empty caption to leave space for logo
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "grey80", fill = NA),
        legend.position = "none",
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
  
  # Render single plot based on current geography
  output$plot <- renderGirafe({
    data <- filtered_data()
    req(data)
    suppressWarnings(create_interactive_plot(data))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")