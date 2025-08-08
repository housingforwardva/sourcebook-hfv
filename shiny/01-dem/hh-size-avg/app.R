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

# =============================================================================
# HFV STYLING SYSTEM INTEGRATION
# =============================================================================

# Register Google Fonts for ggiraph plots and system
register_gfont("Open Sans")
register_gfont("Poppins")

# Register fonts with systemfonts using Google Fonts URLs
tryCatch({
  # For local development and server rendering, we'll use fallback fonts
  # The web fonts are handled by the HTML dependencies in girafe
  message("Google Fonts registered for web rendering")
}, error = function(e) {
  message("Font registration warning: ", e$message)
})

# Compile HFV styles if needed (for deployment compatibility)
compile_hfv_styles_if_needed <- function() {
  css_file <- "www/styles/hfv-theme.css"
  scss_file <- "www/styles/hfv-theme.scss"
  
  # Only compile if CSS doesn't exist or SCSS is newer
  if (!file.exists(css_file) || 
      (file.exists(scss_file) && file.mtime(scss_file) > file.mtime(css_file))) {
    
    message("🔄 Compiling HFV styles...")
    
    # Ensure the CSS directory exists
    dir.create(dirname(css_file), recursive = TRUE, showWarnings = FALSE)
    
    # Compile SCSS to CSS
    tryCatch({
      sass(
        list(sass_file(scss_file)),
        output = css_file,
        options = sass_options(
          output_style = "expanded",
          source_map_embed = FALSE
        )
      )
      message("✅ HFV styles compiled successfully!")
    }, error = function(e) {
      warning("❌ Failed to compile SCSS: ", e$message)
      warning("📝 Using fallback inline styles...")
    })
  }
  
  return(file.exists(css_file))
}

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
ui <- page_fillable(
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
  
  # Get available localities
  locality_list <- reactive({
    avg_size() %>%
      filter(geography == "locality") %>%
      pull(name) %>%
      unique() %>%
      sort()
  })
  
  # Get available CBSAs
  cbsa_list <- reactive({
    avg_size() %>%
      filter(geography == "cbsa") %>%
      pull(name) %>%
      unique() %>%
      sort()
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
    
    # CBSAs
    cbsas <- cbsa_list()
    updateSelectInput(session, "cbsa", 
                      choices = cbsas,
                      selected = if("Richmond, VA" %in% cbsas) "Richmond, VA" else cbsas[1])
    
    # Localities
    localities <- locality_list()
    updateSelectInput(session, "locality", 
                      choices = localities,
                      selected = if("Richmond City" %in% localities) "Richmond City" else localities[1])
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
  
  # Filter data based on selections
  filtered_state <- reactive({
    req(input$tenure)
    
    data <- avg_size() %>%
      filter(geography == "state",
             tenure == input$tenure)
    
    # Apply year filter if needed
    if (!input$show_all_years) {
      req(input$year_start, input$year_end)
      data <- data %>%
        filter(year >= input$year_start, 
               year <= input$year_end)
    }
    
    # Calculate min/max points for labeling
    data %>%
      mutate(label_point = year == min(year) | year == max(year) | 
               estimate == max(estimate) | estimate == min(estimate))
  })
  
  filtered_cbsa <- reactive({
    req(input$tenure, input$cbsa)
    
    data <- avg_size() %>%
      filter(geography == "cbsa",
             tenure == input$tenure,
             name == input$cbsa)
    
    # Apply year filter if needed
    if (!input$show_all_years) {
      req(input$year_start, input$year_end)
      data <- data %>%
        filter(year >= input$year_start, 
               year <= input$year_end)
    }
    
    # Calculate min/max points for labeling
    data %>%
      mutate(label_point = year == min(year) | year == max(year) | 
               estimate == max(estimate) | estimate == min(estimate))
  })
  
  filtered_locality <- reactive({
    req(input$tenure, input$locality)
    
    data <- avg_size() %>%
      filter(geography == "locality",
             tenure == input$tenure,
             name == input$locality)
    
    # Apply year filter if needed
    if (!input$show_all_years) {
      req(input$year_start, input$year_end)
      data <- data %>%
        filter(year >= input$year_start, 
               year <= input$year_end)
    }
    
    # Calculate min/max points for labeling
    data %>%
      mutate(label_point = year == min(year) | year == max(year) | 
               estimate == max(estimate) | estimate == min(estimate))
  })
  
  # Create title text
  title_text <- reactive({
    if (input$tabs == "state") {
      paste(input$tenure, "Average Household Size in Virginia")
    } else if (input$tabs == "cbsa") {
      paste(input$tenure, "Average Household Size in", input$cbsa)
    } else {
      paste(input$tenure, "Average Household Size in", input$locality)
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
  
  # Render plots
  output$state_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(filtered_state()))
  })
  
  output$cbsa_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(filtered_cbsa()))
  })
  
  output$local_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(filtered_locality()))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)