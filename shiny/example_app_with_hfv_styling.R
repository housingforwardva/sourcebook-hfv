# =============================================================================
# PRACTICAL EXAMPLE: Building Permits App with HFV Shared Styling
# This shows exactly how to integrate the new styling system into app.R
# =============================================================================

library(shiny)
library(tidyverse)
library(plotly)
library(bslib)
library(sass)
library(here)

# =============================================================================
# DEPLOYMENT STRATEGY FOR POSIT CONNECT CLOUD
# =============================================================================

# OPTION 1: COMPILE CSS DURING APP STARTUP (RECOMMENDED)
# This approach compiles SCSS to CSS when the app starts, ensuring
# the latest styles are always available without manual compilation

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
          output_style = "compressed",
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

# OPTION 2: PRE-COMPILED CSS (ALTERNATIVE)
# If you prefer to pre-compile CSS and deploy it with your app,
# you can run this once locally and commit the CSS file to your repo

pre_compile_hfv_styles <- function() {
  message("🔄 Pre-compiling HFV styles for deployment...")
  
  sass(
    list(sass_file("www/styles/hfv-theme.scss")),
    output = "www/styles/hfv-theme.css",
    options = sass_options(
      output_style = "compressed",
      source_map_embed = FALSE
    )
  )
  
  message("✅ HFV styles pre-compiled! CSS file ready for deployment.")
}

# =============================================================================
# HFV THEME SETUP
# =============================================================================

# HFV Color Palette
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

# =============================================================================
# DATA LOADING (Your existing data loading code)
# =============================================================================

# For this example, we'll create some sample data
# Replace this with your actual data loading
create_sample_data <- function() {
  # Sample building permits data
  years <- 2018:2023
  types <- c("1-unit", "2-4 units", "5+ units")
  
  expand_grid(
    year = years,
    type = types
  ) %>%
    mutate(
      units = case_when(
        type == "1-unit" ~ rpois(n(), lambda = 5000),
        type == "2-4 units" ~ rpois(n(), lambda = 2000),
        type == "5+ units" ~ rpois(n(), lambda = 1500)
      ),
      bldgs = case_when(
        type == "1-unit" ~ units,
        type == "2-4 units" ~ round(units / 2.5),
        type == "5+ units" ~ round(units / 20)
      ),
      value = units * case_when(
        type == "1-unit" ~ 350000,
        type == "2-4 units" ~ 280000, 
        type == "5+ units" ~ 200000
      )
    )
}

# Load data
permits_data <- create_sample_data()

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- page_fillable(
  # Apply HFV theme
  theme = hfv_theme,
  
  # Head section with styles and meta tags
  tags$head(
    # Mobile viewport
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    ),
    
    # Include HFV CSS if it exists, otherwise use fallback
    if (compile_hfv_styles_if_needed()) {
      tags$link(rel = "stylesheet", href = "styles/hfv-theme.css")
    } else {
      # Fallback: minimal inline CSS with HFV colors
      tags$style(HTML(paste0("
        .hfv-container { 
          max-width: 1200px; 
          margin: 0 auto; 
          padding: 24px; 
        }
        .hfv-header { 
          display: flex; 
          align-items: center; 
          margin-bottom: 24px; 
          padding-bottom: 8px; 
          border-bottom: 2px solid ", hfv_colors$sky, "; 
        }
        .hfv-logo { 
          height: 30px; 
          margin-right: 16px; 
        }
        .hfv-title { 
          margin: 0; 
          color: ", hfv_colors$shadow, "; 
          font-family: 'Poppins', sans-serif; 
        }
        .hfv-sidebar { 
          background-color: #E8EDF2; 
          padding: 16px; 
          border-radius: 6px; 
          box-shadow: 0 2px 4px rgba(0,0,0,0.1); 
        }
      ")))
    }
  ),
  
  # Main container using HFV classes
  div(
    class = "hfv-container",
    
    # Header using HFV styling
    div(
      class = "hfv-header",
      img(
        src = "https://housingforwardva.org/wp-content/uploads/2025/05/HousingForward-VA-Logo-Files-Icon-One-Color-RGB.png",
        alt = "HousingForward VA Logo",
        class = "hfv-logo"
      ),
      h4("Virginia Building Permits Dashboard", class = "hfv-title")
    ),
    
    # Layout using bslib layout_columns
    layout_columns(
      col_widths = c(3, 9),
      gap = "16px",
      
      # Sidebar Panel with HFV styling
      div(
        class = "hfv-sidebar",
        
        h5("Dashboard Controls", 
           style = paste0("color: ", hfv_colors$shadow, "; margin-bottom: 16px;")),
        
        # Metric selection
        selectInput(
          "metric",
          "Select Metric:",
          choices = list(
            "Housing Units" = "units",
            "Buildings" = "bldgs",
            "Total Value ($)" = "value"
          ),
          selected = "units"
        ),
        
        # Building type filter
        checkboxGroupInput(
          "building_types",
          "Building Types:",
          choices = list(
            "Single-family (1-unit)" = "1-unit",
            "Small multi-family (2-4 units)" = "2-4 units", 
            "Large multi-family (5+ units)" = "5+ units"
          ),
          selected = c("1-unit", "2-4 units", "5+ units")
        ),
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "U.S. Census Bureau Building Permits Survey", br(),
            "Sample data for demonstration purposes",
            style = "margin-bottom: 0;"
          )
        )
      ),
      
      # Main Panel with tabs
      div(
        navset_tab(
          id = "main_tabs",
          
          nav_panel(
            title = "Trends Over Time",
            value = "trends",
            div(
              style = "height: 450px; margin-top: 16px;",
              plotlyOutput("trend_plot", height = "100%")
            )
          ),
          
          nav_panel(
            title = "Current Year Breakdown", 
            value = "breakdown",
            div(
              style = "height: 450px; margin-top: 16px;",
              plotlyOutput("breakdown_plot", height = "100%")
            )
          ),
          
          nav_panel(
            title = "Data Summary",
            value = "summary", 
            div(
              style = "margin-top: 16px;",
              DT::dataTableOutput("summary_table")
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    req(input$building_types, input$metric)
    
    permits_data %>%
      filter(type %in% input$building_types)
  })
  
  # Trend plot
  output$trend_plot <- renderPlotly({
    req(filtered_data())
    
    plot_data <- filtered_data() %>%
      group_by(year, type) %>%
      summarise(
        value = sum(.data[[input$metric]], na.rm = TRUE),
        .groups = "drop"
      )
    
    # Create color mapping
    color_map <- c(
      "1-unit" = hfv_colors$sky,
      "2-4 units" = hfv_colors$shadow,
      "5+ units" = hfv_colors$lilac
    )
    
    # Create plot
    p <- ggplot(plot_data, aes(x = year, y = value, fill = type)) +
      geom_col(position = "stack", alpha = 0.9) +
      scale_fill_manual(values = color_map, name = "Building Type") +
      scale_x_continuous(breaks = unique(plot_data$year)) +
      scale_y_continuous(
        labels = if (input$metric == "value") {
          scales::dollar_format(scale = 1e-6, suffix = "M")
        } else {
          scales::comma_format()
        }
      ) +
      labs(
        title = paste("Virginia Building Permits:", 
                     case_when(
                       input$metric == "units" ~ "Housing Units",
                       input$metric == "bldgs" ~ "Buildings", 
                       input$metric == "value" ~ "Total Value"
                     )),
        subtitle = "Stacked by building type",
        x = "Year",
        y = case_when(
          input$metric == "units" ~ "Housing Units",
          input$metric == "bldgs" ~ "Buildings",
          input$metric == "value" ~ "Value (Millions $)"
        )
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(
          color = hfv_colors$shadow,
          family = "Poppins",
          face = "bold"
        ),
        plot.subtitle = element_text(
          color = "#6c757d",
          family = "Open Sans"
        )
      )
    
    # Convert to plotly
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.1
        ),
        margin = list(t = 60, b = 80)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Breakdown plot
  output$breakdown_plot <- renderPlotly({
    req(filtered_data())
    
    current_year_data <- filtered_data() %>%
      filter(year == max(year)) %>%
      mutate(
        metric_value = .data[[input$metric]],
        percentage = metric_value / sum(metric_value) * 100
      )
    
    # Create donut chart
    p <- plot_ly(
      current_year_data,
      labels = ~type,
      values = ~metric_value,
      type = 'pie',
      hole = 0.4,
      marker = list(
        colors = c(hfv_colors$sky, hfv_colors$shadow, hfv_colors$lilac),
        line = list(color = '#FFFFFF', width = 2)
      ),
      textinfo = 'label+percent',
      textposition = 'outside',
      hovertemplate = paste(
        '<b>%{label}</b><br>',
        '%{value:,}<br>',
        '%{percent}<br>',
        '<extra></extra>'
      )
    ) %>%
      layout(
        title = list(
          text = paste(max(current_year_data$year), "Building Permits Distribution"),
          font = list(
            family = "Poppins",
            size = 18,
            color = hfv_colors$shadow
          )
        ),
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.1
        ),
        margin = list(t = 60, b = 80)
      ) %>%
      config(displayModeBar = FALSE)
    
    p
  })
  
  # Summary table
  output$summary_table <- DT::renderDataTable({
    req(filtered_data())
    
    summary_data <- filtered_data() %>%
      group_by(type) %>%
      summarise(
        `Total Units` = scales::comma(sum(units)),
        `Total Buildings` = scales::comma(sum(bldgs)),
        `Total Value` = scales::dollar(sum(value)),
        `Avg Units/Year` = scales::comma(round(mean(units))),
        `Avg Value/Unit` = scales::dollar(round(mean(value/units))),
        .groups = "drop"
      ) %>%
      rename(`Building Type` = type)
    
    DT::datatable(
      summary_data,
      options = list(
        pageLength = 10,
        dom = 't',
        columnDefs = list(
          list(className = 'dt-center', targets = 1:5)
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        columns = 1:6,
        backgroundColor = "#f8f9fa",
        fontFamily = "Open Sans"
      )
  })
}

# =============================================================================
# RUN THE APP
# =============================================================================

shinyApp(ui = ui, server = server)

# =============================================================================
# DEPLOYMENT NOTES FOR POSIT CONNECT CLOUD
# =============================================================================

# REQUIRED FILES TO DEPLOY WITH YOUR APP:
# 
# 1. app.R (this file)
# 2. www/styles/variables.scss
# 3. www/styles/components.scss  
# 4. www/styles/responsive.scss
# 5. www/styles/hfv-theme.scss
# 
# OPTIONAL (if using pre-compiled approach):
# 6. www/styles/hfv-theme.css
#
# DIRECTORY STRUCTURE FOR DEPLOYMENT:
# your-app/
# ├── app.R
# └── www/
#     └── styles/
#         ├── variables.scss
#         ├── components.scss
#         ├── responsive.scss
#         ├── hfv-theme.scss
#         └── hfv-theme.css (optional, auto-generated)
#
# The compile_hfv_styles_if_needed() function ensures that CSS is compiled
# on Posit Connect Cloud during app startup, so you don't need to manually
# manage the CSS file. The SCSS files travel with your app and get compiled
# automatically in the cloud environment.