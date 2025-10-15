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
library(mapgl)
library(rmapshaper)
library(sf)
library(plotly)

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

# Load and prepare data
pit <- read_csv("../../../data/pit_data_virginia_longer.csv") |> 
  select(coc_num = co_c_number, coc_name = co_c_name, year, category, value) |> 
  filter(category == "Total Sheltered Homeless" | category == "Total Unsheltered Homeless")

# Create a state only data frame that aggregates to that geographic-level
state_pit <- pit |> 
  group_by(year, category) |> 
  summarise(value = sum(value), .groups = "drop") |> 
  mutate(coc_num = "VA-000", coc_name = "Statewide", name = "Virginia")

# Create a Continuum of Care data frame that has all CoCs
coc_pit <- pit |> 
  mutate(name = case_when(
    coc_num == "VA-500" ~ "Greater Richmond CoC", 
    coc_num == "VA-501" ~ "Southeastern Virginia Homeless Coaltion", 
    coc_num == "VA-502" ~ "Blue Ridge Interagency Council on Homelessness", 
    coc_num == "VA-503" ~ "BEACH Community Partnership", 
    coc_num == "VA-504" ~ "Thomas Jefferson Area Coalition for the Homeless", 
    coc_num == "VA-505" ~ "Greater Virginia Peninsula Homelessness Consortium", 
    coc_num == "VA-507" ~ "Portsmouth Homeless Action Consortium", 
    coc_num == "VA-508" ~ "Central Virginia CoC", 
    coc_num == "VA-513" ~ "Western Virginia CoC", 
    coc_num == "VA-514" ~ "Fredericksburg Regional CoC", 
    coc_num == "VA-521" ~ "Virginia Balance of State", 
    coc_num == "VA-600" ~ "Arlington County CoC", 
    coc_num == "VA-601" ~ "Fairfax County Office to Prevent and End Homelessness", 
    coc_num == "VA-602" ~ "Loudoun County CoC", 
    coc_num == "VA-603" ~ "The Partnership to Prevent and End Homelessness in the City of Alexandria", 
    coc_num == "VA-604" ~ "Prince William Area CoC", 
    TRUE ~ coc_name
  ))

va_pit <- rbind(state_pit, coc_pit)

# Load geographic data
coc_geo <- sf::st_read("../../../data/geo/virginia_coc.gpkg") |> 
  ms_simplify() |> 
  mutate(coc_num = COCNUM)

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

# Define UI
ui <- page_fillable(
  theme = hfv_theme,
  useShinyjs(), # Initialize shinyjs

  # Main container using HFV classes
  div(
    class = "hfv-container",
    style = "width: 100%; height: 100vh; max-width: 800px; max-height: 500px; margin: 0 auto; padding: 10px; box-sizing: border-box; display: flex; flex-direction: column; overflow: hidden;",
    
    # Header using HFV styling
    div(
      class = "hfv-header",
      style = "display: flex; align-items: center; margin-bottom: 8px; border-bottom: 2px solid #40C0C0; padding-bottom: 5px; flex-shrink: 0;",
      h4("Virginia Point-in-Time Homelessness Count", class = "hfv-title", style = "margin: 0; color: #011E41; font-size: 14px; font-weight: bold;")
    ),

    # Layout using custom flex structure for this map-based app
    div(
      style = "display: flex; flex: 1; min-height: 0; gap: 10px;",
      
      # Sidebar Panel with HFV styling
      div(
        class = "hfv-sidebar",
        style = "background-color: #E8EDF2; padding: 8px; border-radius: 5px; width: 200px; flex-shrink: 0; font-size: 11px; overflow-y: auto;",
        
        h5("Dashboard Controls", 
           class = "text-primary", style = "margin-bottom: 8px; font-size: 12px; font-weight: bold;"),
        
        h5("Selected CoC", style = "margin-bottom: 5px; font-size: 12px; font-weight: bold;"),
        textOutput("selected_coc"),
        hr(style = "margin: 8px 0;"),
        actionButton("reset_btn", "Reset Selection", 
                    style = "width: 100%; font-size: 10px; padding: 3px;",
                    class = "btn-outline-primary btn-sm"),
        hr(style = "margin: 8px 0;"),
        
        h5("About", style = "margin-bottom: 5px; font-size: 12px; font-weight: bold;"),
        p("Click on any Continuum of Care region to see homelessness counts by category.", 
          style = "margin-bottom: 5px; line-height: 1.3;"),
        
        # Data source
        div(
          style = "font-size: 9px; color: #666; margin-top: 5px;",
          p(
            strong("Data Source:"), br(),
            "HUD Point-in-Time Count",
            style = "margin-bottom: 0;"
          )
        )
      ),
        
      # Main content area with map and plot
      div(
        style = "flex: 1; display: flex; flex-direction: column; min-height: 0; gap: 8px;",
        
        # Map Section
        div(
          style = "flex: 1; min-height: 0; display: flex; flex-direction: column;",
          h5("Continuum of Care Regions", style = "margin: 0 0 5px 0; font-size: 12px; font-weight: bold; flex-shrink: 0;"),
          div(
            class = "hfv-chart-container",
            style = "flex: 1; min-height: 0; cursor: default !important; border-radius: 3px; overflow: hidden;",
            maplibreOutput("map_id", height = "100%")
          )
        ),
        
        # Plot Section
        div(
          style = "flex: 1; min-height: 0; display: flex; flex-direction: column;",
          h5("Homelessness Count by Category", style = "margin: 0 0 5px 0; font-size: 12px; font-weight: bold; flex-shrink: 0;"),
          div(
            class = "hfv-chart-container",
            style = "flex: 1; min-height: 0; border-radius: 3px; overflow: hidden;",
            plotlyOutput("bar_chart", height = "100%")
          )
        )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Store the selected CoC
  selected_coc <- reactiveVal("VA-000")  # Default to statewide
  
  # Track if a feature was just clicked to prevent immediate reset
  feature_clicked <- reactiveVal(FALSE)
  
  # Render the map - CORRECTED VERSION
  output$map_id <- renderMaplibre({
    maplibre(
      bounds = coc_geo
    ) %>%
      add_fill_layer(
        id = "coc_layer",
        source = coc_geo,
        fill_color = hfv_colors$sky,
        fill_opacity = 0.5,
        fill_outline_color = "white",
        hover_options = list(
          fill_opacity = 0.75
        )
      ) %>%
      add_navigation_control(pos = "top-right") %>%
      add_fullscreen_control(pos = "top-right")
  })
  
  # Handle map clicks on features - FIXED VERSION
  observeEvent(input$map_id_feature_click, {
    click_info <- input$map_id_feature_click
    message("Feature click detected: ", jsonlite::toJSON(click_info, auto_unbox = TRUE))
    
    # Set flag that feature was clicked
    feature_clicked(TRUE)
    
    if (!is.null(click_info) && !is.null(click_info$properties)) {
      coc_num <- click_info$properties$coc_num %||% click_info$properties$COCNUM
      message("Extracted CoC number: ", coc_num)
      if (!is.null(coc_num)) {
        selected_coc(coc_num)
      }
    }
    
    # Reset the flag after a short delay
    invalidateLater(100, session)
    observe({
      feature_clicked(FALSE)
    })
  })
  
  # Handle map clicks outside polygons (reset) - FIXED VERSION
  observeEvent(input$map_id_click, {
    click_info <- input$map_id_click
    message("Map click detected: ", jsonlite::toJSON(click_info, auto_unbox = TRUE))
    
    # Only reset if no feature was recently clicked
    if (!feature_clicked()) {
      message("Resetting to statewide")
      selected_coc("VA-000")
    } else {
      message("Feature was just clicked, not resetting")
    }
  })
  
  # Handle reset button
  observeEvent(input$reset_btn, {
    selected_coc("VA-000")
  })
  
  # Output selected CoC name
  output$selected_coc <- renderText({
    coc_num <- selected_coc()
    coc_data <- va_pit %>% 
      filter(coc_num == !!coc_num) %>% 
      distinct(name) %>% 
      pull(name)
    
    if (length(coc_data) > 0) {
      paste("Selected:", coc_data[1])
    } else {
      "No CoC selected"
    }
  })
  
  # Create interactive bar chart using plotly
  output$bar_chart <- renderPlotly({
    coc_num <- selected_coc()
    
    # Filter data for selected CoC
    chart_data <- va_pit %>%
      filter(coc_num == !!coc_num) %>%
      arrange(year, category)
    
    if (nrow(chart_data) == 0) {
      # Create empty plot
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                label = "No data available for selected CoC", 
                size = 4) +
        theme_void()
      
      ggplotly(p) %>%
        config(displayModeBar = FALSE)
    } else {
      # Create stacked bar chart
      p <- ggplot(chart_data, 
             aes(x = year, y = value, fill = category,
                 text = paste0(category, "\nYear: ", year, "\nCount: ", scales::comma(value)))) +
        geom_col(position = "stack") +
        scale_fill_manual(
          values = c(
            "Total Sheltered Homeless" = hfv_colors$sky,
            "Total Unsheltered Homeless" = hfv_colors$desert
          )
        ) +
        scale_x_continuous(breaks = unique(chart_data$year)) +
        scale_y_continuous(labels = scales::comma_format()) +
        labs(
          title = paste("Homelessness Count:", chart_data$name[1]),
          x = "Year",
          y = "Count",
          fill = "Category"
        ) +
        theme_minimal(base_family = "Open Sans") +
        theme(
          legend.position = "none",
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = 9),
          plot.margin = margin(1, 1, 1, 1, "pt"),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          axis.text = element_text(size = 7),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(0, 0, 0, 0)
        )
      
      # Convert to plotly and customize
      ggplotly(p, tooltip = "text") %>%
        config(displayModeBar = FALSE) %>%
        layout(
          margin = list(l = 10, r = 10, t = 30, b = 30),
          font = list(size = 8)
        )
    }
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application
shinyApp(ui = ui, server = server)