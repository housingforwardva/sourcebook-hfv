library(tidyverse)
library(mapgl)
library(rmapshaper)
library(sf)
library(shiny)
library(bslib)
library(plotly)
library(systemfonts)
library(shinyjs)

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

  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    )
  ),

  tags$head(
    tags$style(HTML(
      "
      body, html {
        margin: 0;
        padding: 0;
        height: 100vh;
        overflow: hidden;
        font-family: 'Open Sans', sans-serif;
      }
      
      .hfv-container {
        width: 100%;
        height: 100vh;
        max-width: 800px;
        max-height: 500px;
        margin: 0 auto;
        padding: 10px;
        box-sizing: border-box;
        display: flex;
        flex-direction: column;
        overflow: hidden;
      }
      
      .hfv-header {
        display: flex; 
        align-items: center; 
        margin-bottom: 8px; 
        border-bottom: 2px solid #40C0C0; 
        padding-bottom: 5px;
        flex-shrink: 0;
      }
      
      .hfv-header img {
        height: 20px;
        margin-right: 8px;
      }
      
      .title-text {
        margin: 0; 
        color: #011E41;
        font-size: 14px;
        font-weight: bold;
      }
      
      .main-content {
        display: flex;
        flex: 1;
        min-height: 0;
        gap: 10px;
      }
      
      .hfv-sidebar {
        background-color: #E8EDF2;
        padding: 8px;
        border-radius: 5px;
        width: 200px;
        flex-shrink: 0;
        font-size: 11px;
        overflow-y: auto;
      }
      
      .hfv-sidebar h5 {
        margin-bottom: 5px;
        font-size: 12px;
        font-weight: bold;
      }
      
      .hfv-sidebar p {
        margin-bottom: 5px;
        line-height: 1.3;
      }
      
      .hfv-sidebar hr {
        margin: 8px 0;
      }
      
      .content-area {
        flex: 1;
        display: flex;
        flex-direction: column;
        min-height: 0;
        gap: 8px;
      }
      
      .map-section {
        flex: 1;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }
      
      .plot-section {
        flex: 1;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }
      
      .section-title {
        margin: 0 0 5px 0;
        font-size: 12px;
        font-weight: bold;
        flex-shrink: 0;
      }
      
      .map-container {
        flex: 1;
        min-height: 0;
        cursor: default !important;
        border-radius: 3px;
        overflow: hidden;
      }
      
      .map-container * {
        cursor: default !important;
      }
      
      .plot-container {
        flex: 1;
        min-height: 0;
        border-radius: 3px;
        overflow: hidden;
      }
      
      /* Mobile responsive - stack vertically on small screens */
      @media (max-width: 768px) {
        .hfv-container {
          max-width: 100vw;
          max-height: 100vh;
          padding: 5px;
        }
        
        .main-content {
          flex-direction: column;
          gap: 5px;
        }
        
        .hfv-sidebar {
          width: 100%;
          padding: 5px;
          order: 3;
        }
        
        .content-area {
          gap: 5px;
        }
        
        .title-text {
          font-size: 12px;
        }
        
        .hfv-header img {
          height: 16px;
        }
      }
      
      /* Very small screens */
      @media (max-width: 480px) {
        .hfv-container {
          padding: 3px;
        }
        
        .main-content {
          gap: 3px;
        }
        
        .content-area {
          gap: 3px;
        }
        
        .hfv-sidebar {
          padding: 3px;
        }
        
        .title-text {
          font-size: 10px;
        }
        
        .section-title {
          font-size: 10px;
        }
      }
    "
    ))
  ),

  div(
    class = "hfv-container",

    div(
      class = "hfv-header",
      img(
        src = "https://housingforwardva.org/wp-content/uploads/2025/05/HousingForward-VA-Logo-Files-Icon-One-Color-RGB.png",
        alt = "HousingForward VA Logo"
      ),
      h4("Virginia Point-in-Time Homelessness Count", class = "title-text")
    ),

    div(
      class = "main-content",
      
      # Sidebar Panel
      div(
        class = "hfv-sidebar",
        h5("Selected CoC"),
        textOutput("selected_coc"),
        tags$hr(),
        actionButton("reset_btn", "Reset Selection", 
                    style = "width: 100%; font-size: 10px; padding: 3px;",
                    class = "btn-outline-primary btn-sm"),
        tags$hr(),
        h5("About"),
        p("Click on any Continuum of Care region to see homelessness counts by category."),
        div(
          style = "font-size: 9px; color: #666; margin-top: 5px;",
          p("Data source: HUD Point-in-Time Count")
        )
      ),

      # Main content area with map and plot
      div(
        class = "content-area",
        
        # Map Section
        div(
          class = "map-section",
          h5("Continuum of Care Regions", class = "section-title"),
          div(
            class = "map-container",
            maplibreOutput("map_id", height = "100%")
          )
        ),
        
        # Plot Section
        div(
          class = "plot-section",
          h5("Homelessness Count by Category", class = "section-title"),
          div(
            class = "plot-container",
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
        theme_minimal() +
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
}

# Run the application
shinyApp(ui = ui, server = server)