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
library(tigris)
library(sf)
library(tidycensus)
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

# UI for the Shiny app
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
      
      /* Map container styles */
      .map-container {
        width: 100%;
        height: 450px;
      }
      
      /* Plot container styles */
      .plot-container {
        width: 100%;
        height: 350px;
      }
      
      /* MOBILE OPTIMIZATION #3: Medium-sized screens (tablets, smaller laptops) */
      @media (max-width: 992px) {
        .hfv-container {
          padding: 10px;
        }
        
        .title-text {
          font-size: 18px;
        }
        
        .map-container {
          height: 400px;
        }
        
        .plot-container {
          height: 300px;
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
        
        .map-container {
          height: 350px;
        }
        
        .plot-container {
          height: 250px;
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
        
        .map-container {
          height: 300px;
        }
        
        .plot-container {
          height: 200px;
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
      h4("Virginia Homeownership Explorer", class = "title-text")
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
        h5("Selected Location", style = "margin-bottom: 10px; font-weight: bold;"),
        textOutput("selected_tract"),
        textOutput("selected_county"),
        hr(style = "margin: 15px 0;"),
        h5("About", style = "margin-bottom: 10px; font-weight: bold;"),
        p("Click on any census tract to see historical homeownership rates."),
        hr(style = "margin: 15px 0;"),
        div(
          style = "font-size: 10px; color: #666; margin-top: 8px;",
          p("Data source: US Census ACS 5-year estimates, 2010-2023")
        )
      ),
      # Main Panel
      div(
        style = "width: 100%;",
        
        # Map Section
        div(
          style = "margin-bottom: 20px;",
          h5("Homeownership Rate by Census Tract", style = "margin-bottom: 10px; font-weight: bold;"),
          div(
            class = "map-container",
            # Show loading message during render
            div(
              id = "loading-content",
              style = "position: absolute; width: 100%; height: 100%; display: flex; justify-content: center; align-items: center; z-index: 1000;",
              div(
                style = "background-color: rgba(255, 255, 255, 0.8); padding: 20px; border-radius: 5px; text-align: center;",
                h4("Loading map data..."),
                span(class = "spinner-border", role = "status")
              )
            ),
            maplibreOutput("map_id", height = "100%")
          )
        ),
        
        # Plot Section
        div(
          h5("Homeownership Rate Over Time", style = "margin-bottom: 10px; font-weight: bold;"),
          div(
            class = "plot-container",
            plotlyOutput("ho_trend_plot", height = "100%")
          )
        )
      )
    )
  ),
  # Add JavaScript to handle loading indicator
  tags$script(HTML("
    Shiny.addCustomMessageHandler('hideLoading', function(message) {
      document.getElementById('loading-content').style.display = 'none';
    });
  "))
)

# Server function
server <- function(input, output, session) {
  
  # Implement lazy loading with reactiveVal
  tract_map_data <- reactiveVal(NULL)
  va_counties <- reactiveVal(NULL)
  
  # Load data in a separate reactive process to avoid blocking UI
  observe({
    # Load counties first (smaller file)
    va_counties(readRDS("va_co_shape.rds"))
    
    # Then load tract data
    withProgress(message = 'Loading map data...', value = 0, {
      tract_map_data(readRDS("tract_data_simplified.rds"))
      incProgress(1)
    })
    
    # Hide loading indicator when data is ready
    session$sendCustomMessage(type = 'hideLoading', message = list())
  })

  # MOBILE OPTIMIZATION #8: Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
  
  # Store the selected tract 
  selected_data <- reactiveVal(NULL)

  # Lazy load trend data only when needed
  trend_data <- reactive({
    readRDS("trend_data.rds")
  })
  
  # Debug helper - print unique jurisdiction values in trend_data
  observe({
    message("Unique jurisdiction values in trend_data: ")
    td <- trend_data()
    jurisdictions <- unique(td$jurisdiction[td$geography == "Jurisdiction"])
    message(paste(jurisdictions, collapse = ", "))
  })
  
 # Render the map
output$map_id <- renderMaplibre({
  # Wait for data to be loaded
  req(tract_map_data(), va_counties())
  
  # Create map object
  m <- maplibre(
    style = mapgl::carto_style("positron"),
    bounds = tract_map_data()
  ) 
  
  # First add tract layer
  m <- m %>% add_fill_layer(
    id = "tract_data",  
    source = tract_map_data(),
    # Viridis palette (colorblind-friendly)
    fill_color = interpolate(
      column = "ho_rate",
      values = c(0, 20, 40, 60, 80, 100),
      stops = c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725"),
      na_color = "grey"
    ),
    fill_opacity = 0.8,
    tooltip = "custom_tooltip" # Use custom tooltip for better info display
  )
  
  # Then add county boundaries, but specify that they should appear above the tract layer
  m <- m %>% add_line_layer(
    id = "county_lines",
    source = va_counties(),
    line_color = "lightgrey",
    line_width = 1.5  # Made this slightly thicker for better visibility
  )
  
  # Add legend and other elements
  m <- m %>% add_legend(
    "Homeownership Rate in Virginia (%)",
    values = c(0, 20, 40, 60, 80, 100),
    colors = c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725")
  )
  
  # Add geocoder
  m <- m %>% add_geocoder_control(
    position = "top-right", 
    placeholder = "Enter an address"
  )
  
  # Return map
  return(m)
})
  
  # Handle tract clicks using the feature_click event 
  observeEvent(input$map_id_feature_click, {
    message("Tract click detected!")
    click_info <- input$map_id_feature_click
    
    # Debug info
    message("Click info structure: ", paste(capture.output(str(click_info)), collapse = "\n"))
    
    if (!is.null(click_info) && !is.null(click_info$properties)) {
      # Extract properties from the clicked feature
      properties <- click_info$properties
      
      # Extract GEOID from properties
      geoid <- properties$GEOID
      # Extract county name from properties
      county_name <- properties$jurisdiction
      tract <- properties$tract

      county_name <- trimws(county_name)
      
      message("Selected GEOID: ", geoid)
      message("Selected county name: ", county_name)
      
      # Load required data
      td <- trend_data()
      
      # Check if county name exists in trend_data
      county_exists <- county_name %in% td$jurisdiction[td$geography == "Jurisdiction"]
      message("County exists in trend_data: ", county_exists)
      
      if (!county_exists) {
        # Try to find the closest match
        message("Looking for similar county names...")
        similar_names <- td$jurisdiction[td$geography == "Jurisdiction"]
        for (name in similar_names) {
          message("Comparing '", county_name, "' with '", name, "'")
        }
      }
      
      # Get tract name or construct it
      tract_name <- tract
      
      # Get tract historical data
      tract_trend <- td %>%
        filter(geography == "Tract") %>%
        filter(GEOID == geoid) %>%
        select(year, geography, ho_rate) %>%
        mutate(level = "Census Tract")
      
      message("Found ", nrow(tract_trend), " rows for tract")
      
      # Get county historical data  
      county_trend <- td %>% 
        filter(geography == "Jurisdiction") %>% 
        filter(jurisdiction == county_name) %>% 
        select(year, geography, ho_rate) %>%
        mutate(level = "Jurisdiction")
      
      message("Found ", nrow(county_trend), " rows for county: ", county_name)
      
      # Get state historical data
      state_trend <- td %>% 
        filter(geography == "State") %>%
        select(year, geography, ho_rate) %>%
        mutate(level = "Virginia")
      
      message("Found ", nrow(state_trend), " rows for state")
      
      # Combine all data
      combined_trend <- bind_rows(
        tract_trend,
        county_trend,
        state_trend
      )
      
      message("Combined data has ", nrow(combined_trend), " rows")
      
      # Create a more complete data structure for your reactive value
      selected_data(list(
        geoid = geoid,
        county_name = county_name,
        tract_name = tract_name,
        trend_data = combined_trend
      ))
      
      message("Selected data updated successfully")
    } else {
      message("Click event doesn't have the expected properties")
    }
  })
  
  # Output tract name
  output$selected_tract <- renderText({
    data <- selected_data()
    if (is.null(data)) {
      "No tract selected"
    } else {
      paste("Selected Tract:", data$tract_name)
    }
  })
  
  # Output county name
  output$selected_county <- renderText({
    data <- selected_data()
    if (is.null(data)) {
      ""
    } else {
      paste("Selected Jurisdiction:", data$county_name)
    }
  })
  
  # Create plot with better structure
  output$ho_trend_plot <- renderPlotly({
    data <- selected_data()
    
    if (is.null(data)) {
      # Return an empty plot with a message using ggplot
      empty_plot <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Click on a tract to see historical data", size = 4) +
        theme_void()
      
      ggplotly(empty_plot) %>%
        layout(
          xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
        )
    } else {
      plot_data <- data$trend_data
      
      # Check if we have valid data
      if (!is.null(plot_data) && nrow(plot_data) > 0) {
        # Create plot with the combined data
        p <- ggplot(plot_data, 
          aes(
            x = year, 
            y = ho_rate, 
            color = level)) +
          geom_line(size = 1, na.rm = TRUE) +
          geom_point(size = 3, na.rm = TRUE) +
          scale_color_manual(
            values = c("Census Tract" = "#0066CC", "Jurisdiction" = "#FF6600", "Virginia" = "#009933"),
            name = ""
          ) +
          labs(
            title = paste("Homeownership Rate for", data$county_name, "-", data$tract_name)
          ) +
          theme_minimal(base_family = "Open Sans") +
          theme(
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.margin = margin(t = 10, b = 10),
            axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 14, face = "bold"),
            plot.title.position = "plot"
          ) +
          scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(20,100))
        
        # Convert ggplot to plotly - the simple solution
        ggplotly(p, tooltip = c("x", "y", "color")) %>%
          layout(
            autosize = TRUE,
            margin = list(l = 50, r = 50, b = 80, t = 75, pad = 4),
            legend = list(orientation = "h", y = -0.5, x = 0.5, xanchor = "center")
          )
      } else {
        # No valid data available for the plot
        no_data_plot <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("No historical data available for", data$county_name, "-", data$tract_name), 
                   size = 4) +
          theme_void()
        
        ggplotly(no_data_plot) %>%
          layout(
            xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
            yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
          )
      }
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)