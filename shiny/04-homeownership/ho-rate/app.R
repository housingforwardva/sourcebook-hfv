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
library(mapgl)
library(tigris)
library(sf)
library(tidycensus)

# =============================================================================
# Virginia Homeownership Explorer Visualization
# =============================================================================
gdtools::register_gfont(family = "Open Sans")

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

# Define HFV color palette (matching SCSS variables)
hfv_colors <- list(
  sky = "#40C0C0",           # Primary teal
  grass = "#259591",         # Dark teal 
  lilac = "#8B85CA",         # Purple
  shadow = "#011E41",        # Dark navy
  shadow_light = "#102C54",  # Lighter navy
  berry = "#B1005F",         # Magenta
  desert = "#E0592A"         # Orange
)

# =============================================================================
# LOAD DATA OUTSIDE SERVER
# ============================================================================= 
# Load the data (only once)
va_counties <- readRDS("va_co_shape.rds")
tract_map_data <- readRDS("tract_data_simplified.rds")
trend_data <- readRDS("trend_data.rds")

# =============================================================================
# USER INTERFACE
# ============================================================================= 

ui <- page_fillable(
  theme = hfv_theme,
  includeCSS("www/styles/hfv-theme.css"),  # Add custom theme css
  useShinyjs(), # Initialize shinyjs

  # Add the viewport meta tag for mobile devices
  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    )
  ),

  # Main container with responsive padding
  div(
    class = "hfv-container",

    # Header with logo and title
    div(
      class = "hfv-header",
      h4("Virginia Homeownership Explorer", class = "hfv-title")
    ),

    # Responsive grid layout
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
        h5("Selected Location", class = "hfv-sidebar__title"),
        textOutput("selected_tract"),
        textOutput("selected_county"),
        hr(class = "hfv-sidebar__divider"),
        h5("About", class = "hfv-sidebar__title"),
        p("Click on any census tract to see historical homeownership rates."),
        hr(class = "hfv-sidebar__divider"),
        div(
          class = "hfv-sidebar__source",
          p("Data source: US Census ACS 5-year estimates, 2010-2023")
        )
      ),
      
      # Main Panel
      div(
        style = "width: 100%;",
        
        # Map Section
        div(
          style = "margin-bottom: 20px;",
          h5("Homeownership Rate by Census Tract", class = "hfv-sidebar__title"),
          div(
            class = "hfv-chart-container hfv-chart-container--map",
            # Show loading message during render
            div(
              id = "loading-content",
              class = "hfv-chart-loading",
              div(
                style = "background-color: rgba(255, 255, 255, 0.8); padding: 20px; border-radius: 5px; text-align: center;",
                h4("Loading map data..."),
                div(class = "hfv-spinner")
              )
            ),
            maplibreOutput("map_id", height = "100%")
          )
        ),
        
        # Plot Section
        div(
          h5("Homeownership Rate Over Time", class = "hfv-sidebar__title"),
          div(
            class = "hfv-chart-container hfv-chart-container--plot",
            girafeOutput("ho_trend_plot", height = "100%")
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

# =============================================================================
# SERVER FUNCTION 
# =============================================================================

server <- function(input, output, session) {
  
  # Hide loading indicator when data is ready
  observe({
    session$sendCustomMessage(type = 'hideLoading', message = list())
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
  
  # Store the selected tract 
  selected_data <- reactiveVal(NULL)
  
  # Debug helper - print unique jurisdiction values in trend_data
  observe({
    message("Unique jurisdiction values in trend_data: ")
    td <- trend_data
    jurisdictions <- unique(td$jurisdiction[td$geography == "Jurisdiction"])
    message(paste(jurisdictions, collapse = ", "))
  })
  
  # Render the map
  output$map_id <- renderMaplibre({
    # Use pre-loaded data
    
    # Create map object
    m <- maplibre(
      style = mapgl::carto_style("positron"),
      bounds = tract_map_data
    ) 
    
    # First add tract layer
    m <- m %>% add_fill_layer(
      id = "tract_data",  
      source = tract_map_data,
      # Viridis palette (colorblind-friendly)
      fill_color = interpolate(
        column = "ho_rate",
        values = c(0, 20, 40, 60, 80, 100),
        # Cool blues/greens to warm oranges/reds
        stops = c("#011E41", "#259591", "#40C0C0", "#FFC658", "#E0592A", "#FF7276"),
        na_color = "grey"
      ),
      fill_opacity = 0.8,
      tooltip = "custom_tooltip" # Use custom tooltip for better info display
    )
    
    # Then add county boundaries, but specify that they should appear above the tract layer
    m <- m %>% add_line_layer(
      id = "county_lines",
      source = va_counties,
      line_color = "lightgrey",
      line_width = 1.5  # Made this slightly thicker for better visibility
    )
    
    # Add legend and other elements
    m <- m %>% add_legend(
      "Homeownership Rate in Virginia (%)",
      values = c(0, 20, 40, 60, 80, 100),
      colors = c("#011E41", "#259591", "#40C0C0", "#FFC658", "#E0592A", "#FF7276")
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
      
      # Use pre-loaded data
      td <- trend_data
      
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
  
  # Create plot with ggiraph instead of plotly
  output$ho_trend_plot <- renderGirafe({
    data <- selected_data()
    
    if (is.null(data)) {
      # Return an empty plot with a message
      empty_plot <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Click on a tract to see historical data", 
                 size = 4) +
        theme_void()
      
      girafe(ggobj = empty_plot,
             width_svg = 8,
             height_svg = 4)
    } else {
      plot_data <- data$trend_data
      
      # Check if we have valid data
      if (!is.null(plot_data) && nrow(plot_data) > 0) {
        # Create interactive ggplot with ggiraph
        p <- ggplot(plot_data, 
                    aes(x = year, 
                        y = ho_rate, 
                        color = level,
                        group = level)) +
          geom_line_interactive(
            aes(tooltip = paste0(level, "\n",
                                 "Year: ", year, "\n",
                                 "Rate: ", round(ho_rate, 1), "%"),
                data_id = level),
            size = 1, 
            na.rm = TRUE
          ) +
          geom_point_interactive(
            aes(tooltip = paste0(level, "\n",
                                 "Year: ", year, "\n", 
                                 "Rate: ", round(ho_rate, 1), "%"),
                data_id = level),
            size = 3, 
            na.rm = TRUE
          ) +
          scale_color_manual(
            values = c("Census Tract" = "#011E41", 
                       "Jurisdiction" = "#8B85CA", 
                       "Virginia" = "#40C0C0"),
            name = ""
          ) +
          labs(
            title = paste("Homeownership Rate for", data$county_name, "-", data$tract_name),
            x = NULL,
            y = NULL
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
          scale_y_continuous(labels = scales::percent_format(scale = 1), 
                             limits = c(20, 100))
        
        # Convert to interactive plot with ggiraph
        girafe(
          ggobj = p,
          width_svg = 8,
          height_svg = 4,
          options = list(
            opts_hover(css = "stroke-width:2; opacity:1;"),
            opts_hover_inv(css = "opacity:0.4;"),
            opts_tooltip(
              css = "background-color:white; padding:5px; border-radius:3px; border:1px solid #ccc; font-size:12px;",
              use_fill = FALSE
            ),
        opts_sizing(rescale = TRUE),
        opts_toolbar(hidden = c("lasso_select", "lasso_deselect"))
      ),
      fonts = list(
        addGFontHtmlDependency(family = "Open Sans"),
        addGFontHtmlDependency(family = "Poppins")
          )
        )
      } else {
        # No valid data available for the plot
        no_data_plot <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("No historical data available for", 
                                 data$county_name, "-", data$tract_name), 
                   size = 4) +
          theme_void()
        
        girafe(ggobj = no_data_plot,
               width_svg = 8,
               height_svg = 4)
      }
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)