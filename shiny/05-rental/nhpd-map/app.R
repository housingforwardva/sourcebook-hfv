library(shiny)
library(tidyverse)
library(readxl)
library(stringr)
library(mapgl)
library(sf)
library(air)
library(here)
library(bslib)
library(shinyjs)

# =============================================================================
# NATIONAL HOUSING PRESERVATION DATABASE MAP
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

# =============================================================================
# LOAD DATA OUTSIDE SERVER
# =============================================================================

# Load jurisdiction shapefile
juris <- read_rds("../../../data/va_co_shape.rds")



# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- page_fillable(
  theme = hfv_theme,
  includeCSS("www/styles/hfv-theme.css"),
  useShinyjs(),

  div(
    class = "hfv-container",

    div(
      class = "hfv-header",
      h4("National Housing Preservation Database", class = "hfv-title")
    ),

    layout_columns(
      col_widths = c(
        lg = c(3, 9),
        md = c(4, 8),
        sm = 12
      ),
      gap = "16px",

      div( 
        class = "hfv-sidebar",
        h5("Information",
          class = "text-primary", style = "margin-bottom: 16px;"),

        div(
          style = "margin-bottom: 16px;",
          p("This map shows federally subsidized rental housing properties in Virginia from the National Housing Preservation Database.",
            style = "font-size: 0.9rem; line-height: 1.4; margin-bottom: 12px;"),
          p("Click on any green dot to view property details including subsidies, units, and status.",
            style = "font-size: 0.85rem; color: #6c757d; line-height: 1.4;")
        ),
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "National Housing Preservation Database",
            style = "margin-bottom: 0;"
          )
        )
      ),
        
      # Main Panel with map
      div(
        class = "hfv-chart-container",
        style = "height: 600px; margin-top: 16px;",
        mapboxglOutput("nhpd_map", height = "100%")
      )
    )
  )
)

# =============================================================================
# SERVER FUNCTION
# =============================================================================
server <- function(input, output, session) {
  
  # Render the NHPD map
  output$nhpd_map <- renderMapboxgl({
    mapboxgl(bounds = juris) |> 
      add_fill_layer(
        id = "juris",
        source = juris,
        fill_opacity = 0.1,
        fill_outline_color = "blue",
        hover_options = list(
            fill_color = "#1B365D",
            fill_opacity = 0.8
          )) |> 
      add_circle_layer(
        id = "properties",
        source = nhpd,
        circle_color = "green",
        circle_radius = 2,
        circle_opacity = 0.8,
        popup = concat(
          '<div style="background: linear-gradient(135deg, #011E41 0%, #66788d 100%); ',
          'padding: 12px; border-radius: 8px; box-shadow: 0 5px 15px rgba(0,0,0,0.3); ',
          'color: white; font-family: -apple-system, BlinkMacSystemFont, sans-serif; ',
          'max-width: 280px; position: relative;">',

          # Property name with housing icon
          '<h3 style="margin: 0 0 10px 0; font-size: 16px; font-weight: 600; ',
          'display: flex; align-items: center; gap: 6px;">',
          '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">',
          '<path d="M3 9l9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"></path>',
          '<polyline points="9,22 9,12 15,12 15,22"></polyline></svg>',
          get_column("property_name"),
          '</h3>',

          # Address
          '<div style="font-size: 12px; opacity: 0.9; margin-bottom: 10px;">',
          get_column("property_address"), '<br>',
          get_column("city"), ', ', get_column("state"), ' ', get_column("zip_code"),
          '</div>',

          # Status badge
          '<div style="background: rgba(255,255,255,0.2); padding: 6px 8px; border-radius: 4px; ',
          'margin-bottom: 10px; font-size: 11px; font-weight: 600; text-align: center;">',
          'Status: ', get_column("property_status"),
          '</div>',

          # Stats grid - 3 columns
          '<div style="display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 6px; margin-bottom: 10px;">',

          # Max assisted units card
          '<div style="background: rgba(255,255,255,0.15); padding: 6px; border-radius: 4px; text-align: center;">',
          '<div style="font-size: 10px; opacity: 0.8; margin-bottom: 2px;">Max Assisted</div>',
          '<div style="font-size: 14px; font-weight: 600;">',
          get_column("max_assisted_units"),
          '</div>',
          '</div>',

          # Total units card
          '<div style="background: rgba(255,255,255,0.15); padding: 6px; border-radius: 4px; text-align: center;">',
          '<div style="font-size: 10px; opacity: 0.8; margin-bottom: 2px;">Total Units</div>',
          '<div style="font-size: 14px; font-weight: 600;">',
          get_column("total_units"),
          '</div>',
          '</div>',

          # Subsidies count card
          '<div style="background: rgba(255,255,255,0.15); padding: 6px; border-radius: 4px; text-align: center;">',
          '<div style="font-size: 10px; opacity: 0.8; margin-bottom: 2px;">Subsidies</div>',
          '<div style="font-size: 14px; font-weight: 600;">',
          get_column("num_subsidies"),
          '</div>',
          '</div>',
          '</div>',

          # Active subsidies - condensed
          '<div style="margin-bottom: 8px;">',
          '<div style="font-size: 11px; font-weight: 600; margin-bottom: 4px;">Active Subsidies:</div>',
          '<div style="font-size: 10px; background: rgba(255,255,255,0.1); padding: 4px; border-radius: 4px;">',
          get_column("active_subsidies"),
          '</div>',
          '</div>',

          # Data source footer
          '<div style="font-size: 9px; opacity: 0.7; text-align: center; ',
          'padding-top: 6px; border-top: 1px solid rgba(255,255,255,0.2);">',
          'NHPD',
          '</div>',

          '</div>'
        ))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)