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
library(lubridate)
library(zoo)
library(gfonts)

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

# Load data outside of server
hpi <- read_rds("hpi.rds") |> 
  mutate(date = as.Date(as.yearqtr(date, format = "%Y Q%q"))) |> 
  select(geography, name, date, hpi) |> 
  filter(!is.na(hpi))

# Create lists for filters
cbsa_list <- sort(unique(hpi$name[hpi$geography == "CBSA"]))

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
        h4("Housing Price Index Analysis", class = "hfv-title")
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

          # Tooltip info
          div(
            style = "margin-bottom: 16px; font-size: 0.8rem;",
            p("Hover over points to see details", style = "margin-bottom: 8px;"),
            verbatimTextOutput("hover_info", placeholder = TRUE)
          ),

          # Divider
          hr(style = "margin: 24px 0; border-color: #ced4da;"),

          # Data source
          div(
            style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
            p(
              strong("Data Source:"), br(),
              "Federal Housing Finance Agency (FHFA) Housing Price Index",
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

  # Parse geography from URL
  current_geo <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    list(
      type = query$geo %||% "state",
      cbsa = query$cbsa
    )
  })

  # Filter data based on current geography
  filtered_data <- reactive({
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      hpi |>
        filter(geography == "CBSA", name == geo$cbsa)
    } else if (geo$type == "nonmetro") {
      hpi |>
        filter(geography == "Nonmetro")
    } else {
      hpi |>
        filter(geography == "State")
    }
  })

  # Plot title based on geography
  plot_title <- reactive({
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      paste("Housing Price Index in", geo$cbsa)
    } else if (geo$type == "nonmetro") {
      "Housing Price Index in Nonmetropolitan Virginia"
    } else {
      "Housing Price Index in Virginia"
    }
  })
  
  # Function to create interactive line plots
  create_line_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Get latest value for label
    latest_data <- data |> 
      filter(date == max(date, na.rm = TRUE))
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Date: ", year(date), " Q", quarter(date), "\n",
        "HPI: ", round(hpi, 2)
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(
                  x = date,
                  y = hpi,
                  group = 1
                )) +
      geom_line_interactive(
        aes(tooltip = tooltip),
        color = hfv_colors$sky,
        linewidth = 1.2
      ) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = paste(date, hpi)),
        color = hfv_colors$sky,
        size = 2
      ) +
      # Add label for latest value
      geom_text(data = latest_data, 
                aes(label = round(hpi, 1)),
                hjust = -0.3, vjust = 0.5, 
                color = hfv_colors$shadow) +
      labs(
        title = title_text,
        y = "Housing Price Index",
        x = "Year",
        caption = " " # Add empty caption to leave space for logo
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 15, 30, 5) # Extra bottom margin for logo
      ) +
      scale_x_date(date_labels = "%Y", date_breaks = "5 years")
    
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
        opts_sizing(rescale = TRUE),
        opts_toolbar(hidden = c("lasso_select", "lasso_deselect"))
      ),
      fonts = list(
        addGFontHtmlDependency(family = "Open Sans"),
        addGFontHtmlDependency(family = "Poppins")
      )
    )
  }
  
  # Render the plot
  output$plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_line_plot(filtered_data(), plot_title())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
  
  # Display hover information
  output$hover_info <- renderText({
    data <- filtered_data()
    geo <- current_geo()

    # If there's no data, show a placeholder message
    if (is.null(data) || nrow(data) == 0) {
      return("Hover over a point for details")
    }

    # Get geography name
    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      geo_name <- geo$cbsa
    } else if (geo$type == "nonmetro") {
      geo_name <- "Nonmetropolitan Virginia"
    } else {
      geo_name <- "Virginia"
    }

    # Format some example hover data for display
    if (nrow(data) > 0) {
      # Take the latest data point as an example
      example <- data |> filter(date == max(date, na.rm = TRUE))

      paste0(
        geo_name, "\n",
        "Latest HPI: ", round(example$hpi[1], 1), "\n",
        "Date: ", year(example$date[1]), " Q", quarter(example$date[1])
      )
    } else {
      "Hover over a point for details"
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")

