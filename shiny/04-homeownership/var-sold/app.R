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
library(arrow)
library(gfonts)

# =============================================================================
# MEDIAN DAYS ON MARKET VISUALIZATION
# =============================================================================

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

# =============================================================================
# LOAD DATA OUTSIDE SERVER
# =============================================================================

data <- read_rds("home-sales.rds")

# Get available options
msa_list <- unique(sort(data$name[data$geography == "MSA"]))
locality_list <- unique(sort(data$name[data$geography == "Locality"]))


# =============================================================================
# USER INTERFACE
# =============================================================================

# Define UI
ui <- function(request) {
  page_fillable(
    theme = hfv_theme,
    includeCSS("www/styles/hfv-theme.css"),  # Add custom theme css
    useShinyjs(), # Initialize shinyjs

    # Main container using HFV classes
    div(
      class = "hfv-container",

      # Header using HFV styling
      div(
        class = "hfv-header",
        h4("Homes Sold", class = "hfv-title")
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

          h5("Filters",
             class = "text-primary", style = "margin-bottom: 16px;"),

          # Divider
          hr(style = "margin: 24px 0; border-color: #ced4da;"),

          # Data source
          div(
            style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
            p(
              strong("Data Source:"), br(),
              "Virginia Association of REALTORS",
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

# =============================================================================
# SERVER FUNCTION
# =============================================================================

# Server function
server <- function(input, output, session) {

  # Parse geography from URL
  current_geo <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    list(
      type = query$geo %||% "state",
      msa = query$msa,
      locality = query$locality
    )
  })

  # Filter data based on current geography
  filtered_data <- reactive({
    geo <- current_geo()

    if (geo$type == "msa" && !is.null(geo$msa)) {
      data |>
        filter(geography == "MSA", name == geo$msa)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      data |>
        filter(geography == "Locality", name == geo$locality)
    } else {
      data |>
        filter(geography == "State")
    }
  })

  # Plot title based on geography
  plot_title <- reactive({
    geo <- current_geo()

    if (geo$type == "msa" && !is.null(geo$msa)) {
      paste("Homes Sold -", geo$msa)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      paste("Homes Sold -", geo$locality)
    } else {
      "Virginia Homes Sold"
    }
  })
  
  # Function to create denial rate plots
  create_sold_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Create tooltips and prepare data for x-axis labels
    plot_data <- data %>%
      mutate(
        tooltip = paste0(
          "Quarter: ", quarter, "\n",
          "Homes Sold: ", units
        ),
        # Extract year from quarter for labeling
        year = str_extract(quarter, "\\d{4}"),
        quarter_num = str_extract(quarter, "Q\\d"),
        # Create a flag for Q1 quarters to show labels
        show_label = ifelse(quarter_num == "Q1", year, "")
      )
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = quarter,
                    y = units,
                    fill = units)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = units),
        width = 0.7
      ) +
      # Add continuous color scale based on #259591
      scale_fill_gradient(
        low = "#99a5b3",   # Light version of #259591
        high = "#011E41",   # Your specified color
        name = "Homes Sold"
      ) +
      # Custom x-axis labels - only show Q1 years
      scale_x_discrete(
        labels = plot_data$show_label
      ) +
      labs(
        title = title_text,
        caption = " " # Add empty caption to leave space for logo
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 20, 30, 5) # Extra bottom margin for logo
      )
    
    # Add logo
    logo_url <- "https://housingforwardva.org/wp-content/uploads/2024/08/HousingForward-VA-Logo-Files-Horizontal-Gradient-RGB.png"
    
    p_with_logo <- ggdraw(p) +
      draw_image(
        logo_url,
        x = 0.85,
        y = 0.05,
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
    suppressWarnings(create_interactive_plot(create_sold_plot(filtered_data(), plot_title())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")