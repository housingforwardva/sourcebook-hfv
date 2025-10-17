library(shiny)
library(tidyverse)
library(readxl)
library(stringr)
library(cowplot)
library(mapgl)
library(sf)
library(air)
library(here)
library(bslib)
library(shinyjs)
library(ggiraph)
library(gfonts)

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

va_subsidies <- read_rds("data.rds") |>
  filter(subsidy_status == "Active/Inconclusive")

cbsa_list <- sort(unique(va_subsidies$cbsa_title))

locality_list <- sort(unique(va_subsidies$name_long))


# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- function(request) {
  page_fillable(
    theme = hfv_theme,
    includeCSS("www/styles/hfv-theme.css"),
    useShinyjs(),
    # Main container using HFV classes
    div(
      class = "hfv-container",

      # Header using HFV styling
      div(
        class = "hfv-header",
        h4("Federally Assisted Rental Housing", class = "hfv-title")
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

          h5("Filters", class = "text-primary", style = "margin-bottom: 16px;"),

          # Divider
          hr(style = "margin: 24px 0; border-color: #ced4da;"),

          # Data source
          div(
            style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
            p(
              strong("Data Source:"),
              br(),
              "National Housing Preservation Database.",
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

server <- function(input, output, session) {
  # Parse geography from URL
  current_geo <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    list(
      type = query$geo %||% "state",
      cbsa = query$cbsa,
      locality = query$locality
    )
  })

  # Filter data based on current geography
  filtered_data <- reactive({
    geo <- current_geo()
    base_data <- va_subsidies

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      base_data %>%
        filter(cbsa_title == geo$cbsa) %>%
        group_by(cbsa_title, subsidy_name, subsidy_status) %>%
        summarise(
          value = sum(assisted_units, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          tooltip = paste0(
            "Subsidy Name: ",
            subsidy_name,
            "\n",
            "Assisted units: ",
            value
          )
        ) %>%
        group_by(subsidy_name) %>%
        mutate(max_value_per_subsidy = max(value)) %>%
        ungroup()
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      base_data %>%
        filter(name_long == geo$locality) %>%
        group_by(name_long, subsidy_name, subsidy_status) %>%
        summarise(
          value = sum(assisted_units, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          tooltip = paste0(
            "Subsidy Name: ",
            subsidy_name,
            "\n",
            "Assisted units: ",
            value
          )
        ) %>%
        group_by(subsidy_name) %>%
        mutate(max_value_per_subsidy = max(value)) %>%
        ungroup()
    } else {
      base_data %>%
        group_by(subsidy_name, subsidy_status) %>%
        summarise(
          value = sum(assisted_units, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          tooltip = paste0(
            "Subsidy Name: ",
            subsidy_name,
            "\n",
            "Assisted units: ",
            value
          )
        ) %>%
        group_by(subsidy_name) %>%
        mutate(max_value_per_subsidy = max(value)) %>%
        ungroup()
    }
  })

  # Plot title based on geography
  plot_title <- reactive({
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      paste("Federally-Assisted Rental Housing in", geo$cbsa, "Metro")
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      paste("Federally-Assisted Rental Housing in", geo$locality)
    } else {
      "Federally-Assisted Rental Housing in Virginia"
    }
  })

  # Single function to create all plots
  create_subsidy_plot <- function(data, title_text, subtitle_text = NULL) {
    req(nrow(data) > 0)

    # Add tooltips
    plot_data <- data

    # Create base plot
    p <- ggplot(
      plot_data,
      aes(
        x = reorder(subsidy_name, max_value_per_subsidy),
        y = value,
        fill = subsidy_status
      )
    ) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = subsidy_name),
        position = "stack"
      ) +
      scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
      labs(
        title = title_text,
        caption = " "
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 0.5),
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 30, 5)
      )

    # Add logo (single implementation)
    add_hfv_logo(p)
  }

  # Helper function for logo (extracted to reduce duplication)
  add_hfv_logo <- function(plot) {
    logo_url <- "https://housingforwardva.org/wp-content/uploads/2024/08/HousingForward-VA-Logo-Files-Horizontal-Gradient-RGB.png"

    ggdraw(plot) +
      draw_image(
        logo_url,
        x = 0.85,
        y = 0.05,
        width = 0.15,
        height = 0.15
      )
  }

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
        opts_selection(type = "none")
      )
    )
  }

  # Render the plot
  output$plot <- renderGirafe({
    data <- filtered_data()
    req(data)
    plot <- create_subsidy_plot(data, plot_title())
    create_interactive_plot(plot)
  })

  # Mobile optimization
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")
