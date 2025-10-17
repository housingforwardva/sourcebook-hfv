library(shiny)
library(tidyverse)
library(ggiraph) # For interactive ggplots
library(here) # For here() function in file paths
library(grid) # For grobs
library(png) # For reading PNG files
library(bslib) # For modern UI components
library(cowplot) # For adding logo to plots
library(scales) # For number_format
library(shinyjs) # For dynamic UI updates
library(magick) # For image handling
library(gdtools)
library(gfonts)

# =============================================================================
# GROSS RENT DISTRIBUTION VISUALIZATION
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

data <- read_rds("data.rds")

# Define proper rent range factor order
rent_range_levels <- c(
  "No cash rent",
  "Less than $500",
  "$500 to $749",
  "$750 to $999",
  "$1,000 to $1,249",
  "$1,250 to $1,499",
  "$1,500 to $1,999",
  "$2,000 or more"
)

# Apply factor ordering to data
data <- data |>
  filter(!is.na(rent_range)) |>
  mutate(rent_range = factor(rent_range, levels = rent_range_levels))

state_data <- data |>
  group_by(year, rent_range) |>
  summarise(estimate = sum(estimate)) |>
  ungroup()

cbsa_data <- data |>
  group_by(year, cbsa_title, rent_range) |>
  summarise(estimate = sum(estimate)) |>
  ungroup()

local_data <- data |>
  group_by(year, name_long, rent_range) |>
  summarise(estimate = sum(estimate)) |>
  ungroup()


cbsa_list <- sort(unique(cbsa_data$cbsa_title))

locality_list <- sort(unique(local_data$name_long))

year_list <- sort(unique(cbsa_data$year), decreasing = TRUE)


# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- function(request) {
  page_fillable(
    theme = hfv_theme,
    includeCSS("www/styles/hfv-theme.css"),
    useShinyjs(),

    div(
      class = "hfv-container",

      div(
        class = "hfv-header",
        h4("Gross Rent Distribution", class = "hfv-title")
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
          h5("Filters", class = "text-primary", style = "margin-bottom: 16px;"),

          div(
            style = "margin-bottom: 16px;",
            selectInput(
              "year",
              "Select Year:",
              choices = NULL,
              width = "100%",
              selectize = FALSE
            )
          ),

          # Divider
          hr(style = "margin: 24px 0; border-color: #ced4da;"),

          # Data source
          div(
            style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
            p(
              strong("Data Source:"),
              br(),
              "U.S. Census Bureau, American Community Survey 5-Year Estimates, Table B25063",
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

  # Initialize year dropdown
  observe({
    updateSelectInput(
      session,
      "year",
      choices = year_list,
      selected = year_list[1]
    )
  })

  # Filter data based on current geography
  filtered_data <- reactive({
    req(input$year)
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      cbsa_data %>%
        filter(cbsa_title == geo$cbsa, year == input$year)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      local_data %>%
        filter(name_long == geo$locality, year == input$year)
    } else {
      state_data %>%
        filter(year == input$year)
    }
  })

  # Plot title based on geography
  plot_title <- reactive({
    req(input$year)
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      paste("Gross Rent Distribution in", geo$cbsa, "(", input$year, ")")
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      paste("Gross Rent Distribution in", geo$locality, "(", input$year, ")")
    } else {
      paste("Virginia Gross Rent Distribution (", input$year, ")")
    }
  })

  # Create a plot function for gross rent distribution
  create_plot <- function(data, title_text) {
    # Add tooltip text to the data
    data <- data %>%
      mutate(
        tooltip = paste0(
          "Rent Range: ",
          rent_range,
          "\n",
          "Households: ",
          format(estimate, big.mark = ",")
        )
      )

    # Create a pure, base ggplot with no theme customizations that could cause conflicts
    p <- ggplot(data, aes(x = rent_range, y = estimate)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = rent_range),
        fill = "#40C0C0"
      ) +
      scale_y_continuous(
        labels = scales::number_format(big.mark = ","),
        expand = expansion(mult = c(0, 0.1))
      ) +
      labs(
        title = title_text,
        caption = " ", # Add empty caption to leave space for logo
        y = "Number of Households",
        x = "Rent Range"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(15, "pt"),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 30, 5) # Extra bottom margin for logo
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
    suppressWarnings(create_interactive_plot(create_plot(
      filtered_data(),
      plot_title()
    )))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")
