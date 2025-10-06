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

# =============================================================================
# VACANCY RATE VISUALIZATION
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

vacancy <- read_rds("./data.rds")

state_data <- vacancy |> 
  filter(geography == "state")

cbsa_data <- vacancy |> 
  filter(geography == "cbsa")

local_data <- vacancy |> 
  filter(geography == "county")

cbsa_list <- sort(unique(cbsa_data$cbsa_title))
locality_list <- sort(unique(local_data$name_long))

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
        h4("Rental Vacancy Rate", class = "hfv-title")
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
          h5("Filters",
            class = "text-primary", style = "margin-bottom: 16px;"),

          # Divider
          hr(style = "margin: 24px 0; border-color: #ced4da;"),

          # Data source
          div(
            style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
            p(
              strong("Data Source:"), br(),
              "U.S. Census Bureau, American Community Survey 5-Year Estimates",
              style = "margin-bottom: 0;"
            ),
            p(
              strong("Note:"), "Vacancy rate calculated as (Total Units - Renter Occupied) / Total Units",
              style = "margin-bottom: 0; margin-top: 8px;"
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

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      cbsa_data %>%
        filter(cbsa_title == geo$cbsa)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      local_data %>%
        filter(name_long == geo$locality)
    } else {
      state_data
    }
  })

  # Plot title based on geography
  plot_title <- reactive({
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      paste("Rental Vacancy Rate in", geo$cbsa)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      paste("Rental Vacancy Rate in", geo$locality)
    } else {
      "Rental Vacancy Rate in Virginia"
    }
  })
  
  # Create a plot function for vacancy rate
  create_plot <- function(data, title_text) {
    # Add tooltip text to the data
    data <- data %>%
      mutate(tooltip = paste0(
        "Year: ", year, "\n",
        "Vacancy Rate: ", scales::percent(rate, accuracy = 0.1)
      ))
    
    # Create a pure, base ggplot with no theme customizations that could cause conflicts
    p <- ggplot(data, 
                aes(x = year, 
                    y = rate,
                  group = 1)) +
      geom_line_interactive(
        aes(tooltip = tooltip, data_id = year),
        color = "#40C0C0",
        size = 2
      ) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = year),
        color = "#40C0C0",
        size = 3
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                         expand = expansion(mult = c(0.05, 0.1))) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
      labs(
        title = title_text,
        caption = " ", # Add empty caption to leave space for logo
        y = "Vacancy Rate",
        x = "Year"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 10),
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
        opts_hover(css = "stroke-width:3px;"),
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
    suppressWarnings(create_interactive_plot(create_plot(filtered_data(), plot_title())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })

}
# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")