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
# MEDIAN GROSS RENT VISUALIZATION
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

# Load the data
data <- read_rds("./data.rds")

state_med_rent_data <- data |> 
  filter(geography == "state")

cbsa_med_rent_data <- data |> 
  filter(geography == "cbsa")

local_med_rent_data <- data |> 
  filter(geography == "county")

# Get available options
state_list <- sort(unique(state_med_rent_data$NAME))
cbsa_list <- sort(unique(cbsa_med_rent_data$NAME))
locality_list <- sort(unique(local_med_rent_data$NAME))

# =============================================================================
# USER INTERFACE
# =============================================================================

# Define UI
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
        h4("Median Gross Rent", class = "hfv-title")
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

          # Dollar type selector
          div(
            style = "margin-bottom: 16px;",
            radioButtons("dollar_type", "Dollar Type:",
                         choices = list("Current Dollars" = "estimate",
                                        "Inflation-Adjusted Dollars" = "adjusted"),
                         selected = "estimate",
                         inline = FALSE)
          ),

          # Divider
          hr(style = "margin: 24px 0; border-color: #ced4da;"),

          # Data source
          div(
            style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
            p(
              strong("Data Source:"), br(),
              "U.S. Census Bureau, American Community Survey 5-Year Estimates, Table B25064",
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

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      cbsa_med_rent_data %>%
        filter(NAME == geo$cbsa)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      local_med_rent_data %>%
        filter(NAME == geo$locality) %>%
        mutate(
          estimate = as.numeric(estimate),
          adjusted = as.numeric(adjusted)
        )
    } else {
      state_med_rent_data %>%
        filter(NAME == "Virginia")
    }
  })

  # Y-axis label based on dollar type
  y_label <- reactive({
    ifelse(input$dollar_type == "adjusted",
           "Inflation-Adjusted Dollars",
           "Current Dollars")
  })

  # Plot title based on geography
  plot_title <- reactive({
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      paste("Median Gross Rent in", geo$cbsa)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      paste("Median Gross Rent in", geo$locality)
    } else {
      "Median Gross Rent in Virginia"
    }
  })
  
  # Function to create interactive line plots
  create_line_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Get selected column for y-axis
    y_var <- input$dollar_type
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Year: ", year, "\n",
        "Rent: ", scales::dollar(get(y_var))
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(
                  x = year,
                  y = .data[[y_var]],
                  group = 1
                )) +
      geom_line_interactive(
        aes(tooltip = tooltip, data_id = year),
        color = "#40C0C0",
        linewidth = 2
      ) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = year),
        color = "#40C0C0",
        size = 4
      ) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = title_text,
        y = y_label(),
        x = "Year",
        caption = " " # Add empty caption to leave space for logo
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 30, 5) # Extra bottom margin for logo
      )
    
    # Handle different x-axis scales between locality and other levels
    if("locality" %in% names(data)) {
      p <- p + scale_x_discrete(
        breaks = unique(plot_data$year),
        labels = unique(plot_data$year)
      )
    } else {
      p <- p + scale_x_continuous(
        breaks = unique(plot_data$year),
        labels = unique(plot_data$year)
      )
    }
    
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
}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")