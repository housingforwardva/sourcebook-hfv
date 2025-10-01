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
library(sass) # For SCSS compilation
library(gdtools)
library(gfonts)

# =============================================================================
# HFV STYLING SYSTEM INTEGRATION
# =============================================================================

# Register Google Fonts for ggiraph plots and system
register_gfont("Open Sans")
register_gfont("Poppins")

# Register fonts with systemfonts using Google Fonts URLs
tryCatch(
  {
    # For local development and server rendering, we'll use fallback fonts
    # The web fonts are handled by the HTML dependencies in girafe
    message("Google Fonts registered for web rendering")
  },
  error = function(e) {
    message("Font registration warning: ", e$message)
  }
)

# Compile HFV styles if needed (for deployment compatibility)
compile_hfv_styles_if_needed <- function() {
  css_file <- "www/styles/hfv-theme.css"
  scss_file <- "www/styles/hfv-theme.scss"

  # Only compile if CSS doesn't exist or SCSS is newer
  if (
    !file.exists(css_file) ||
      (file.exists(scss_file) && file.mtime(scss_file) > file.mtime(css_file))
  ) {
    message("🔄 Compiling HFV styles...")

    # Ensure the CSS directory exists
    dir.create(dirname(css_file), recursive = TRUE, showWarnings = FALSE)

    # Compile SCSS to CSS
    tryCatch(
      {
        sass(
          list(sass_file(scss_file)),
          output = css_file,
          options = sass_options(
            output_style = "expanded",
            source_map_embed = FALSE
          )
        )
        message("✅ HFV styles compiled successfully!")
      },
      error = function(e) {
        warning("❌ Failed to compile SCSS: ", e$message)
        warning("📝 Using fallback inline styles...")
      }
    )
  }

  return(file.exists(css_file))
}

# Define tenure colors locally
tenure_colors <- c(
  "All households" = "#40C0C0",
  "Homeowner" = "#011E41",
  "Renter" = "#E0592A"
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
        h4("Median Household Income by Tenure", class = "hfv-title")
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

          h5(
            "Dashboard Controls",
            class = "text-primary",
            style = "margin-bottom: 16px;"
          ),

          # Tenure selector
          div(
            style = "margin-bottom: 16px;",
            selectInput(
              "tenure",
              "Select Tenure:",
              choices = NULL,
              width = "100%",
              selectize = FALSE
            )
          ),

          # Dollar type toggle
          div(
            style = "margin-bottom: 16px;",
            radioButtons(
              "dollar_type",
              "Dollar Type:",
              choices = list(
                "Current Dollars" = "estimate",
                "Inflation-Adjusted" = "adjusted"
              ),
              selected = "adjusted",
              inline = FALSE
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
              "U.S. Census Bureau, American Community Survey 5-Year Estimates, Table B25119",
              style = "margin-bottom: 0;"
            ),
            conditionalPanel(
              condition = "input.dollar_type == 'adjusted'",
              p(
                strong("Note:"),
                "Income adjusted to 2023 dollars using CPI",
                style = "margin-bottom: 0; margin-top: 8px;"
              )
            )
          )
        ),

        # Main Panel
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
  # Load the data
  state_data <- read_rds("b25119_state.rds") %>%
    mutate(year = as.character(year))

  cbsa_data <- read_rds("b25119_cbsa.rds") %>%
    mutate(year = as.character(year))

  local_data <- read_rds("b25119_local.rds") %>%
    mutate(year = as.character(year))

  tenure_list <- c("All households", "Homeowner", "Renter")

  # Get current geography from URL
  current_geo <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    list(
      type = query$geo %||% "state",
      cbsa = query$cbsa,
      locality = query$locality
    )
  })

  # Initialize tenure dropdown
  observe({
    updateSelectInput(
      session,
      "tenure",
      choices = tenure_list,
      selected = "All households"
    )
  })

  # Single reactive for filtered data based on current geography
  filtered_data <- reactive({
    req(input$tenure)
    geo <- current_geo()

    if (geo$type == "state") {
      state_data %>%
        filter(state == "Virginia", tenure == input$tenure)
    } else if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      cbsa_data %>%
        filter(cbsa == geo$cbsa, tenure == input$tenure)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      local_data %>%
        filter(locality == geo$locality, tenure == input$tenure)
    } else {
      NULL
    }
  })

  # Plot title
  plot_title <- reactive({
    geo <- current_geo()
    if (geo$type == "state") {
      "Median Household Income in Virginia"
    } else if (geo$type == "cbsa") {
      paste("Median Household Income in", geo$cbsa)
    } else {
      paste("Median Household Income in", geo$locality)
    }
  })

  # Function to create interactive line plots
  create_line_plot <- function(data, title_text) {
    req(nrow(data) > 0)

    # Get selected dollar type
    value_col <- input$dollar_type

    # Determine y-axis label based on dollar type
    y_label <- if (value_col == "adjusted") {
      "Median Household Income (2023 Dollars)"
    } else {
      "Median Household Income (Current Dollars)"
    }

    # Add tooltips
    plot_data <- data %>%
      mutate(
        tooltip = paste0(
          "Year: ",
          year,
          "\n",
          "Tenure: ",
          tenure,
          "\n",
          "Income: ",
          scales::dollar(get(value_col))
        )
      )

    # Create base plot
    p <- ggplot(
      plot_data,
      aes(x = year, y = .data[[value_col]], color = tenure, group = tenure)
    ) +
      geom_line(linewidth = 1) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = paste(year, tenure)),
        size = 3
      ) +
      scale_color_manual(values = tenure_colors) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = title_text,
        subtitle = paste("For", tolower(input$tenure), "households"),
        caption = " ", # Add empty caption to leave space for logo
        y = y_label,
        x = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
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
    data <- filtered_data()
    req(data)
    suppressWarnings(create_interactive_plot(create_line_plot(
      data,
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
