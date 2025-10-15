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
        h4("Housing Units by Structure Type", class = "hfv-title")
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

          # Year select
          div(
            style = "margin-bottom: 16px;",
            selectInput("year", "Select Year:",
                        choices = 2017:2023,
                        selected = 2023,
                        width = "100%",
                        selectize = FALSE)
          ),

          # Display options
          div(
            style = "margin-bottom: 16px;",
            radioButtons("displayType", "Display:",
                         choices = c("Percent" = "percent", "Count" = "count"),
                         selected = "percent",
                         inline = TRUE)
          ),

          # Divider
          hr(style = "margin: 24px 0; border-color: #ced4da;"),

          # Data source
          div(
            style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
            p(
              strong("Data Source:"), br(),
              "U.S. Census Bureau, American Community Survey, Table B25127",
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
      cbsa = query$cbsa,
      locality = query$locality
    )
  })

  # Load the data
  b25127 <- readRDS("b25127.rds")

  # Define structure order
  structure_order <- c("1, detached or attached", "2 to 4", "5 to 19", "20 to 49",
                       "50 or more", "Mobile home, boat, RV, van, etc.")

  # Pre-compute datasets
  state_data <- b25127 %>%
    group_by(year, tenure, structure) %>%
    summarise(estimate = sum(estimate), .groups = "drop") %>%
    group_by(year) %>%
    mutate(percent = estimate/sum(estimate)) %>%
    ungroup() %>%
    mutate(structure = factor(structure, levels = structure_order))

  cbsa_data <- b25127 %>%
    group_by(year, cbsa_title, tenure, structure) %>%
    summarise(estimate = sum(estimate), .groups = "drop") %>%
    group_by(year, cbsa_title) %>%
    mutate(percent = estimate/sum(estimate)) %>%
    ungroup() %>%
    mutate(structure = factor(structure, levels = structure_order))

  locality_data <- b25127 %>%
    group_by(year, name_long, tenure, structure) %>%
    summarise(estimate = sum(estimate), .groups = "drop") %>%
    group_by(year, name_long) %>%
    mutate(percent = estimate/sum(estimate)) %>%
    ungroup() %>%
    mutate(structure = factor(structure, levels = structure_order))

  # Filter data based on current geography
  filtered_data <- reactive({
    req(input$year)
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      cbsa_data %>%
        filter(cbsa_title == geo$cbsa, year == input$year)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      locality_data %>%
        filter(name_long == geo$locality, year == input$year)
    } else {
      state_data %>%
        filter(year == input$year)
    }
  })
  
  # Function to create interactive stacked bar chart
  create_structure_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Determine whether to use percent or count
    if (input$displayType == "percent") {
      plot_data <- data %>%
        mutate(value = percent,
               tooltip = paste0(
                 "Structure Type: ", structure, "\n",
                 "Tenure: ", tenure, "\n",
                 "Percent: ", scales::percent(percent, accuracy = 0.1)
               ))
      y_label <- "Percent of Housing Units"
      y_scale <- scale_y_continuous(labels = scales::percent_format())
    } else {
      plot_data <- data %>%
        mutate(value = estimate,
               tooltip = paste0(
                 "Structure Type: ", structure, "\n",
                 "Tenure: ", tenure, "\n",
                 "Units: ", format(estimate, big.mark = ",")
               ))
      y_label <- "Number of Housing Units"
      y_scale <- scale_y_continuous(labels = scales::number_format(big.mark = ","))
    }
    
    # Create base plot
    p <- ggplot(plot_data, 
                aes(x = structure,
                    y = value,
                    fill = tenure)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = interaction(structure, tenure)),
        position = "stack"
      ) +
      scale_fill_manual(values = c(
        "Homeowner" = hfv_colors$shadow,
        "Renter" = hfv_colors$sky
      )) +
      y_scale +
      labs(
        title = title_text,
        subtitle = paste("Year:", input$year),
        caption = " ", # Add empty caption to leave space for logo
        y = y_label,
        x = NULL,
        fill = "Tenure"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 15, 5) # Extra bottom margin for logo
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
  
  # Set plot title based on geography
  plot_title <- reactive({
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      paste("Housing Units by Structure Type in", geo$cbsa)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      paste("Housing Units by Structure Type in", geo$locality)
    } else {
      "Virginia Housing Units by Structure Type"
    }
  })

  # Render the plot
  output$plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_structure_plot(filtered_data(), plot_title())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")
