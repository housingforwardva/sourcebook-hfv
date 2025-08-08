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
library(tidycensus)

# Load data - ONLY load from the specified path, no simulated data
bps <- read_rds("./bps.rds")

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

# Prepare aggregated datasets with recategorized building types
# First, create a function to recode building types
recode_type <- function(df) {
  df %>%
    # Assuming the original column is called 'type' or similar
    # Combine 2-unit and 3-4 unit into "2-4 unit"
    mutate(
      type = case_when(
        type == "1-unit" ~ "1-unit",
        type == "2-units" | type == "3-4 units" ~ "2-4 units",
        type == "5+ units" ~ "5+ units",
        TRUE ~ as.character(type)
      )
    )
}

# Apply the recoding and create aggregated datasets
state <- bps %>%
  recode_type() %>%
  group_by(year, type) %>%
  summarise(
    bldgs = sum(bldgs),
    units = sum(units),
    value = sum(value),
    .groups = 'drop'
  )

cbsa <- bps %>%
  recode_type() %>%
  group_by(year, cbsa_title, type) %>%
  summarise(
    bldgs = sum(bldgs),
    units = sum(units),
    value = sum(value),
    .groups = 'drop'
  )

locality <- bps %>%
  recode_type() %>%
  group_by(year, name_long, type) %>%
  summarise(
    bldgs = sum(bldgs),
    units = sum(units),
    value = sum(value),
    .groups = 'drop'
  )

# Define UI
ui <- page_fillable(
  theme = hfv_theme,
  useShinyjs(), # Initialize shinyjs

  # Main container using HFV classes
  div(
    class = "hfv-container",
    
    # Header using HFV styling
    div(
      class = "hfv-header",
      h4("Building Permit Trends", class = "hfv-title")
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

        # Metric select
        div(
          style = "margin-bottom: 16px;",
          selectInput(
            "metric",
            "Select Metric:",
            choices = list(
              "Units" = "units",
              "Buildings" = "bldgs",
              "Value ($)" = "value"
            ),
            selected = "units",
            width = "100%",
            selectize = TRUE
          )
        ),

        # Building type filter
        div(
          style = "margin-bottom: 16px;",
          checkboxGroupInput(
            "types",
            "Building Types:",
            choices = list(
              "1-unit" = "1-unit",
              "2-4 units" = "2-4 units",
              "5+ units" = "5+ units"
            ),
            selected = c("1-unit", "2-4 units", "5+ units"),
            width = "100%"
          )
        ),

        # Geography selectors
        div(
          style = "margin-bottom: 16px;",
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            selectInput(
              "cbsa",
              "Metro Area:",
              choices = NULL,
              width = "100%",
              selectize = FALSE
            )
          ),
          conditionalPanel(
            condition = "input.tabs == 'local'",
            selectInput(
              "locality",
              "Locality:",
              choices = NULL,
              width = "100%",
              selectize = FALSE
            )
          )
        ),

        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),

        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "U.S. Census Bureau Building Permits Survey",
            style = "margin-bottom: 0;"
          )
        )
      ),

      # Main Panel with tabs
      div(
        navset_tab(
          id = "tabs",
          
          nav_panel(
            title = "State",
            value = "state",
            div(
              class = "hfv-chart-container",
              style = "height: 450px; margin-top: 16px;",
              girafeOutput("state_plot", height = "100%")
            )
          ),
          
          nav_panel(
            title = "Metro Area",
            value = "cbsa", 
            div(
              class = "hfv-chart-container",
              style = "height: 450px; margin-top: 16px;",
              girafeOutput("cbsa_plot", height = "100%")
            )
          ),
          
          nav_panel(
            title = "Locality",
            value = "local",
            div(
              class = "hfv-chart-container",
              style = "height: 450px; margin-top: 16px;",
              girafeOutput("local_plot", height = "100%")
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Add this at the beginning of your server function:
  logo_data <- NULL

  # At the start of your server function, load and encode the logo:
  observe({
    # Try to read logo file from www directory
    tryCatch(
      {
        # This path works in both local and deployed environments
        logo_file <- "www/hfv_logo.png"

        # Read the binary data and convert to base64
        logo_binary <- readBin(logo_file, "raw", file.info(logo_file)$size)
        logo_data <<- logo_binary
      },
      error = function(e) {
        # Log error message
        message("Could not load logo: ", e$message)
      }
    )
  })

  # Update metro area choices
  observe({
    cbsa_choices <- sort(unique(cbsa$cbsa_title))
    updateSelectInput(
      session,
      "cbsa",
      choices = cbsa_choices,
      selected = "Richmond, VA"
    )
  })

  # Update locality choices
  observe({
    locality_choices <- sort(unique(locality$name_long))
    updateSelectInput(
      session,
      "locality",
      choices = locality_choices,
      selected = "Richmond City"
    )
  })

  # Filter data based on selected building types
  filter_data <- function(data) {
    # Filter by selected building types
    data %>% filter(type %in% input$types)
  }

  # Generate title based on current tab and selections
  get_title <- reactive({
    if (input$tabs == "state") {
      "Virginia Building Permits"
    } else if (input$tabs == "cbsa") {
      paste(input$cbsa, "Building Permits")
    } else {
      paste(input$locality, "Building Permits")
    }
  })

  # Get subtitle based on metric selection
  get_subtitle <- reactive({
    metric_label <- case_when(
      input$metric == "units" ~ "Housing Units",
      input$metric == "bldgs" ~ "Buildings",
      input$metric == "value" ~ "Value ($ million)",
      TRUE ~ input$metric
    )

    metric_label
  })

  # Create plots with interactive tooltips on stacked bars
  create_plot <- function(data, metric_col, title, subtitle) {
    # Map colors to building types
    color_mapping <- c(
      "1-unit" = "#40C0C0",
      "2-4 units" = "#011E41",
      "5+ units" = "#8B85CA"
    )

    # Format tooltip text
    tooltip_format <- function(year, type, value) {
      if (metric_col == "value") {
        paste0(
          type,
          " (",
          year,
          "): ",
          scales::dollar(value, accuracy = 0.1, prefix = "$", suffix = "M")
        )
      } else {
        paste0(
          type,
          " (",
          year,
          "): ",
          scales::comma(value, accuracy = 1)
        )
      }
    }

    # For value, convert to millions
    if (metric_col == "value") {
      data <- data %>%
        mutate(value = value / 1000000)
    }

    # Create the base plot
    p <- ggplot(
      data,
      aes(
        x = year,
        y = !!sym(metric_col),
        fill = type,
        tooltip = tooltip_format(year, type, !!sym(metric_col)),
        data_id = paste(type, year)
      )
    ) +
      geom_col_interactive(position = "stack") +
      scale_fill_manual(values = color_mapping) +
      scale_x_continuous(breaks = unique(data$year)) +
      # Format y-axis based on the metric
      {
        if (metric_col == "value")
          scale_y_continuous(
            labels = scales::dollar_format(
              scale = 1,
              prefix = "$",
              suffix = "M"
            )
          ) else
          scale_y_continuous(labels = scales::number_format(big.mark = ","))
      } +
      labs(
        title = title,
        subtitle = subtitle,
        caption = " ", # Empty caption to leave space for logo
        fill = "Building Type"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "bottom",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 30, 5) # Extra bottom margin for logo
      )

    # Add logo directly using external URL
    logo_url <- "https://housingforwardva.org/wp-content/uploads/2024/08/HousingForward-VA-Logo-Files-Horizontal-Gradient-RGB.png"

    # Add logo to the plot using the URL
    logo_plot <- ggdraw(p) +
      draw_image(
        logo_url, # Use URL directly
        x = 0.85, # Horizontal position (right side)
        y = 0.05, # Vertical position (bottom)
        width = 0.15,
        height = 0.15
      )

    # Create girafe object with the logo plot
    girafe(
      ggobj = logo_plot,
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

  # Create the state plot
  output$state_plot <- renderGirafe({
    req(input$metric, input$types)

    # Filter and prepare the data
    plot_data <- filter_data(state)
    metric_col <- input$metric

    # Create the plot
    create_plot(
      plot_data,
      metric_col,
      get_title(),
      get_subtitle()
    )
  })

  # Create the CBSA plot
  output$cbsa_plot <- renderGirafe({
    req(input$metric, input$cbsa, input$types)

    # Filter and prepare data
    plot_data <- filter_data(cbsa) %>%
      filter(cbsa_title == input$cbsa)

    metric_col <- input$metric

    # Create the plot
    create_plot(
      plot_data,
      metric_col,
      get_title(),
      get_subtitle()
    )
  })

  # Create the locality plot
  output$local_plot <- renderGirafe({
    req(input$metric, input$locality, input$types)

    # Filter and prepare data
    plot_data <- filter_data(locality) %>%
      filter(name_long == input$locality)

    metric_col <- input$metric

    # Create the plot
    create_plot(
      plot_data,
      metric_col,
      get_title(),
      get_subtitle()
    )
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
