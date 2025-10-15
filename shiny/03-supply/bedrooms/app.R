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

# Define the bedroom order
bedroom_order <- c("No bedroom", "1 bedroom", "2 bedrooms", "3 bedrooms", 
                   "4 bedrooms", "5 or more bedrooms")

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
      h4("Distribution of Housing by Bedroom Count", class = "hfv-title")
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
                      choices = NULL, 
                      selected = NULL, 
                      width = "100%",
                      selectize = FALSE)
        ),
        
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "U.S. Census Bureau, American Community Survey 5-Year Estimates, Table B25042",
            style = "margin-bottom: 0;"
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
  # Read in the data
  b25042 <- read_rds(here("data", "rds", "b25042.rds"))

  # Pre-process the data
  state_bed <- b25042 %>%
    group_by(year, tenure, br) %>%
    summarise(estimate = sum(estimate), .groups = "drop") %>%
    mutate(br = factor(br, levels = bedroom_order))

  cbsa_bed <- b25042 %>%
    group_by(year, cbsa_title, tenure, br) %>%
    summarise(estimate = sum(estimate), .groups = "drop") %>%
    mutate(br = factor(br, levels = bedroom_order))

  local_bed <- b25042 %>%
    group_by(year, name_long, tenure, br) %>%
    summarise(estimate = sum(estimate), .groups = "drop") %>%
    mutate(br = factor(br, levels = bedroom_order))

  # Get current geography from URL
  current_geo <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    list(
      type = query$geo %||% "state",
      cbsa = query$cbsa,
      locality = query$locality
    )
  })

  # Update year filter choices
  observe({
    years <- unique(b25042$year)
    updateSelectInput(session, "year",
                      choices = years,
                      selected = max(years))
  })
  
  # Create title text
  title_text <- reactive({
    geo <- current_geo()
    if (geo$type == "state") {
      paste("Bedroom Distribution in Virginia", input$year)
    } else if (geo$type == "cbsa") {
      paste("Bedroom Distribution in", geo$cbsa, input$year)
    } else {
      paste("Bedroom Distribution in", geo$locality, input$year)
    }
  })
  
  # Create a plot function for bedroom distribution
  create_plot <- function(data) {
    # Add tooltip text to the data
    data <- data %>%
      mutate(tooltip = paste0(
        "Bedrooms: ", br, "\n",
        "Tenure: ", tenure, "\n",
        "Households: ", format(estimate, big.mark = ",")
      ))
    
    # Create a pure, base ggplot with no theme customizations that could cause conflicts
    p <- ggplot(data, 
                aes(x = br, 
                    y = estimate, 
                    fill = tenure)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = paste(br, tenure)),
        position = "stack"
      ) +
      facet_wrap(~tenure, ncol = 1) +
      coord_flip() +  # Flip for horizontal bars
      # Use Housing Forward Virginia colors
      scale_fill_manual(values = c("Homeowner" = "#40C0C0", "Renter" = "#011E41")) +
      scale_y_continuous(labels = scales::number_format(big.mark = ","),
                         expand = expansion(mult = c(0, 0.1))) +
      labs(
        title = title_text(),
        caption = " ", # Add empty caption to leave space for logo
        y = "Number of Households",
        x = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        strip.text = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        panel.spacing.x = unit(15, "pt"),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        # Make the plot more compact overall with extra bottom margin for logo
        plot.margin = margin(5, 5, 15, 5)
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
  
  # Single reactive for filtered data based on current geography
  filtered_data <- reactive({
    req(input$year)
    geo <- current_geo()

    if (geo$type == "state") {
      state_bed %>%
        filter(year == input$year)
    } else if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      cbsa_bed %>%
        filter(year == input$year,
               cbsa_title == geo$cbsa)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      local_bed %>%
        filter(year == input$year,
               name_long == geo$locality)
    } else {
      NULL
    }
  })
  
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
    suppressWarnings(create_interactive_plot(create_plot(data)))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")