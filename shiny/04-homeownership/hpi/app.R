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
hpi <- read_rds(here("data", "rds", "hpi.rds")) |> 
  mutate(date = as.Date(as.yearqtr(date, format = "%Y Q%q"))) |> 
  select(geography, name, date, hpi) |> 
  filter(!is.na(hpi))

# Create lists for filters
cbsa_list <- sort(unique(hpi$name[hpi$geography == "CBSA"]))

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
        
        # Geography selectors
        div(
          style = "margin-bottom: 16px;",
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            selectInput("cbsa_select", "Metro Area:", 
                        choices = cbsa_list,
                        selected = if("Richmond, VA" %in% cbsa_list) "Richmond, VA" else cbsa_list[1],
                        width = "100%", 
                        selectize = FALSE)
          )
        ),
        
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
            title = "Nonmetro",
            value = "nonmetro",
            div(
              class = "hfv-chart-container",
              style = "height: 450px; margin-top: 16px;",
              girafeOutput("nonmetro_plot", height = "100%")
            )
          )
        )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Create filtered datasets
  state_data <- reactive({
    hpi |> 
      filter(geography == "State")
  })
  
  filtered_cbsa <- reactive({
    req(input$cbsa_select)
    hpi |> 
      filter(geography == "CBSA",
             name == input$cbsa_select)
  })
  
  nonmetro_data <- reactive({
    hpi |> 
      filter(geography == "Nonmetro")
  })
  
  # Plot titles
  state_title <- reactive({
    "Housing Price Index in Virginia"
  })
  
  cbsa_title <- reactive({
    paste("Housing Price Index in", input$cbsa_select)
  })
  
  nonmetro_title <- reactive({
    "Housing Price Index in Nonmetropolitan Virginia"
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
  
  # Render the plots
  output$state_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_line_plot(state_data(), state_title())))
  })
  
  output$cbsa_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_line_plot(filtered_cbsa(), cbsa_title())))
  })
  
  output$nonmetro_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_line_plot(nonmetro_data(), nonmetro_title())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
  
  # Handle hover info for all plots
  get_hover_data <- reactive({
    if (input$tabs == "state") {
      data <- state_data()
      geo_name <- "Virginia"
    } else if (input$tabs == "cbsa") {
      data <- filtered_cbsa()
      geo_name <- input$cbsa_select
    } else { # nonmetro
      data <- nonmetro_data()
      geo_name <- "Nonmetropolitan Virginia"
    }
    
    list(
      data = data,
      geo_name = geo_name
    )
  })
  
  # Display hover information
  output$hover_info <- renderText({
    hover_data <- get_hover_data()
    data <- hover_data$data
    
    # If there's no hover data, show a placeholder message
    if (is.null(data) || nrow(data) == 0) {
      return("Hover over a point for details")
    }
    
    geo_name <- hover_data$geo_name
    
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
shinyApp(ui = ui, server = server)

