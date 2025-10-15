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
library(arrow)
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

# HFV Color Palette
hfv_colors <- list(
  sky = "#40C0C0",           # Primary teal
  grass = "#259591",         # Dark teal/success
  lilac = "#8B85CA",         # Purple/info
  shadow = "#011E41",        # Dark navy/secondary
  shadow_light = "#102C54",  # Lighter navy
  berry = "#B1005F",         # Magenta/danger
  desert = "#E0592A"         # Orange/warning
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

# Define consistent colors for race_ethnicity categories
race_ethnicity_colors <- c(
  "White, non-Hispanic" = "#011E41",
  "Black" = "#B1005F",
  "Hispanic or Latino" = "#E0592A",
  "Asian" = hfv_colors$grass,
  "Other Minority" = hfv_colors$lilac,
  "White Co-Applicant" = hfv_colors$shadow_light,
  "Incomplete/No Data" = "#CCCCCC"
)

# Create a Bootstrap theme
hfv_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#333333",
  primary = hfv_colors$sky,
  secondary = hfv_colors$shadow,
  success = hfv_colors$grass,
  info = hfv_colors$lilac,
  warning = hfv_colors$desert,
  danger = hfv_colors$berry,
  base_font = font_google("Open Sans"),
  heading_font = font_google("Poppins"),
  font_scale = 0.8
)

# Define UI
ui <- function(request) {
  page_fillable(
    theme = hfv_theme,
    useShinyjs(),

    # Mobile optimization viewport
    tags$head(
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
      )
    ),

    # CSS styles (same as reference app)
    tags$head(
      tags$style(HTML(
      "
      body, html {
        margin: 0;
        padding: 0;
        height: auto;
        overflow-x: hidden;
      }
      
      @media (max-height: 600px) {
        .hfv-container {
          padding: 10px !important;
          margin: 0 auto !important;
          max-height: 500px !important;
          overflow: hidden !important;
        }
        
        .hfv-header {
          margin-bottom: 8px !important;
        }
        
        .hfv-sidebar {
          padding: 8px !important;
        }
        
        .girafe-container {
          height: 280px !important;
          min-height: 280px !important;
        }
        
        body, html {
          overflow: hidden !important;
        }
      }
      
      .hfv-container {
        max-width: 1200px; 
        margin: 0 auto; 
        padding: 45px;
      }
      
      .hfv-header {
        display: flex; 
        align-items: center; 
        margin-bottom: 15px; 
        border-bottom: 2px solid #40C0C0; 
        padding-bottom: 8px;
      }
      
      .hfv-header img {
        height: 30px;
        margin-right: 10px;
      }
      
      .title-text {
        margin: 0; 
        color: #011E41;
        font-size: 20px;
      }
      
      .hfv-sidebar {
        background-color: #E8EDF2;
        padding: 15px;
        border-radius: 5px;
      }
      
      .girafe-container {
        width: 100%;
        height: auto;
        min-height: 350px;
        overflow: visible;
      }
      
      .girafe-container svg {
        width: 100% !important;
        height: 100% !important;
      }
      
      @media (max-width: 992px) {
        .hfv-container {
          padding: 10px;
        }
        
        .title-text {
          font-size: 18px;
        }
        
        .girafe-container {
          height: 400px;
        }
      }
      
      @media (max-width: 768px) {
        .hfv-container {
          padding: 8px;
          border-width: 1px;
        }
        
        .title-text {
          font-size: 16px;
        }
        
        .hfv-header {
          margin-bottom: 10px;
        }
        
        .hfv-sidebar {
          padding: 10px;
          margin-bottom: 10px;
        }
        
        .control-label {
          font-size: 12px;
        }
        
        .form-check-label {
          font-size: 11px;
        }
        
        .form-select {
          font-size: 12px;
        }
        
        .form-control {
          font-size: 12px;
        }
        
        .girafe-container {
          height: 350px;
        }
      }
      
      @media (max-width: 480px) {
        .hfv-container {
          padding: 5px;
        }
        
        .hfv-header img {
          height: 25px;
        }
        
        .title-text {
          font-size: 14px;
        }
        
        .hfv-sidebar {
          padding: 8px;
        }
        
        .girafe-container {
          height: 300px;
        }
        
        .nav-tabs .nav-link {
          font-size: 13px;
          padding: 6px 10px;
        }
      "
    ))
  ),

  # Main container
  div(
    class = "hfv-container",
    
    # Header with logo and title
    div(
      class = "hfv-header",
      img(
        src = "https://housingforwardva.org/wp-content/uploads/2025/05/HousingForward-VA-Logo-Files-Icon-One-Color-RGB.png",
        alt = "HousingForward VA Logo"
      ),
      h4("Mortgage Lending by Race and Ethnicity", class = "title-text")
    ),

    # Responsive layout
    layout_columns(
      fillable = TRUE,
      col_widths = c(
        lg = c(3, 9),
        md = c(4, 8),
        sm = c(12, 12)
      ),

      # Sidebar Panel
      div(
        class = "hfv-sidebar",
        
        # Year selector
        div(
          style = "margin-bottom: 10px;",
          selectInput("year", "Year:", 
                     choices = c(2018:2024), 
                     selected = 2024, 
                     width = "100%", 
                     selectize = FALSE)
        ),
        
        # Loan purpose selector
        div(
          style = "margin-bottom: 10px;",
          selectInput("loan_purpose", "Loan Purpose:", 
                     choices = c("Home purchase", "Home improvement", "Refinancing", "Cash-out refinancing", "Other purpose"),
                     selected = "Home purchase", 
                     width = "100%", 
                     selectize = FALSE)
        ),
        
        # Occupancy type selector
        div(
          style = "margin-bottom: 10px;",
          selectInput("occupancy_type", "Occupancy Type:", 
                     choices = c("Principal residence", "Second residence", "Investment property"),
                     selected = "Principal residence", 
                     width = "100%", 
                     selectize = FALSE)
        ),
        
        # Horizontal line
        hr(style = "margin: 3px 0;"),

        # Source information
        div(
          style = "font-size: 10px; color: #666; margin-top: 2px;",
          p(
            "Source: Consumer Financial Protection Bureau, Home Mortgage Disclosure Act (HMDA) data.",
            style = "margin-bottom: 0;"
          )
        )
      ),

      # Main Panel with single plot
      div(
        div(class = "girafe-container", girafeOutput("plot", height = "100%"))
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
  hmda_data <- reactive({
    read_parquet("hmda_va_clean.parquet")
  })

  # Filter data based on current geography
  filtered_data <- reactive({
    req(input$year, input$loan_purpose, input$occupancy_type)
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      hmda_data() %>%
        filter(
          activity_year == input$year,
          loan_purpose == input$loan_purpose,
          occupancy_type == input$occupancy_type,
          cbsa_title == geo$cbsa
        ) %>%
        group_by(race_ethnicity) %>%
        summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(count))
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      hmda_data() %>%
        filter(
          activity_year == input$year,
          loan_purpose == input$loan_purpose,
          occupancy_type == input$occupancy_type,
          name_long == geo$locality
        ) %>%
        group_by(race_ethnicity) %>%
        summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(count))
    } else {
      hmda_data() %>%
        filter(
          activity_year == input$year,
          loan_purpose == input$loan_purpose,
          occupancy_type == input$occupancy_type
        ) %>%
        group_by(race_ethnicity) %>%
        summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(count))
    }
  })

  # Plot title based on geography
  plot_title <- reactive({
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      paste("Mortgage Loans by Race/Ethnicity -", geo$cbsa, "-", input$year)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      paste("Mortgage Loans by Race/Ethnicity -", geo$locality, "-", input$year)
    } else {
      paste("Virginia Mortgage Loans by Race/Ethnicity -", input$year)
    }
  })
  
  # Function to create mortgage plots
  create_mortgage_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", race_ethnicity, "\n",
        "Loan Count: ", format(count, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = reorder(race_ethnicity, count),
                    y = count,
                    fill = race_ethnicity)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = race_ethnicity),
        width = 0.7
      ) +
      scale_fill_manual(values = race_ethnicity_colors, na.value = "#CCCCCC") +
      coord_flip() +
      scale_y_continuous(labels = comma_format()) +
      labs(
        title = title_text,
        caption = " ",
        y = "Number of Loans",
        x = "Race/Ethnicity"
      ) +
      theme_minimal(base_family = "Arial") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 30, 5)
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
  
  # Convert to interactive girafe
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
      )
    )
  }
  
  # Render the plot
  output$plot <- renderGirafe({
    create_interactive_plot(create_mortgage_plot(filtered_data(), plot_title()))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")