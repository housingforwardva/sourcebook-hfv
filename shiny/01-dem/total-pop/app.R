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

# =============================================================================
# HFV STYLING SYSTEM INTEGRATION
# =============================================================================

# Compile HFV styles if needed (for deployment compatibility)
compile_hfv_styles_if_needed <- function() {
  css_file <- "../../www/styles/hfv-theme.css"
  scss_file <- "../../www/styles/hfv-theme.scss"
  
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

# Define UI
ui <- page_fillable(
  theme = hfv_theme,
  useShinyjs(), # Initialize shinyjs

  # Head section with styles and meta tags
  tags$head(
    # Mobile viewport
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    ),
    
    # Include HFV CSS if it exists, otherwise use fallback
    if (compile_hfv_styles_if_needed()) {
      tags$link(rel = "stylesheet", href = "../../www/styles/hfv-theme.css")
    } else {
      # Fallback: minimal inline CSS with HFV colors and reliable fonts
      tags$style(HTML("
        body { font-family: 'Open Sans', 'Helvetica Neue', Helvetica, Arial, sans-serif !important; }
        h1, h2, h3, h4, h5, h6 { font-family: 'Poppins', 'Helvetica Neue', Helvetica, Arial, sans-serif !important; }
        .hfv-container { max-width: 1200px; margin: 0 auto; padding: 24px; }
        .hfv-header { display: flex; align-items: center; margin-bottom: 24px; padding-bottom: 8px; border-bottom: 2px solid #40C0C0; }
        .hfv-logo { height: 24px; margin-right: 16px; }
        .hfv-title { margin: 0; color: #011E41; font-family: 'Poppins', 'Helvetica Neue', Helvetica, Arial, sans-serif !important; }
        .hfv-sidebar { background-color: #E8EDF2; padding: 16px; border-radius: 6px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .hfv-chart-container { width: 100%; height: auto; min-height: 350px; }
        .text-primary { color: #40C0C0 !important; }
        @media (max-height: 600px) { .hfv-container { padding: 10px !important; max-height: 500px !important; } .hfv-chart-container { min-height: 280px !important; } }
        @media (max-width: 768px) { .hfv-container { padding: 8px; } .hfv-sidebar { padding: 10px; margin-bottom: 10px; } }
        @media (max-width: 480px) { .hfv-container { padding: 5px; } .hfv-logo { height: 20px; } }
      "))
    }
  ),

  # Main container using HFV classes
  div(
    class = "hfv-container",
    
    # Header using HFV styling
    div(
      class = "hfv-header",
      h4("Total Population", class = "hfv-title")
    ),

    # Layout using bslib layout_columns
    layout_columns(
      col_widths = c(
        lg = c(3, 9),
        md = c(4, 8), 
        sm = c(12)
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
            selectInput("cbsa", "Metro Area:", choices = NULL, width = "100%", selectize = FALSE)
          ),
          conditionalPanel(
            condition = "input.tabs == 'local'",
            selectInput("locality", "Locality:", choices = NULL, width = "100%", selectize = FALSE)
          )
        ),
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "U.S. Census Bureau, Population Estimates Program and Decennial Census",
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


# Server function
server <- function(input, output, session) {
  # Load the data
  total_pop <- reactive({
    read_rds("./total_pop.rds")
  })
  
  # Pre-compute datasets
  state_data <- reactive({
    total_pop() %>% 
      group_by(year, counttype) %>% 
      summarise(value = sum(value), .groups = "drop")
  })
  
  cbsa_data <- reactive({
    total_pop() %>% 
      group_by(year, cbsa_title, counttype) %>% 
      summarise(value = sum(value), .groups = "drop")
  })
  
  locality_data <- reactive({
    total_pop()
  })
  
  # Get available CBSAs and localities
  cbsa_list <- reactive({
    sort(unique(cbsa_data()$cbsa_title))
  })
  
  locality_list <- reactive({
    sort(unique(locality_data()$name_long))
  })
  
  # Initialize dropdowns
  observe({
    # CBSAs
    updateSelectInput(session, "cbsa", 
                      choices = cbsa_list(),
                      selected = if("Richmond, VA" %in% cbsa_list()) "Richmond, VA" else cbsa_list()[1])
    
    # Localities
    updateSelectInput(session, "locality", 
                      choices = locality_list(),
                      selected = if("Richmond City" %in% locality_list()) "Richmond City" else locality_list()[1])
  })
  
  # Create filtered datasets
  filtered_cbsa <- reactive({
    req(input$cbsa)
    
    cbsa_data() %>%
      filter(cbsa_title == input$cbsa)
  })
  
  filtered_locality <- reactive({
    req(input$locality)
    
    locality_data() %>%
      filter(name_long == input$locality)
  })
  
  # Plot titles
  state_title <- reactive({
    "Virginia Population"
  })
  
  cbsa_title <- reactive({
    paste("Population of", input$cbsa, "CBSA")
  })
  
  locality_title <- reactive({
    paste("Population of", input$locality)
  })
  
  # Function to create population trend plots
  create_pop_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Year: ", year, "\n",
        "Population: ", format(value, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = factor(year),
                    y = value)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = year),
        fill = "#40C0C0",
        width = 0.7
      ) +
      geom_text(
        aes(label = format(value, big.mark = ",")),
        color = "white",
        size = 4,
        angle = 90,
        hjust = 1.5,
        vjust = 0.5
      ) +
      scale_y_continuous(labels = number_format(big.mark = ",")) +
      labs(
        title = title_text,
        caption = " ", # Add empty caption to leave space for logo
        y = "Population",
        x = "Year"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
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
      )
    )
  }
  
  # Render the plots
  output$state_plot <- renderGirafe({
    create_interactive_plot(create_pop_plot(state_data(), state_title()))
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_pop_plot(filtered_cbsa(), cbsa_title()))
  })
  
  output$local_plot <- renderGirafe({
    create_interactive_plot(create_pop_plot(filtered_locality(), locality_title()))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)