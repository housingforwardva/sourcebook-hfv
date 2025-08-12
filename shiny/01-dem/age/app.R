# Population by Age Visualization ----------------------------------------------

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

# Define HFV color palette (matching SCSS variables)
hfv_colors <- list(
  sky = "#40C0C0",           # Primary teal
  grass = "#259591",         # Dark teal 
  lilac = "#8B85CA",         # Purple
  shadow = "#011E41",        # Dark navy
  shadow_light = "#102C54",  # Lighter navy
  berry = "#B1005F",         # Magenta
  desert = "#E0592A"         # Orange
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
      h4("Population by Age", class = "hfv-title")
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
        
                # Year selector
        div(
          style = "margin-bottom: 16px;",
          selectInput("year", "Select Year:", 
                      choices = 2010:2023, 
                      selected = 2023, 
                      width = "100%",
                      selectize = FALSE)
        ),
        
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
# Streamlined server function with reduced redundancy
server <- function(input, output, session) {
  # Load the data (only once)
  pop_age <- reactive({
    readRDS("pop_age.rds")
  })
  
  # Define age group order (move to global or make reactive if it might change)
  age_order <- c("Under 10", "10 to 17", "18 to 24", "25 to 29", "30 to 34", 
                 "35 to 44", "45 to 54", "55 to 64", "65 to 74", "75 and over")
  
  # Get available choices (reactive because we need to call pop_age())
  cbsa_list <- reactive({
    sort(unique(pop_age()$cbsa_title))
  })
  
  locality_list <- reactive({
    sort(unique(pop_age()$name_long))
  })
  
  # Initialize dropdowns
  observe({
    cbsa_choices <- cbsa_list()
    locality_choices <- locality_list()
    
    updateSelectInput(session, "cbsa", 
                      choices = cbsa_choices,
                      selected = if("Richmond, VA" %in% cbsa_choices) "Richmond, VA" else cbsa_choices[1])
    
    updateSelectInput(session, "locality", 
                      choices = locality_choices,
                      selected = if("Richmond City" %in% locality_choices) "Richmond City" else locality_choices[1])
  })
  
  # Single reactive for filtered data by geography type and year
  filtered_data <- reactive({
    req(input$year)
    
    base_data <- pop_age() %>%
      filter(year == input$year) %>%
      mutate(agegroup = factor(agegroup, levels = age_order))
    
    # Return a list with all three data types
    list(
      state = base_data %>%
        group_by(agegroup) %>%
        summarise(value = sum(value), .groups = "drop"),
      
      cbsa = if (!is.null(input$cbsa)) {
        base_data %>%
          filter(cbsa_title == input$cbsa) %>%
          group_by(agegroup) %>%
          summarise(value = sum(value), .groups = "drop")
      } else NULL,
      
      locality = if (!is.null(input$locality)) {
        base_data %>%
          filter(name_long == input$locality)
      } else NULL
    )
  })
  
  # Single function to create all plots
  create_age_plot <- function(data, title_text, subtitle_text = NULL) {
    req(nrow(data) > 0)
    
    # Add tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Age Group: ", agegroup, "\n",
        "Population: ", format(value, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data, aes(x = agegroup, y = value, fill = agegroup)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = agegroup),
        position = "stack"
      ) +
      scale_fill_manual(values = c(
        "Under 10" = hfv_colors$sky,
        "10 to 17" = hfv_colors$grass,
        "18 to 24" = hfv_colors$lilac,
        "25 to 29" = hfv_colors$shadow_light,
        "30 to 34" = hfv_colors$shadow,
        "35 to 44" = hfv_colors$berry,
        "45 to 54" = "#D3447E",
        "55 to 64" = hfv_colors$desert,
        "65 to 74" = "#F08A65",
        "75 and over" = "#FAC172"
      )) +
      scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
      labs(
        title = title_text,
        subtitle = subtitle_text %||% paste("Year:", input$year),
        caption = " ",
        y = "Population",
        x = "Age Group"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 30, 5)
      )
    
    # Add logo (single implementation)
    add_hfv_logo(p)
  }
  
  # Helper function for logo (extracted to reduce duplication)
  add_hfv_logo <- function(plot) {
    logo_url <- "https://housingforwardva.org/wp-content/uploads/2024/08/HousingForward-VA-Logo-Files-Horizontal-Gradient-RGB.png"
    
    ggdraw(plot) +
      draw_image(
        logo_url,
        x = 0.85, y = 0.05,
        width = 0.15, height = 0.15
      )
  }
  
  # Single function to create interactive plots
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
        opts_sizing(rescale = TRUE)
      )
    )
  }
  
  # Render plots using the streamlined approach
  output$state_plot <- renderGirafe({
    data <- filtered_data()$state
    req(data)
    plot <- create_age_plot(data, "Virginia Population by Age Group")
    create_interactive_plot(plot)
  })
  
  output$cbsa_plot <- renderGirafe({
    data <- filtered_data()$cbsa
    req(data, input$cbsa)
    title <- paste("Population by Age Group in", input$cbsa, "Metro")
    plot <- create_age_plot(data, title)
    create_interactive_plot(plot)
  })
  
  output$local_plot <- renderGirafe({
    data <- filtered_data()$locality
    req(data, input$locality)
    title <- paste("Population by Age Group in", input$locality)
    plot <- create_age_plot(data, title)
    create_interactive_plot(plot)
  })
  
  # Mobile optimization
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)