# Household Composition Visualization ------------------------------------------
# This app visualizes household composition data across Virginia
# with tabs for statewide, CBSA, and locality views

library(shiny)
library(tidyverse)
library(ggtext)
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

# Compile styles if needed
compile_hfv_styles_if_needed()

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
  shadow_light = "#102C54", # Lighter shade of shadow color
  berry = "#B1005F",
  desert = "#E0592A"
)



# UI
ui <- page_fillable(
  theme = hfv_theme,
  useShinyjs(), # Initialize shinyjs
  
  # Main container using HFV classes
  div(
    class = "hfv-container",
    
    # Header using HFV styling
    div(
      class = "hfv-header",
      h4("Household Composition", class = "hfv-title")
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
        
        # Year selector (common to all tabs)
        div(
          style = "margin-bottom: 15px;",
          selectInput(
            "selected_year",
            "Select Year:",
            choices = NULL,
            width = "100%",
            selectize = FALSE
          )
        ),
        
        # Geography selectors
        div(
          style = "margin-bottom: 15px;",
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            selectInput(
              "cbsa",
              "Select CBSA:",
              choices = NULL,
              width = "100%",
              selectize = FALSE
            )
          ),
          conditionalPanel(
            condition = "input.tabs == 'locality'",
            selectInput(
              "locality",
              "Select Locality:",
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
            "U.S. Census Bureau, 5-Year American Community Survey 5-year estimates, Table B11021.",
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
            value = "locality",
            div(
              class = "hfv-chart-container",
              style = "height: 450px; margin-top: 16px;",
              girafeOutput("locality_plot", height = "100%")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load the data with error handling
  if (!file.exists("b11012_data.rds")) {
    stop("Data file 'b11012_data.rds' not found. Please ensure it exists in the app directory.")
  }
  
  hh_type <- read_rds("b11012_data.rds")
  
  # Create lists (NOT reactive expressions)
  cbsa_list <- sort(unique(hh_type$cbsa_title))
  locality_list <- sort(unique(hh_type$name_long))
  year_list <- sort(unique(hh_type$year), decreasing = TRUE)
  
  
  # Initialize dropdowns - CORRECTED
  observe({
    updateSelectInput(session, "selected_year", 
                      choices = year_list,
                      selected = year_list[1])
    
    updateSelectInput(session, "cbsa", 
                      choices = cbsa_list,
                      selected = if("Richmond, VA" %in% cbsa_list) "Richmond, VA" else cbsa_list[1])
    
    updateSelectInput(session, "locality", 
                      choices = locality_list,
                      selected = if("Richmond City" %in% locality_list) "Richmond City" else locality_list[1])
  })
  
  # Create reactive expression for selected year
  selected_year <- reactive({
    req(input$selected_year)  # Ensure input exists
    input$selected_year
  })
  
  # Locality data - with better error handling
  locality_hh <- reactive({
    req(input$locality, selected_year())  # Require inputs
    
    year_selected <- selected_year()
    
    result <- hh_type %>% 
      filter(year == year_selected, name_long == input$locality) %>%
      group_by(type, sub) %>% 
      summarise(estimate = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
      group_by(type) %>%
      mutate(
        total_by_type = sum(estimate),
        percent = estimate / total_by_type,
        rank_within_type = rank(percent, ties.method = "first")
      ) %>%
      ungroup()
    
    # Debug: Print result info
    print(paste("Locality data for", input$locality, "in", year_selected))
    print(paste("Rows returned:", nrow(result)))
    
    return(result)
  })
  
  # CBSA data - with better error handling  
  cbsa_hh <- reactive({
    req(input$cbsa, selected_year())
    
    year_selected <- selected_year()
    
    result <- hh_type %>% 
      filter(year == year_selected, cbsa_title == input$cbsa) %>%
      group_by(type, sub) %>% 
      summarise(estimate = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
      group_by(type) %>%
      mutate(
        total_by_type = sum(estimate),
        percent = estimate / total_by_type,
        rank_within_type = rank(percent, ties.method = "first")
      ) %>%
      ungroup()
    
    print(paste("CBSA data for", input$cbsa, "in", year_selected))
    print(paste("Rows returned:", nrow(result)))
    
    return(result)
  })
  
  # State data - with better error handling
  state_hh <- reactive({
    req(selected_year())
    
    year_selected <- selected_year()
    
    result <- hh_type %>% 
      filter(year == year_selected) %>%
      group_by(type, sub) %>% 
      summarise(estimate = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
      group_by(type) %>%
      mutate(
        total_by_type = sum(estimate),
        percent = estimate / total_by_type,
        rank_within_type = rank(percent, ties.method = "first")
      ) %>%
      ungroup()
    
    print(paste("State data for", year_selected))
    print(paste("Rows returned:", nrow(result)))
    
    return(result)
  })
  
  # Generate title text
  title_text <- "<b><span style='color:#011E41'>Householder with no partner</span></b> and 
<b><span style='color:#40C0C0'>Married or cohabitating couple</span></b>"
  
  # Function to create interactive plots (consolidated)
  create_interactive_plot <- function(data, subtitle_text) {
    p <- ggplot(data,
                aes(x = reorder(sub, rank_within_type),
                    y = percent,
                    fill = type)) + 
      geom_col_interactive(
        aes(tooltip = paste0(sub, ": ", scales::percent(percent, accuracy = 0.1))),
        hover_nearest = TRUE
      ) +
      # Match text color to bar fill color
      geom_text_interactive(
        aes(label = scales::percent(percent, accuracy = 1),
            color = type,
            tooltip = paste0("Count: ", format(estimate, big.mark = ","))),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3.5
      ) +
      # Make sure text colors match fill colors
      scale_color_manual(values = c(hfv_colors$shadow, hfv_colors$sky)) +
      scale_fill_manual(values = c(hfv_colors$shadow, hfv_colors$sky)) +
      labs(title = title_text,
           subtitle = subtitle_text,
           caption = " ", # Empty caption to leave space for logo
           x = NULL,
           y = "Percent of Households") +
      scale_y_continuous(labels = scales::percent_format()) +
      # Hide the color legend since it's redundant with the fill legend
      guides(color = "none") +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title = element_markdown(),
        plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 30, 5) # Extra bottom margin for logo
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      facet_grid(cols = vars(type), scales = "free_x", space = "free") 
    
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
    
    girafe(
      ggobj = p_with_logo,
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
  
  # Create interactive plots for each tab
  output$state_plot <- renderGirafe({
    state_data <- state_hh()
    year_selected <- selected_year()
    create_interactive_plot(state_data, paste("Virginia:", year_selected))
  })
  
  output$cbsa_plot <- renderGirafe({
    cbsa_data <- cbsa_hh()
    year_selected <- selected_year()
    create_interactive_plot(cbsa_data, paste(input$cbsa, ":", year_selected))
  })
  
  output$locality_plot <- renderGirafe({
    locality_data <- locality_hh()
    year_selected <- selected_year()
    create_interactive_plot(locality_data, paste(input$locality, ":", year_selected))
  })
  
  
  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)