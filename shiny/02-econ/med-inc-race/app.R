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
      h4("Median Household Income by Race/Ethnicity", class = "hfv-title")
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
          selectInput("year", "Select Year:", choices = NULL, width = "100%", selectize = FALSE)
        ),
        
        # Show inflation-adjusted option
        div(
          style = "margin-bottom: 16px;",
          checkboxInput("adjusted", "Show Inflation-Adjusted", FALSE)
        ),
        
        # Geography selectors
        div(
          style = "margin-bottom: 16px;",
          conditionalPanel(
            condition = "input.tabs == 'state'",
            selectInput("state_select", "Select State:", choices = NULL, width = "100%", selectize = FALSE)
          ),
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            selectInput("cbsa_select", "Metro Area:", choices = NULL, width = "100%", selectize = FALSE)
          ),
          conditionalPanel(
            condition = "input.tabs == 'local'",
            selectInput("locality_select", "Locality:", choices = NULL, width = "100%", selectize = FALSE)
          )
        ),
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Data source
        div(
          style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
          p(
            strong("Data Source:"), br(),
            "U.S. Census Bureau, American Community Survey 5-Year Estimates, Table B19013",
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
  state_inc_data <- reactive({
    read_rds(here("data", "rds", "b19013_state.rds"))
  })
  
  cbsa_inc_data <- reactive({
    read_rds(here("data", "rds", "b19013_cbsa.rds"))
  })
  
  locality_inc_data <- reactive({
    read_rds(here("data", "rds", "b19013_locality.rds"))
  })
  
  # Create color vector for races
  race_colors <- c(
    "White alone, not Hispanic" = "#40C0C0",
    "Black alone" = "#011E41",
    "Asian alone" = "#259591",
    "Hispanic (any race)" = "#E0592A",
    "Two or more races" = "#B1005F",
    "American Indian alone" = "#8B85CA",
    "Pacific Islander alone" = "#FFC658",
    "Some other race alone" = "#FF7276"
  )
  
  # Get available options
  state_list <- reactive({
    sort(unique(state_inc_data()$state))
  })
  
  cbsa_list <- reactive({
    sort(unique(cbsa_inc_data()$CBSA))
  })
  
  locality_list <- reactive({
    sort(unique(locality_inc_data()$locality))
  })
  
  year_list <- reactive({
    sort(unique(state_inc_data()$year), decreasing = TRUE)
  })
  
  # Initialize dropdowns
  observe({
    # Years
    updateSelectInput(session, "year", 
                      choices = year_list(),
                      selected = max(year_list()))
    
    # States
    updateSelectInput(session, "state_select", 
                      choices = state_list(),
                      selected = if("Virginia" %in% state_list()) "Virginia" else state_list()[1])
    
    # CBSAs
    updateSelectInput(session, "cbsa_select", 
                      choices = cbsa_list(),
                      selected = if("Richmond, VA Metro Area" %in% cbsa_list()) "Richmond, VA Metro Area" else cbsa_list()[1])
    
    # Localities
    updateSelectInput(session, "locality_select", 
                      choices = locality_list(),
                      selected = if("Richmond city" %in% locality_list()) "Richmond city" else locality_list()[1])
  })
  
  # Get filtered data based on selected tab and inputs
  filtered_state <- reactive({
    req(input$state_select, input$year)
    
    state_inc_data() %>%
      filter(
        state == input$state_select,
        year == input$year
      )
  })
  
  filtered_cbsa <- reactive({
    req(input$cbsa_select, input$year)
    
    cbsa_inc_data() %>%
      filter(
        CBSA == input$cbsa_select,
        year == input$year
      )
  })
  
  filtered_locality <- reactive({
    req(input$locality_select, input$year)
    
    locality_inc_data() %>%
      filter(
        locality == input$locality_select,
        year == input$year
      )
  })
  
  # Plot titles
  state_title <- reactive({
    paste("Median Household Income by Race/Ethnicity in", input$state_select, "(", input$year, ")")
  })
  
  cbsa_title <- reactive({
    paste("Median Household Income by Race/Ethnicity in", input$cbsa_select, "(", input$year, ")")
  })
  
  locality_title <- reactive({
    paste("Median Household Income by Race/Ethnicity in", input$locality_select, "(", input$year, ")")
  })
  
  # Y-axis label based on inflation adjustment
  y_label <- reactive({
    if(input$adjusted) {
      "Median Household Income (Inflation-Adjusted)"
    } else {
      "Median Household Income"
    }
  })
  
  # Function to create interactive bar plots
  create_bar_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Select which value to plot based on checkbox
    value_col <- if(input$adjusted) "adjusted" else "estimate"
    
    # Filter out NA values
    plot_data <- data %>% 
      drop_na() %>%
      # Use the value column to order the races
      mutate(race = factor(race, levels = race[order(get(value_col))]))
    
    # Add tooltips
    plot_data <- plot_data %>%
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", race, "\n",
        "Income: ", scales::dollar(get(value_col))
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(
                  x = race,
                  y = .data[[value_col]],
                  fill = race)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = race)
      ) +
      # Add the value labels at the end of each bar
      geom_text(aes(label = scales::dollar(.data[[value_col]], accuracy = 1)),
                hjust = -0.2,
                color = "#333333",
                size = 3) +
      # Set the fill colors
      scale_fill_manual(values = race_colors) +
      # Extend the plot area to make room for labels
      coord_flip(clip = "off") +
      # Format y-axis with dollar signs
      scale_y_continuous(
        labels = scales::dollar_format(),
        limits = function(x) c(0, max(x) * 1.2)  # Add 20% headroom for labels
      ) +
      labs(
        title = title_text,
        caption = " ",  # Empty caption for logo space
        x = NULL,
        y = y_label()
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 20, 30, 5)  # Extra right margin for labels, bottom for logo
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
  
  # Render plots
  output$state_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_bar_plot(filtered_state(), state_title())))
  })
  
  output$cbsa_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_bar_plot(filtered_cbsa(), cbsa_title())))
  })
  
  output$local_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_bar_plot(filtered_locality(), locality_title())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)