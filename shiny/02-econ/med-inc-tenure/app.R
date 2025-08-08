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
        
        h5("Dashboard Controls", 
           class = "text-primary", style = "margin-bottom: 16px;"),
        
        # Tenure selector
        div(
          style = "margin-bottom: 16px;",
          selectInput("tenure", "Select Tenure:", choices = NULL, width = "100%", selectize = FALSE)
        ),
        
        # Dollar type toggle
        div(
          style = "margin-bottom: 16px;",
          radioButtons("dollar_type", "Dollar Type:",
                       choices = list("Current Dollars" = "estimate", 
                                      "Inflation-Adjusted" = "adjusted"),
                       selected = "adjusted",
                       inline = FALSE)
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
            "U.S. Census Bureau, American Community Survey 5-Year Estimates, Table B25119",
            style = "margin-bottom: 0;"
          ),
          conditionalPanel(
            condition = "input.dollar_type == 'adjusted'",
            p(
              strong("Note:"), "Income adjusted to 2023 dollars using CPI",
              style = "margin-bottom: 0; margin-top: 8px;"
            )
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
  state_data <- reactive({
    read_rds(here("data", "rds", "b25119_state.rds")) %>% 
      mutate(year = as.character(year))
  })
  
  cbsa_data <- reactive({
    read_rds(here("data", "rds", "b25119_cbsa.rds")) %>% 
      mutate(year = as.character(year))
  })
  
  local_data <- reactive({
    read_rds(here("data", "rds", "b25119_local.rds")) %>% 
      mutate(year = as.character(year))
  })
  
  # Get available options
  state_list <- reactive({
    sort(unique(state_data()$state))
  })
  
  cbsa_list <- reactive({
    sort(unique(cbsa_data()$cbsa))
  })
  
  locality_list <- reactive({
    sort(unique(local_data()$locality))
  })
  
  tenure_list <- c("All households", "Homeowner", "Renter")
  
  # Initialize dropdowns
  observe({
    # Tenure
    updateSelectInput(session, "tenure", 
                      choices = tenure_list,
                      selected = "All households")
    
    # States
    updateSelectInput(session, "state_select", 
                      choices = state_list(),
                      selected = if("Virginia" %in% state_list()) "Virginia" else state_list()[1])
    
    # CBSAs
    updateSelectInput(session, "cbsa_select", 
                      choices = cbsa_list(),
                      selected = if("Richmond, VA" %in% cbsa_list()) "Richmond, VA" else cbsa_list()[1])
    
    # Localities
    updateSelectInput(session, "locality_select", 
                      choices = locality_list(),
                      selected = if("Richmond City" %in% locality_list()) "Richmond City" else locality_list()[1])
  })
  
  # Create filtered datasets
  filtered_state <- reactive({
    req(input$state_select, input$tenure)
    
    state_data() %>%
      filter(state == input$state_select,
             tenure == input$tenure)
  })
  
  filtered_cbsa <- reactive({
    req(input$cbsa_select, input$tenure)
    
    cbsa_data() %>%
      filter(cbsa == input$cbsa_select,
             tenure == input$tenure)
  })
  
  filtered_locality <- reactive({
    req(input$locality_select, input$tenure)
    
    local_data() %>%
      filter(locality == input$locality_select,
             tenure == input$tenure)
  })
  
  # Plot titles
  state_title <- reactive({
    paste("Median Household Income in", input$state_select)
  })
  
  cbsa_title <- reactive({
    paste("Median Household Income in", input$cbsa_select)
  })
  
  locality_title <- reactive({
    paste("Median Household Income in", input$locality_select)
  })
  
  # Function to create interactive line plots
  create_line_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Get selected dollar type
    value_col <- input$dollar_type
    
    # Determine y-axis label based on dollar type
    y_label <- if(value_col == "adjusted") {
      "Median Household Income (2023 Dollars)"
    } else {
      "Median Household Income (Current Dollars)"
    }
    
    # Add tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Year: ", year, "\n",
        "Tenure: ", tenure, "\n",
        "Income: ", scales::dollar(get(value_col))
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = year,
                    y = .data[[value_col]],
                    color = tenure,
                    group = tenure)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = paste(year, tenure)),
        size = 3
      ) +
      scale_color_manual(values = c(
        "All households" = "#011E41",
        "Homeowner" = "#40C0C0",
        "Renter" = "#B1005F"
      )) +
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
  
  # Render the plots
  output$state_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_line_plot(filtered_state(), state_title())))
  })
  
  output$cbsa_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_line_plot(filtered_cbsa(), cbsa_title())))
  })
  
  output$local_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_line_plot(filtered_locality(), locality_title())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)