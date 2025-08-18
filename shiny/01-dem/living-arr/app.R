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
      h4("Living Arrangements of Adults", class = "hfv-title")
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
        
        # Age group select
        div(
          style = "margin-bottom: 16px;",
          selectInput("age", "Select Age Group:", 
                      choices = NULL, 
                      selected = NULL, 
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
            "U.S. Census Bureau, American Community Survey 5-Year Estimates",
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

# Load data outside server for faster loading
lvng_arr <- readRDS("b09021_data.rds")

# Create lists for filters
year_list <- sort(unique(lvng_arr$year), decreasing = TRUE)
age_list <- sort(unique(lvng_arr$age))
cbsa_list <- sort(unique(lvng_arr$cbsa_title))
locality_list <- sort(unique(lvng_arr$name_long))

# Pre-aggregate data
# Locality data
locality_la <- lvng_arr %>% 
  group_by(year, name_long, age, type) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>%
  group_by(year, name_long, age) %>% 
  mutate(percent = estimate/sum(estimate))

# CBSA data
cbsa_la <- lvng_arr %>% 
  group_by(year, cbsa_title, age, type) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>% 
  group_by(year, cbsa_title, age) %>% 
  mutate(percent = estimate/sum(estimate))

# State data
state_la <- lvng_arr %>% 
  group_by(year, age, type) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>% 
  group_by(year, age) %>% 
  mutate(percent = estimate/sum(estimate))

# Server function
server <- function(input, output, session) {
  
  # Initialize dropdowns
  observe({
    updateSelectInput(session, "year", 
                      choices = year_list,
                      selected = max(year_list))
    
    updateSelectInput(session, "age", 
                      choices = age_list,
                      selected = "18 to 34")
    
    # CBSAs
    updateSelectInput(session, "cbsa", 
                      choices = cbsa_list,
                      selected = if("Richmond, VA" %in% cbsa_list) "Richmond, VA" else cbsa_list[1])
    
    # Localities
    updateSelectInput(session, "locality", 
                      choices = locality_list,
                      selected = if("Richmond City" %in% locality_list) "Richmond City" else locality_list[1])
  })
  
  # Filter data for plots
  filtered_state <- reactive({
    req(input$year, input$age)
    
    state_la %>%
      filter(year == input$year,
             age == input$age)
  })
  
  filtered_cbsa <- reactive({
    req(input$year, input$age, input$cbsa)
    
    cbsa_la %>%
      filter(year == input$year,
             age == input$age,
             cbsa_title == input$cbsa)
  })
  
  filtered_locality <- reactive({
    req(input$year, input$age, input$locality)
    
    locality_la %>%
      filter(year == input$year,
             age == input$age,
             name_long == input$locality)
  })
  
  # Create subtitle text
  state_subtitle <- reactive({
    paste("Virginia -", input$year, "-", input$age)
  })
  
  cbsa_subtitle <- reactive({
    paste(input$cbsa, "-", input$year, "-", input$age)
  })
  
  locality_subtitle <- reactive({
    paste(input$locality, "-", input$year, "-", input$age)
  })
  
  # Helper function for creating interactive plots
  create_plot <- function(data, subtitle) {
    req(nrow(data) > 0)
    
    # Add tooltips to the data
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Living Arrangement: ", type, "\n",
        "Percentage: ", scales::percent(percent, accuracy = 0.1), "\n",
        "Count: ", format(estimate, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = reorder(type, percent),
                    y = percent,
                    fill = type)) + 
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = type)
      ) +
      # Match text color to bar fill color
      geom_text(aes(label = scales::percent(percent, accuracy = 1),
                    color = type),
                position = position_dodge(width = 0.9),
                vjust = -0.5,
                size = 3.5) +
      # Use the HFV colors for fill and text
      scale_fill_manual(values = c(
        "Lives alone" = hfv_colors$sky,
        "Lives with married or unmarried partner" = hfv_colors$grass,
        "Lives with other nonrelative(s)" = hfv_colors$lilac,
        "Lives with other relative(s)" = hfv_colors$shadow,
        "Lives with parent(s)" = hfv_colors$berry
      )) +
      scale_color_manual(values = c(
        "Lives alone" = hfv_colors$sky,
        "Lives with married or unmarried partner" = hfv_colors$grass,
        "Lives with other nonrelative(s)" = hfv_colors$lilac,
        "Lives with other relative(s)" = hfv_colors$shadow,
        "Lives with parent(s)" = hfv_colors$berry
      )) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = "Living Arrangements of Adults",
        subtitle = subtitle,
        caption = " ", # Add empty caption to leave space for logo
        y = "Percentage",
        x = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, lineheight = 0.8),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 30, 5) # Extra bottom margin for logo
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    
    # Add logo using local file instead of external URL for better performance
    tryCatch({
      logo_path <- "www/hfv_rgb_logo.png"
      if (file.exists(logo_path)) {
        p_with_logo <- ggdraw(p) +
          draw_image(
            logo_path,
            x = 0.85, # Horizontal position (right side)
            y = 0.05, # Vertical position (bottom)
            width = 0.15,
            height = 0.15
          )
      } else {
        p_with_logo <- p  # Return plot without logo if file doesn't exist
      }
    }, error = function(e) {
      p_with_logo <- p  # Return plot without logo on error
    })
    
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
    suppressWarnings(create_interactive_plot(create_plot(filtered_state(), state_subtitle())))
  })
  
  output$cbsa_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_plot(filtered_cbsa(), cbsa_subtitle())))
  })
  
  output$local_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_plot(filtered_locality(), locality_subtitle())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)