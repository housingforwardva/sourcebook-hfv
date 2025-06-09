library(shiny)
library(tidyverse)
library(ggiraph)     # For interactive ggplots
library(systemfonts) # For font_google
library(here)        # For here() function in file paths
library(grid)        # For grobs
library(png)         # For reading PNG files
library(bslib)       # For modern UI components
library(cowplot)     # For adding logo to plots
library(scales)      # For number_format
library(ggtext)      # For formatted text in plots
library(shinyjs)     # For dynamic UI updates
library(magick)      # For image handling

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

# Create a Bootstrap theme
hfv_theme <- bs_theme(
  version = 5,                        # Use Bootstrap 5
  bg = "#ffffff",                     # Background color
  fg = "#333333",                     # Text color
  primary = hfv_colors$sky,           # Primary color
  secondary = hfv_colors$shadow,      # Secondary color
  success = hfv_colors$grass,         # Success color
  info = hfv_colors$lilac,            # Info color
  warning = hfv_colors$desert,        # Warning color
  danger = hfv_colors$berry,          # Danger color
  base_font = font_google("Open Sans"),
  heading_font = font_google("Poppins"),
  font_scale = 0.8                    # Compact the text more for small window
)

# Load data outside of server
pop_change <- read_rds("./pop_change.rds")

# Create lists for filters
cbsa_list <- sort(unique(pop_change$cbsa_title))
locality_list <- sort(unique(pop_change$name_long))

# Pre-process data
cbsa_pop <- pop_change %>% 
  group_by(year, cbsa_title, component) %>% 
  summarise(value = sum(value), .groups = "drop")

state_pop <- pop_change %>% 
  group_by(year, component) %>% 
  summarise(value = sum(value), .groups = "drop")

# Create color-coded subtitle
subtitle_text <- "Net <span style='color:#011E41'><b>domestic migration</b></span>, <span style='color:#40C0C0'><b>international migration</b></span>, and <span style='color:#8B85CA'><b>natural increase (or decrease)</b></span>"

# Define UI
ui <- page_fillable(
  theme = hfv_theme,
  useShinyjs(), # Initialize shinyjs

  # MOBILE OPTIMIZATION #1: Add the viewport meta tag for mobile devices
  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    )
  ),

  # MOBILE OPTIMIZATION #2: Add CSS with media queries for responsive design
  tags$head(
    tags$style(HTML(
      "
      /* Base styles for all screen sizes */
      body, html {
        margin: 0;
        padding: 0;
        height: 100vh;
        overflow-x: hidden;
      }
      
      /* Container styles */
      .hfv-container {
        max-width: 1200px; 
        margin: 0 auto; 
        border: 2px solid #011E41; 
        border-radius: 5px; 
        padding: 45px;
      }
      
      /* Header styles */
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
      
      /* Sidebar panel styles */
      .hfv-sidebar {
        background-color: #E8EDF2;
        padding: 15px;
        border-radius: 5px;
      }
      
      /* Plot container styles */
      .girafe-container {
        width: 100%;
        height: 450px;
        overflow: visible;
      }
      
      .girafe-container svg {
        width: 100% !important;
        height: 100% !important;
      }
      
      /* MOBILE OPTIMIZATION #3: Medium-sized screens (tablets, smaller laptops) */
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
      
      /* MOBILE OPTIMIZATION #4: Small screens (large phones, small tablets) */
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
      
      /* MOBILE OPTIMIZATION #5: Extra-small screens (phones) */
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
      }
    "
    ))
  ),

  # Main container with responsive padding
  div(
    class = "hfv-container",

    # Header with logo and title
    div(
      class = "hfv-header",
      img(
        src = "https://housingforwardva.org/wp-content/uploads/2025/05/HousingForward-VA-Logo-Files-Icon-One-Color-RGB.png",
        alt = "HousingForward VA Logo"
      ),
      h4("Components of Population Change", class = "title-text")
    ),

    # MOBILE OPTIMIZATION #6: Responsive grid layout with different column widths for different screen sizes
    layout_columns(
      fillable = TRUE,
      col_widths = c(
        # For larger screens (lg and up): sidebar takes 25% width, main content takes 75%
        lg = c(3, 9),
        # For medium screens (md): sidebar takes 33% width, main content takes 67%
        md = c(4, 8),
        # For small screens (sm and xs): full width stacked layout
        sm = c(12, 12)
      ),

      # Sidebar Panel
      div(
        class = "hfv-sidebar",

        # Geography selectors
        div(
          style = "margin-bottom: 15px;",
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            selectInput(
              "cbsa_select", 
              "Metro Area:",
              choices = cbsa_list,
              selected = if("Richmond, VA" %in% cbsa_list) "Richmond, VA" else cbsa_list[1],
              width = "100%",
              selectize = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.tabs == 'local'",
            selectInput(
              "locality_select",
              "Locality:",
              choices = locality_list,
              selected = if("Richmond City" %in% locality_list) "Richmond City" else locality_list[1],
              width = "100%",
              selectize = TRUE
            )
          )
        ),

        # Component legend
        div(
          style = "margin-bottom: 15px;",
          div(
            style = "font-size: 12px;",
            HTML("<div style='margin-bottom: 5px;'><span style='display: inline-block; width: 12px; height: 12px; background-color: #011E41; margin-right: 8px;'></span>Domestic Migration</div>"),
            HTML("<div style='margin-bottom: 5px;'><span style='display: inline-block; width: 12px; height: 12px; background-color: #40C0C0; margin-right: 8px;'></span>International Migration</div>"),
            HTML("<div style='margin-bottom: 5px;'><span style='display: inline-block; width: 12px; height: 12px; background-color: #8B85CA; margin-right: 8px;'></span>Natural Increase/Decrease</div>")
          )
        ),

        # Horizontal line
        hr(style = "margin: 15px 0;"),

        # Source information
        div(
          style = "font-size: 10px; color: #666; margin-top: 8px;",
          p("Source: U.S. Census Bureau, Population Estimates Program and Decennial Census.")
        )
      ),

      # Main Panel (tabs)
      div(
        style = "width: 100%;",

        navset_tab(
          id = "tabs",
          nav_panel(
            title = "State",
            value = "state",
            padding = 5,
            # MOBILE OPTIMIZATION #7: Direct plot output without uiOutput wrappers
            div(class = "girafe-container", girafeOutput("state_plot"))
          ),
          nav_panel(
            title = "Metro Area",
            value = "cbsa",
            padding = 5,
            div(class = "girafe-container", girafeOutput("cbsa_plot"))
          ),
          nav_panel(
            title = "Locality",
            value = "local",
            padding = 5,
            div(class = "girafe-container", girafeOutput("local_plot"))
          )
        )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Create filtered datasets
  filtered_cbsa <- reactive({
    req(input$cbsa_select)
    
    cbsa_pop %>%
      filter(cbsa_title == input$cbsa_select)
  })
  
  filtered_locality <- reactive({
    req(input$locality_select)
    
    pop_change %>%
      filter(name_long == input$locality_select)
  })
  
  # Plot titles
  state_title <- reactive({
    "Virginia Components of Population Change"
  })
  
  cbsa_title <- reactive({
    paste("Components of Population Change in", input$cbsa_select)
  })
  
  locality_title <- reactive({
    paste("Components of Population Change in", input$locality_select)
  })
  
  # Function to create stacked bar plots for population components
  create_pop_change_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Year: ", year, "\n",
        "Component: ", component, "\n",
        "Change: ", format(value, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = year,
                    y = value,
                    fill = component)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = paste(year, component)),
        position = "stack"
      ) +
      scale_fill_manual(values = c(
        "Domestic migration" = hfv_colors$shadow,
        "International migration" = hfv_colors$sky,
        "Natural increase" = hfv_colors$lilac
      )) +
      scale_y_continuous(labels = number_format(big.mark = ",")) +
      labs(
        title = title_text,
        caption = " ", # Add empty caption to leave space for logo
        y = "Population Change",
        x = NULL
      ) +
      theme_minimal(base_family = "Arial") +
      theme(
        legend.position = "none",
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
      width_svg = 8, # Set explicit width
      height_svg = 5, # Set explicit height
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
    create_interactive_plot(create_pop_change_plot(state_pop, state_title()))
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_pop_change_plot(filtered_cbsa(), cbsa_title()))
  })
  
  output$local_plot <- renderGirafe({
    create_interactive_plot(create_pop_change_plot(filtered_locality(), locality_title()))
  })

  # MOBILE OPTIMIZATION #8: Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)