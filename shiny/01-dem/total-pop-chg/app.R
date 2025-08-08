library(shiny)
library(tidyverse)
library(ggiraph)     # For interactive ggplots
library(systemfonts) # For font_google
library(here)        # For here() function in file paths
library(grid)        # For grobs
library(png)         # For reading PNG files
library(bslib)       # For modern UI components
library(cowplot)     # For adding logo to plots
library(scales)      # For number formatting
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


# Server function
server <- function(input, output, session) {
  # Load the data (non-reactive, load once)
  total_pop <- read_rds("./total_pop.rds")
  
  # Pre-compute datasets
  state_data <- total_pop %>% 
    group_by(year) %>% 
    summarise(value = sum(value), .groups = "drop") %>% 
    ungroup() %>% 
    mutate(diff = value - lag(value),
           diff = replace_na(diff, 0)) %>% 
    mutate(run_diff = cumsum(diff)) %>% 
    filter(run_diff != 0) %>% 
    mutate(pct = run_diff/first(value))
  
  cbsa_data <- total_pop %>% 
    group_by(year, cbsa_title, counttype) %>% 
    summarise(value = sum(value), .groups = "drop")
  
  # Get available CBSAs and localities
  cbsa_list <- sort(unique(cbsa_data$cbsa_title))
  locality_list <- sort(unique(total_pop$name_long))
  
  # Initialize dropdowns
  observe({
    # CBSAs
    updateSelectInput(session, "cbsa", 
                      choices = cbsa_list,
                      selected = if("Richmond, VA" %in% cbsa_list) "Richmond, VA" else cbsa_list[1])
    
    # Localities
    updateSelectInput(session, "locality", 
                      choices = locality_list,
                      selected = if("Richmond City" %in% locality_list) "Richmond City" else locality_list[1])
  })
  
  # Create filtered datasets
  filtered_cbsa <- reactive({
    req(input$cbsa)
    
    cbsa_data %>%
      filter(cbsa_title == input$cbsa) %>% 
      group_by(year, counttype) %>% 
      summarise(value = sum(value), .groups = "drop") %>% 
      ungroup() %>% 
      mutate(diff = value - lag(value),
             diff = replace_na(diff, 0)) %>% 
      mutate(run_diff = cumsum(diff)) %>%  
      filter(run_diff != 0) %>% 
      mutate(pct = run_diff/first(value))
  })
  
  filtered_locality <- reactive({
    req(input$locality)
    
    total_pop %>%
      filter(name_long == input$locality) %>% 
      mutate(diff = value - lag(value),
             diff = replace_na(diff, 0)) %>% 
      mutate(run_diff = cumsum(diff)) %>%  
      filter(run_diff != 0) %>% 
      mutate(pct = run_diff/first(value))
  }) 
  
  # Plot titles
  state_title <- reactive({
    "Virginia Population Change Since 2010"
  })
  
  cbsa_title <- reactive({
    paste("Population Change Since 2010 in", input$cbsa)
  })
  
  locality_title <- reactive({
    paste("Population Change Since 2010 in", input$locality)
  })
  
  # Function to create population change plots
  create_pop_change_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Year: ", year, "\n",
        "Change: ", scales::percent(pct, accuracy = 0.1)
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = year,
                    y = pct,
                    group = 1)) +
      geom_line(color = hfv_colors$shadow, linewidth = 1) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = year),
        size = 3,
        color = hfv_colors$shadow
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = title_text,
        caption = " ", # Add empty caption to leave space for logo
        y = "Population Change (%)",
        x = "Year"
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
    create_interactive_plot(create_pop_change_plot(state_data, state_title()))
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_pop_change_plot(filtered_cbsa(), cbsa_title()))
  })
  
  output$local_plot <- renderGirafe({
    create_interactive_plot(create_pop_change_plot(filtered_locality(), locality_title()))
  })

  # MOBILE OPTIMIZATION #9: Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}



# Run the application 
shinyApp(ui = ui, server = server)