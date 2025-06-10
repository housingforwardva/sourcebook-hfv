# Household Composition Shiny App
# This app visualizes household composition data across Virginia
# with tabs for statewide, CBSA, and locality views

library(shiny)
library(tidyverse)
library(ggtext)
library(hdatools) 
library(ggiraph)
library(scales)
library(here)  # For better path handling
library(shinyWidgets) # Added for toggle switch
library(bslib) # For modern UI components
library(systemfonts) # For font_google
library(grid) # For grobs
library(png) # For reading PNG files
library(shinyjs) # For dynamic UI updates
library(cowplot) # For adding logo to plots
library(magick) # For image handling
library(gdtools) # For font registration

# Determine the app directory and set the data path
# Option 1: Using here package (recommended)
data_path <- here::here("data","rds", "hh_type.rds")

# Load the data with error handling
tryCatch({
  hh_type <- read_rds(data_path)
  # Create a list of all unique CBSAs and localities in Virginia
  cbsa_list <- sort(unique(hh_type$cbsa_title))
  locality_list <- sort(unique(hh_type$name_long))
  year_list <- sort(unique(hh_type$year), decreasing = TRUE)
}, error = function(e) {
  # This will be displayed when the app starts if there's an error loading the data
  stop(paste("Error loading data file:", e$message, 
             "\nPlease check the path to your data file. If your app is in a subdirectory of your project,",
             "you may need to adjust the path in the app.R file."))
})

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

# Register Google fonts
tryCatch({
  gdtools::register_gfont("Open Sans")
  gdtools::register_gfont("Poppins")
}, error = function(e) {
  message("Could not register Google fonts: ", e$message)
})

# Create a Bootstrap theme
hfv_theme <- bs_theme(
  version = 5, # Use Bootstrap 5
  bg = "#ffffff", # Background color
  fg = "#333333", # Text color
  primary = hfv_colors$sky, # Primary color
  secondary = hfv_colors$shadow, # Secondary color
  success = hfv_colors$grass, # Success color
  info = hfv_colors$lilac, # Info color
  warning = hfv_colors$desert, # Warning color
  danger = hfv_colors$berry, # Danger color
  base_font = font_google("Open Sans"),
  heading_font = font_google("Poppins"),
  font_scale = 0.8 # Compact the text more for small window
)

# UI
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
        height: auto;
        overflow-x: hidden;
      }
      
      /* Iframe optimization for 800x500 dimensions */
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
      
      /* Container styles */
      .hfv-container {
        max-width: 1200px; 
        margin: 0 auto; 
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
      h4("Household Composition", class = "title-text")
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

        # Year toggle for state
        div(
          style = "margin-bottom: 15px;",
          conditionalPanel(
            condition = "input.tabs == 'statewide'",
            switchInput(
              inputId = "state_year_toggle",
              label = paste("Toggle between", min(year_list), "and", max(year_list)),
              value = TRUE,
              onLabel = max(year_list),
              offLabel = min(year_list),
              size = "small",
              width = "100%"
            )
          ),
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            selectInput(
              "cbsa",
              "Select CBSA:",
              choices = cbsa_list,
              selected = cbsa_list[1],
              width = "100%",
              selectize = TRUE
            ),
            switchInput(
              inputId = "cbsa_year_toggle",
              label = "Switch Year",
              value = TRUE,
              onLabel = max(year_list),
              offLabel = min(year_list),
              size = "small",
              width = "100%"
            )
          ),
          conditionalPanel(
            condition = "input.tabs == 'locality'",
            selectInput(
              "locality",
              "Select Locality:",
              choices = locality_list,
              selected = locality_list[1],
              width = "100%",
              selectize = TRUE
            ),
            switchInput(
              inputId = "locality_year_toggle",
              label = paste("Toggle between", min(year_list), "and", max(year_list)),
              value = TRUE,
              onLabel = max(year_list),
              offLabel = min(year_list),
              size = "small",
              width = "100%"
            )
          ),
          conditionalPanel(
            condition = "input.tabs == 'compare'",
            selectInput(
              "compare_type",
              "Compare by:",
              choices = c("Years", "Localities"),
              selected = "Years",
              width = "100%",
              selectize = TRUE
            ),
            conditionalPanel(
              condition = "input.compare_type == 'Years'",
              selectInput(
                "compare_locality",
                "Select Locality:",
                choices = locality_list,
                selected = locality_list[1],
                width = "100%",
                selectize = TRUE
              ),
              selectInput(
                "compare_year1",
                "First Year:",
                choices = year_list,
                selected = year_list[2],
                width = "100%",
                selectize = TRUE
              ),
              selectInput(
                "compare_year2",
                "Second Year:",
                choices = year_list,
                selected = year_list[1],
                width = "100%",
                selectize = TRUE
              )
            ),
            conditionalPanel(
              condition = "input.compare_type == 'Localities'",
              switchInput(
                inputId = "compare_year_toggle",
                label = paste("Toggle between", min(year_list), "and", max(year_list)),
                value = TRUE,
                onLabel = max(year_list),
                offLabel = min(year_list),
                size = "small",
                width = "100%"
              ),
              selectInput(
                "compare_locality1",
                "First Locality:",
                choices = locality_list,
                selected = locality_list[1],
                width = "100%",
                selectize = TRUE
              ),
              selectInput(
                "compare_locality2",
                "Second Locality:",
                choices = locality_list,
                selected = locality_list[2],
                width = "100%",
                selectize = TRUE
              )
            )
          )
        ),

        # Download buttons
        div(
          style = "margin-bottom: 15px;",
          conditionalPanel(
            condition = "input.tabs == 'statewide'",
            downloadButton("download_state", "Download Plot", class = "btn-primary", width = "100%")
          ),
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            downloadButton("download_cbsa", "Download Plot", class = "btn-primary", width = "100%")
          ),
          conditionalPanel(
            condition = "input.tabs == 'locality'",
            downloadButton("download_locality", "Download Plot", class = "btn-primary", width = "100%")
          ),
          conditionalPanel(
            condition = "input.tabs == 'compare'",
            downloadButton("download_compare", "Download Plot", class = "btn-primary", width = "100%")
          )
        ),

        # Horizontal line
        hr(style = "margin: 15px 0;"),

        # Source information
        div(
          style = "font-size: 10px; color: #666; margin-top: 8px;",
          p("Source: U.S. Census Bureau, American Community Survey 5-year estimates, Table B11021.")
        )
      ),

      # Main Panel (tabs)
      div(
        style = "width: 100%;",

        navset_tab(
          id = "tabs",
          nav_panel(
            title = "Statewide",
            value = "statewide",
            padding = 5,
            # MOBILE OPTIMIZATION #7: Direct plot output without uiOutput wrappers
            div(class = "girafe-container", girafeOutput("state_plot"))
          ),

          nav_panel(
            title = "CBSA",
            value = "cbsa",
            padding = 5,
            div(class = "girafe-container", girafeOutput("cbsa_plot"))
          ),

          nav_panel(
            title = "Locality",
            value = "locality",
            padding = 5,
            div(class = "girafe-container", girafeOutput("locality_plot"))
          ),

          nav_panel(
            title = "Compare",
            value = "compare",
            padding = 5,
            div(class = "girafe-container", girafeOutput("compare_plot"))
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Create reactive expressions for toggle switches to get selected years
  state_year <- reactive({
    if(input$state_year_toggle) max(year_list) else min(year_list)
  })
  
  cbsa_year <- reactive({
    if(input$cbsa_year_toggle) max(year_list) else min(year_list)
  })
  
  locality_year <- reactive({
    if(input$locality_year_toggle) max(year_list) else min(year_list)
  })
  
  compare_year <- reactive({
    if(input$compare_year_toggle) max(year_list) else min(year_list)
  })
  
  # Pre-process data for better performance
  # Aggregate data to the locality-level
  locality_hh <- reactive({
    selected_year <- locality_year()
    
    hh_type %>% 
      group_by(year, name_long) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      filter(year == selected_year, 
             name_long == input$locality) %>% 
      group_by(type) %>% 
      mutate(rank_within_type = rank(percent, ties.method = "first")) %>% 
      ungroup()
  })
  
  # Aggregate data to the CBSA-level
  cbsa_hh <- reactive({
    selected_year <- cbsa_year()
    
    hh_type %>% 
      group_by(year, cbsa_title, type, subtype) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, cbsa_title) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      filter(year == selected_year, 
             cbsa_title == input$cbsa) %>% 
      group_by(type) %>% 
      mutate(rank_within_type = rank(percent, ties.method = "first")) %>% 
      ungroup()
  })
  
  # Aggregate data to the state-level
  state_hh <- reactive({
    selected_year <- state_year()
    
    hh_type %>% 
      group_by(year, type, subtype) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      filter(year == selected_year) %>% 
      group_by(type) %>% 
      mutate(rank_within_type = rank(percent, ties.method = "first")) %>% 
      ungroup()
  })
  
  # Generate title text
  title_text <- "<b><span style='color:#011E41'>Householder with no partner</span></b> and 
<b><span style='color:#40C0C0'>Married or cohabitating couple</span></b>"
  
  # Create a custom theme function to be applied consistently
  custom_theme <- function() {
    theme_hfv() %+replace%
      theme(
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          vjust = 0.5,
          lineheight = 0.8,
          margin = margin(t = 5)
        ),
        plot.title = element_markdown(),
        plot.subtitle = element_text(size = 12, margin = margin(b = 10))
      )
  }
  
  # Create interactive plot for Statewide tab
  output$state_plot <- renderGirafe({
    state_data <- state_hh()
    selected_year <- state_year()
    
    p <- ggplot(state_data,
                aes(x = reorder(subtype, rank_within_type),
                    y = percent,
                    fill = type)) + 
      # Make sure text colors match fill colors
      scale_color_manual(values = c("#011E41", "#40C0C0")) +
      scale_fill_manual(values = c("#011E41", "#40C0C0")) +
      geom_col_interactive(
        aes(tooltip = paste0(subtype, ": ", scales::percent(percent, accuracy = 0.1))),
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
      labs(title = title_text,
           subtitle = paste("Virginia:", selected_year),
           caption = " ", # Empty caption to leave space for logo
           x = NULL,
           y = "Percent of Households") +
      scale_y_continuous(labels = scales::percent_format()) +
      # Hide the color legend since it's redundant with the fill legend
      guides(color = "none") +
      custom_theme() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      facet_grid(cols = vars(type), scales = "free_x", space = "free") 
    
    girafe(
      ggobj = p,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "fill-opacity: 0.8;"),
        opts_sizing(rescale = TRUE)
      )
    )
  })
  
  # Create interactive plot for CBSA tab
  output$cbsa_plot <- renderGirafe({
    cbsa_data <- cbsa_hh()
    selected_year <- cbsa_year()
    
    p <- ggplot(cbsa_data,
                aes(x = reorder(subtype, rank_within_type),
                    y = percent,
                    fill = type)) + 
      geom_col_interactive(
        aes(tooltip = paste0(subtype, ": ", scales::percent(percent, accuracy = 0.1))),
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
      scale_color_manual(values = c("#011E41", "#40C0C0")) +
      scale_fill_manual(values = c("#011E41", "#40C0C0")) +
      labs(title = title_text,
           subtitle = paste(input$cbsa, ":", selected_year),
           caption = " ", # Empty caption to leave space for logo
           x = NULL,
           y = "Percent of Households") +
      scale_y_continuous(labels = scales::percent_format()) +
      # Hide the color legend since it's redundant with the fill legend
      guides(color = "none") +
      custom_theme() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      facet_grid(cols = vars(type), scales = "free_x", space = "free")
    
    girafe(
      ggobj = p,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "fill-opacity: 0.8;"),
        opts_sizing(rescale = TRUE)
      )
    )
  })
  
  # Create interactive plot for Locality tab
  output$locality_plot <- renderGirafe({
    locality_data <- locality_hh()
    selected_year <- locality_year()
    
    p <- ggplot(locality_data,
                aes(x = reorder(subtype, rank_within_type),
                    y = percent,
                    fill = type)) + 
      geom_col_interactive(
        aes(tooltip = paste0(subtype, ": ", scales::percent(percent, accuracy = 0.1))),
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
      scale_color_manual(values = c("#011E41", "#40C0C0")) +
      labs(title = title_text,
           subtitle = paste(input$locality, ":", selected_year),
           caption = " ", # Empty caption to leave space for logo
           x = NULL,
           y = "Percent of Households") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = c("#011E41", "#40C0C0")) +
      # Hide the color legend since it's redundant with the fill legend
      guides(color = "none") +
      custom_theme() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      facet_grid(cols = vars(type), scales = "free_x", space = "free")
    
    girafe(
      ggobj = p,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "fill-opacity: 0.8;"),
        opts_sizing(rescale = TRUE)
      )
    )
  })
  
  # Helper function to create static plots for downloads
  create_static_plot <- function(data, subtitle) {
    ggplot(data,
           aes(x = reorder(subtype, rank_within_type),
               y = percent,
               fill = type)) + 
      geom_col() +
      # Match text color to bar fill color
      geom_text(aes(label = scales::percent(percent, accuracy = 1),
                    color = type),
                position = position_dodge(width = 0.9),
                vjust = -0.5,
                size = 3.5) +
      # Make sure text colors match fill colors
      scale_color_manual(values = c("#011E41", "#40C0C0")) +
      labs(title = "Household Composition by Type",
           subtitle = subtitle,
           caption = "Source: ACS 5-year estimates",
           x = NULL,
           y = "Percent of Households") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = c("#011E41", "#40C0C0")) +
      theme_minimal() +
      # Hide the color legend since it's redundant with the fill legend
      guides(color = "none") +
      theme(
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          vjust = 0.5,
          lineheight = 0.8,
          margin = margin(t = 5)
        ),
        plot.title = element_markdown(),
        plot.subtitle = element_text(size = 12, margin = margin(b = 10))
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      facet_grid(cols = vars(type), scales = "free_x", space = "free")
  }
  
  # Download handlers for each plot
  output$download_state <- downloadHandler(
    filename = function() {
      selected_year <- state_year()
      paste("virginia-household-composition-", selected_year, ".png", sep = "")
    },
    content = function(file) {
      selected_year <- state_year()
      p <- create_static_plot(state_hh(), paste("Virginia:", selected_year))
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_cbsa <- downloadHandler(
    filename = function() {
      selected_year <- cbsa_year()
      clean_name <- gsub("[^a-zA-Z0-9]", "-", input$cbsa)
      paste(clean_name, "-household-composition-", selected_year, ".png", sep = "")
    },
    content = function(file) {
      selected_year <- cbsa_year()
      p <- create_static_plot(cbsa_hh(), paste(input$cbsa, ":", selected_year))
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_locality <- downloadHandler(
    filename = function() {
      selected_year <- locality_year()
      clean_name <- gsub("[^a-zA-Z0-9]", "-", input$locality)
      paste(clean_name, "-household-composition-", selected_year, ".png", sep = "")
    },
    content = function(file) {
      selected_year <- locality_year()
      p <- create_static_plot(locality_hh(), paste(input$locality, ":", selected_year))
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  # Comparison plot data preparation
  compare_data <- reactive({
    if (input$compare_type == "Years") {
      # Compare the same locality across different years
      data1 <- hh_type %>% 
        filter(name_long == input$compare_locality, year == input$compare_year1) %>%
        group_by(year, name_long) %>% 
        mutate(percent = estimate/sum(estimate)) %>%
        group_by(type) %>% 
        mutate(rank_within_type = rank(percent, ties.method = "first")) %>%
        ungroup() %>%
        mutate(comparison = paste(name_long, ":", input$compare_year1))
      
      data2 <- hh_type %>% 
        filter(name_long == input$compare_locality, year == input$compare_year2) %>%
        group_by(year, name_long) %>% 
        mutate(percent = estimate/sum(estimate)) %>%
        group_by(type) %>% 
        mutate(rank_within_type = rank(percent, ties.method = "first")) %>%
        ungroup() %>%
        mutate(comparison = paste(name_long, ":", input$compare_year2))
      
      bind_rows(data1, data2)
    } else {
      # Compare different localities in the same year
      selected_year <- compare_year()
      
      data1 <- hh_type %>% 
        filter(name_long == input$compare_locality1, year == selected_year) %>%
        group_by(year, name_long) %>% 
        mutate(percent = estimate/sum(estimate)) %>%
        group_by(type) %>% 
        mutate(rank_within_type = rank(percent, ties.method = "first")) %>%
        ungroup() %>%
        mutate(comparison = input$compare_locality1)
      
      data2 <- hh_type %>% 
        filter(name_long == input$compare_locality2, year == selected_year) %>%
        group_by(year, name_long) %>% 
        mutate(percent = estimate/sum(estimate)) %>%
        group_by(type) %>% 
        mutate(rank_within_type = rank(percent, ties.method = "first")) %>%
        ungroup() %>%
        mutate(comparison = input$compare_locality2)
      
      bind_rows(data1, data2)
    }
  })
  
  # Render comparison plot
  output$compare_plot <- renderGirafe({
    comparison_data <- compare_data()
    
    # Generate appropriate title based on comparison type
    if (input$compare_type == "Years") {
      plot_title <- paste("Comparing", input$compare_locality, "between", input$compare_year1, "and", input$compare_year2)
    } else {
      selected_year <- compare_year()
      plot_title <- paste("Comparing", input$compare_locality1, "and", input$compare_locality2, "in", selected_year)
    }
    
    p <- ggplot(comparison_data,
                aes(x = reorder(subtype, rank_within_type),
                    y = percent,
                    fill = comparison)) + 
      geom_col_interactive(
        aes(tooltip = paste0(subtype, ": ", scales::percent(percent, accuracy = 0.1))),
        position = "dodge",
        hover_nearest = TRUE
      ) +
      # Match text color to bar fill color
      geom_text_interactive(
        aes(label = scales::percent(percent, accuracy = 1),
            color = comparison,
            tooltip = paste0("Count: ", format(estimate, big.mark = ","))),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3
      ) +
      # Make sure text colors match fill colors
      scale_color_manual(values = c("#011E41", "#40C0C0")) +
      labs(title = title_text,
           subtitle = plot_title,
           caption = " ", # Empty caption to leave space for logo
           x = NULL,
           y = "Percent of Households",
           fill = "Comparison",
           color = "Comparison") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = c("#011E41", "#40C0C0")) +
      custom_theme() +
      theme(
        legend.position = "top"
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      facet_grid(cols = vars(type), scales = "free_x", space = "free")
    
    girafe(
      ggobj = p,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "fill-opacity: 0.8;"),
        opts_sizing(rescale = TRUE)
      )
    )
  })
  
  # Download handler for comparison plot
  output$download_compare <- downloadHandler(
    filename = function() {
      if (input$compare_type == "Years") {
        clean_name <- gsub("[^a-zA-Z0-9]", "-", input$compare_locality)
        paste(clean_name, "-comparison-", input$compare_year1, "-vs-", input$compare_year2, ".png", sep = "")
      } else {
        selected_year <- compare_year()
        clean_name1 <- gsub("[^a-zA-Z0-9]", "-", input$compare_locality1)
        clean_name2 <- gsub("[^a-zA-Z0-9]", "-", input$compare_locality2)
        paste(clean_name1, "-vs-", clean_name2, "-", selected_year, ".png", sep = "")
      }
    },
    content = function(file) {
      comparison_data <- compare_data()
      
      # Generate appropriate title based on comparison type
      if (input$compare_type == "Years") {
        plot_title <- paste("Comparing", input$compare_locality, "between", input$compare_year1, "and", input$compare_year2)
      } else {
        selected_year <- compare_year()
        plot_title <- paste("Comparing", input$compare_locality1, "and", input$compare_locality2, "in", selected_year)
      }
      
      p <- ggplot(comparison_data,
                  aes(x = reorder(subtype, rank_within_type),
                      y = percent,
                      fill = comparison)) + 
        geom_col(position = "dodge") +
        # Match text color to bar fill color
        geom_text(aes(label = scales::percent(percent, accuracy = 1),
                      color = comparison),
                  position = position_dodge(width = 0.9),
                  vjust = -0.5,
                  size = 3) +
        # Make sure text colors match fill colors
        scale_color_manual(values = c("#011E41", "#40C0C0")) +
        labs(title = "Household Composition by Type",
             subtitle = plot_title,
             caption = "Source: ACS 5-year estimates",
             x = NULL,
             y = "Percent of Households",
             fill = "Comparison",
             color = "Comparison") +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_minimal() +
        scale_fill_manual(values = c("#011E41", "#40C0C0")) +
        theme(
          axis.text.x = element_text(
            angle = 0,
            hjust = 0.5,
            vjust = 0.5,
            lineheight = 0.8,
            margin = margin(t = 5)
          ),
          legend.position = "top",
          plot.title = element_markdown(),
          plot.subtitle = element_text(size = 12, margin = margin(b = 10))
        ) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
        facet_grid(cols = vars(type), scales = "free_x", space = "free")
      
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  # MOBILE OPTIMIZATION #7: Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)