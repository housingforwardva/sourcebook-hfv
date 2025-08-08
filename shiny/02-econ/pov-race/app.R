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
      h4("Poverty Rate by Race and Ethnicity", class = "title-text")
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
              choices = NULL,
              width = "100%",
              selectize = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.tabs == 'locality'",
            selectInput(
              "locality_select",
              "Locality:",
              choices = NULL,
              width = "100%",
              selectize = TRUE
            )
          )
        ),

        # Horizontal line
        hr(style = "margin: 15px 0;"),

        # Source information
        div(
          style = "font-size: 10px; color: #666; margin-top: 8px;",
          p("Source: U.S. Census Bureau, American Community Survey 5-year estimates.")
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
            value = "locality",
            padding = 5,
            div(class = "girafe-container", girafeOutput("locality_plot"))
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load data
  poverty_race <- reactive({
    read_rds(here("data", "rds", "poverty_race.rds"))
  })
  
  # Process state data
  state_data <- reactive({
    req(poverty_race())
    
    pov_race_state <- poverty_race() %>% 
      group_by(year, race) %>% 
      summarise(estimate = sum(estimate),
                totalrace = sum(totalrace)) %>% 
      mutate(rate = estimate/totalrace) %>% 
      ungroup()
    
    # Calculate the mean rate for each race to help determine order of facets
    state_summary <- pov_race_state %>%
      group_by(race) %>%
      summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
      arrange(desc(mean_rate))
    
    # Add ordered factor
    pov_race_state %>%
      mutate(race_ordered = factor(race, levels = state_summary$race))
  })
  
  # Define color palette based on unique race values
  race_colors <- reactive({
    req(state_data())
    
    # First, get the actual unique race values from your data
    race_levels <- unique(state_data()$race)
    
    # Create color vector without names first
    color_values <- c("#E0592A", "#259591", "#011E41", 
                      "#40C0C0", "#B1005F", "#8B85CA", 
                      "#102C54", "#FFC658")
    
    # Then create a named vector matching your actual data values
    setNames(color_values[1:length(race_levels)], race_levels)
  })
  
  # Process CBSA data
  cbsa_data <- reactive({
    req(poverty_race())
    
    pov_race_cbsa <- poverty_race() %>% 
      group_by(year, race, cbsa_title) %>% 
      summarise(estimate = sum(estimate),
                totalrace = sum(totalrace)) %>% 
      mutate(rate = estimate/totalrace) %>% 
      ungroup()
    
    # Update CBSA choices in the UI
    cbsa_choices <- sort(unique(pov_race_cbsa$cbsa_title))
    updateSelectInput(session, "cbsa_select", choices = cbsa_choices, 
                      selected = ifelse("Richmond, VA" %in% cbsa_choices, "Richmond, VA", cbsa_choices[1]))
    
    pov_race_cbsa
  })
  
  # Filter CBSA data based on selection
  filtered_cbsa_data <- reactive({
    req(cbsa_data(), input$cbsa_select)
    
    cbsa <- cbsa_data() %>% 
      filter(cbsa_title == input$cbsa_select)
    
    # Calculate the mean rate for each race to help determine order of facets
    cbsa_summary <- cbsa %>%
      group_by(race) %>%
      summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
      arrange(desc(mean_rate))
    
    # Add ordered factor
    cbsa %>%
      mutate(race_ordered = factor(race, levels = cbsa_summary$race))
  })
  
  # Process locality data
  locality_data <- reactive({
    req(poverty_race())
    
    pov_race_local <- poverty_race()
    
    # Update locality choices in the UI
    locality_choices <- sort(unique(pov_race_local$locality))
    updateSelectInput(session, "locality_select", choices = locality_choices, 
                      selected = ifelse("Richmond city" %in% locality_choices, "Richmond city", locality_choices[1]))
    
    pov_race_local
  })
  
  # Filter locality data based on selection
  filtered_locality_data <- reactive({
    req(locality_data(), input$locality_select)
    
    local <- locality_data() %>% 
      filter(locality == input$locality_select)
    
    # Calculate the mean rate for each race to help determine order of facets
    local_summary <- local %>%
      group_by(race) %>%
      summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
      arrange(desc(mean_rate))
    
    # Add ordered factor
    local %>%
      mutate(race_ordered = factor(race, levels = local_summary$race))
  })
  
  # Function to create interactive plots
  create_poverty_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Get latest year data for labels
    latest_year <- max(data$year)
    latest_data <- data %>% filter(year == latest_year)
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Race: ", race_ordered, "\n",
        "Year: ", year, "\n",
        "Poverty Rate: ", scales::percent(rate, accuracy = 0.1)
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
           aes(
             x = year,
             y = rate,
             color = race_ordered,
             group = race_ordered)) +
      geom_line_interactive(
        aes(tooltip = tooltip, data_id = paste(race_ordered, year)),
        linewidth = 1.5
      ) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = paste(race_ordered, year)),
        size = 3
      ) +
      # Add labels for latest values
      geom_text(data = latest_data, 
                aes(label = scales::percent(rate, accuracy = 0.1)),
                hjust = -0.3, vjust = 0.5, size = 3) +
      facet_wrap(~race_ordered, ncol = 3) +
      scale_color_manual(values = race_colors()) +
      # Better x-axis formatting - show fewer years
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = max(1, length(x) %/% 4))]) +  
      # Format y-axis as percentage
      scale_y_continuous(labels = scales::percent_format(), 
                         limits = c(0, NA)) +
      labs(
        title = title_text,
        caption = " ", # Empty caption to leave space for logo
        x = NULL,
        y = "Poverty Rate"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "none",
        panel.spacing = unit(1.5, "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.title.position = "plot",
        plot.margin = margin(5, 5, 30, 5), # Extra bottom margin for logo
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
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

    # Create girafe object with the logo plot
    girafe(
      ggobj = p_with_logo,
      width_svg = 8,
      height_svg = 5,
      options = list(
        opts_hover(css = "stroke-width:3;"),
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
  
  # State Plot
  output$state_plot <- renderGirafe({
    req(state_data(), race_colors())
    suppressWarnings(create_poverty_plot(state_data(), "Virginia Poverty Rate by Race and Ethnicity"))
  })
  
  # CBSA Plot
  output$cbsa_plot <- renderGirafe({
    req(filtered_cbsa_data(), race_colors())
    title <- paste0("Poverty Rate by Race and Ethnicity - ", input$cbsa_select)
    suppressWarnings(create_poverty_plot(filtered_cbsa_data(), title))
  })
  
  # Locality Plot
  output$locality_plot <- renderGirafe({
    req(filtered_locality_data(), race_colors())
    title <- paste0("Poverty Rate by Race and Ethnicity - ", input$locality_select)
    suppressWarnings(create_poverty_plot(filtered_locality_data(), title))
  })
  
  # MOBILE OPTIMIZATION #8: Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the app
shinyApp(ui = ui, server = server)