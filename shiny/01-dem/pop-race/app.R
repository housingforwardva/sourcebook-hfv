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

# Load data outside of server
race_data <- read_rds("race_ethnicity.rds")

# Create lists for filters
cbsa_list <- sort(unique(race_data$cbsa_title))
locality_list <- sort(unique(race_data$name_long))
year_list <- sort(as.character(unique(race_data$year)))

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
      h4("Population by Race and Ethnicity", class = "hfv-title")
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
                      choices = year_list, 
                      selected = max(year_list), 
                      width = "100%",
                      selectize = FALSE)
        ),
        
        # Geography selectors
        div(
          style = "margin-bottom: 16px;",
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            selectInput("cbsa", "Metro Area:", 
                        choices = cbsa_list,
                        selected = if("Richmond, VA" %in% cbsa_list) "Richmond, VA" else cbsa_list[1],
                        width = "100%", 
                        selectize = FALSE)
          ),
          conditionalPanel(
            condition = "input.tabs == 'local'",
            selectInput("locality", "Locality:", 
                        choices = locality_list,
                        selected = if("Richmond City" %in% locality_list) "Richmond City" else locality_list[1],
                        width = "100%", 
                        selectize = FALSE)
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
  
  # Create filtered datasets
  filtered_state <- reactive({
    req(input$year)
    
    race_data %>% 
      group_by(year, label) %>% 
      summarise(value = sum(value), .groups = "drop") %>% 
      filter(year == input$year) %>% 
      mutate(percent = value/sum(value),
             value_label = number_format(big.mark = ",")(value),
             percent_label = percent_format(accuracy = 0.1)(percent)) %>% 
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", label, "\n",
        "Count: ", value_label, "\n",
        "Percentage: ", percent_label
      ))
  })
  
  filtered_cbsa <- reactive({
    req(input$year, input$cbsa)
    
    race_data %>% 
      group_by(year, cbsa_title, label) %>% 
      summarise(value = sum(value), .groups = "drop") %>% 
      filter(year == input$year,
             cbsa_title == input$cbsa) %>% 
      mutate(percent = value/sum(value),
             value_label = number_format(big.mark = ",")(value),
             percent_label = percent_format(accuracy = 0.1)(percent)) %>% 
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", label, "\n",
        "Count: ", value_label, "\n",
        "Percentage: ", percent_label
      ))
  })
  
  filtered_locality <- reactive({
    req(input$year, input$locality)
    
    race_data %>% 
      filter(year == input$year,
             name_long == input$locality) %>% 
      group_by(year) %>% 
      mutate(percent = value/sum(value),
             value_label = number_format(big.mark = ",")(value),
             percent_label = percent_format(accuracy = 0.1)(percent)) %>%
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", label, "\n",
        "Count: ", value_label, "\n",
        "Percentage: ", percent_label
      ))
  })
  
  # Plot titles
  state_title <- reactive({
    paste("Virginia Population by Race and Ethnicity in", input$year)
  })
  
  cbsa_title <- reactive({
    paste("Population by Race and Ethnicity in", input$cbsa, "(", input$year, ")")
  })
  
  locality_title <- reactive({
    paste("Population by Race and Ethnicity in", input$locality, "(", input$year, ")")
  })
  
  # Function to create bar charts for race/ethnicity distribution
  create_race_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Create base plot
    p <- ggplot(data,
                aes(x = reorder(label, -percent),
                    y = percent,
                    fill = label)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = label),
        position = "dodge"
      ) +
      scale_fill_manual(values = c(
        "White, non-Hispanic" = hfv_colors$sky,
        "Black" = hfv_colors$shadow,
        "Asian" = hfv_colors$grass,
        "Hispanic or Latino" = hfv_colors$desert,
        "Multiracial" = hfv_colors$berry,
        "Another race" = hfv_colors$lilac
      )) +
      scale_y_continuous(
        labels = percent_format(), 
        limits = c(0, 1)
      ) +
      labs(
        title = title_text,
        caption = " ", # Add empty caption to leave space for logo
        y = "Percentage of Population",
        x = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
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
    suppressWarnings(create_interactive_plot(create_race_plot(filtered_state(), state_title())))
  })
  
  output$cbsa_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_race_plot(filtered_cbsa(), cbsa_title())))
  })
  
  output$local_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_race_plot(filtered_locality(), locality_title())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)