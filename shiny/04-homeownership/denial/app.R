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
library(arrow)

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

# Define HFV color palette
hfv_colors <- list(
  sky = "#40C0C0",
  grass = "#259591",
  lilac = "#8B85CA", 
  shadow = "#011E41",
  shadow_light = "#102C54",
  berry = "#B1005F",
  desert = "#E0592A"
)

# Define consistent colors for race_ethnicity categories
race_ethnicity_colors <- c(
  "White, non-Hispanic" = hfv_colors$shadow,
  "Black" = hfv_colors$berry,
  "Hispanic or Latino" = hfv_colors$desert,
  "Asian" = hfv_colors$grass,
  "Other Minority" = hfv_colors$lilac,
  "White Co-Applicant" = hfv_colors$shadow_light,
  "Incomplete/No Data" = "#CCCCCC"
)

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
      h4("Mortgage Denial Rates by Race and Ethnicity", class = "hfv-title")
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
          selectInput("year", "Year:", 
                     choices = c(2018:2024), 
                     selected = 2024, 
                     width = "100%", 
                     selectize = FALSE)
        ),
        
        # Loan purpose selector
        div(
          style = "margin-bottom: 16px;",
          selectInput("loan_purpose", "Loan Purpose:", 
                     choices = c("Home purchase", "Home improvement", "Refinancing", "Cash-out refinancing", "Other purpose"),
                     selected = "Home purchase", 
                     width = "100%", 
                     selectize = FALSE)
        ),
        
        # Occupancy type selector
        div(
          style = "margin-bottom: 16px;",
          selectInput("occupancy_type", "Occupancy Type:", 
                     choices = c("Principal residence", "Second residence", "Investment property"),
                     selected = "Principal residence", 
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
            "Consumer Financial Protection Bureau, Home Mortgage Disclosure Act (HMDA) data",
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
  
  # Load the data (using path relative to project root)
  local_lookup <- reactive({
    read_csv("../../data/local_lookup.csv") |> 
      mutate(fips_full = as.character(fips_full))
  })
  
  loans_race <- reactive({
    read_parquet("../../data/parquet/hmda_va_clean.parquet") |> 
      select(activity_year, lei, fips_full = county_code, race_ethnicity, action_taken, purchaser_type, loan_purpose,
             occupancy_type) |> 
      mutate(count = 1) |> 
      group_by(activity_year, fips_full, race_ethnicity, action_taken, loan_purpose, occupancy_type) |> 
      summarise(count = sum(count), .groups = "drop") %>% 
      left_join(local_lookup(), by = "fips_full") |> 
      filter(state == "Virginia")
  })
  
  # Get available CBSAs and localities
  cbsa_list <- reactive({
    loans_race() %>%
      filter(!is.na(cbsa_title)) %>%
      pull(cbsa_title) %>%
      unique() %>%
      sort()
  })
  
  locality_list <- reactive({
    loans_race() %>%
      filter(!is.na(name_long)) %>%
      pull(name_long) %>%
      unique() %>%
      sort()
  })
  
  # Initialize dropdowns
  observe({
    # CBSAs
    updateSelectInput(session, "cbsa", 
                      choices = cbsa_list(),
                      selected = if("Richmond, VA" %in% cbsa_list()) "Richmond, VA" else cbsa_list()[1])
    
    # Localities
    updateSelectInput(session, "locality", 
                      choices = locality_list(),
                      selected = if("Richmond City" %in% locality_list()) "Richmond City" else locality_list()[1])
  })
  
  # Create state-level denial data
  state_data <- reactive({
    loans_race() |> 
      filter(activity_year == input$year) |> 
      group_by(state, race_ethnicity, loan_purpose, occupancy_type) |> 
      mutate(total = sum(count)) |> 
      filter(loan_purpose == input$loan_purpose) |> 
      filter(occupancy_type == input$occupancy_type) |> 
      group_by(race_ethnicity, action_taken, total) |> 
      summarise(count = sum(count), .groups = "drop") |> 
      mutate(rate = count/total) |> 
      filter(action_taken == "Application denied") |> 
      arrange(desc(rate))
  })
  
  # Create CBSA-level denial data
  cbsa_data <- reactive({
    req(input$cbsa)
    
    loans_race() |> 
      filter(activity_year == input$year) |> 
      group_by(cbsa_title, race_ethnicity, loan_purpose, occupancy_type) |> 
      mutate(total = sum(count)) |> 
      filter(loan_purpose == input$loan_purpose) |> 
      filter(occupancy_type == input$occupancy_type) |> 
      filter(cbsa_title == input$cbsa) |> 
      group_by(cbsa_title, race_ethnicity, action_taken, total) |> 
      summarise(count = sum(count), .groups = "drop") |> 
      mutate(rate = count/total) |> 
      filter(action_taken == "Application denied") |> 
      arrange(desc(rate))
  })
  
  # Create locality-level denial data
  locality_data <- reactive({
    req(input$locality)
    
    loans_race() |> 
      filter(activity_year == input$year) |> 
      group_by(name_long, race_ethnicity, loan_purpose, occupancy_type) |> 
      mutate(total = sum(count)) |> 
      filter(loan_purpose == input$loan_purpose) |> 
      filter(occupancy_type == input$occupancy_type) |> 
      filter(name_long == input$locality) |> 
      group_by(name_long, race_ethnicity, action_taken, total) |> 
      summarise(count = sum(count), .groups = "drop") |> 
      mutate(rate = count/total) |> 
      filter(action_taken == "Application denied") |> 
      arrange(desc(rate))
  })
  
  # Plot titles
  state_title <- reactive({
    paste("Virginia Loan Denial Rates by Race/Ethnicity -", input$year)
  })
  
  cbsa_title <- reactive({
    paste("Loan Denial Rates by Race/Ethnicity -", input$cbsa, "-", input$year)
  })
  
  locality_title <- reactive({
    paste("Loan Denial Rates by Race/Ethnicity -", input$locality, "-", input$year)
  })
  
  # Function to create denial rate plots
  create_denial_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", race_ethnicity, "\n",
        "Denial Rate: ", percent(rate, accuracy = 0.1), "\n",
        "Applications Denied: ", format(count, big.mark = ","), "\n",
        "Total Applications: ", format(total, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = reorder(race_ethnicity, rate),
                    y = rate,
                    fill = race_ethnicity)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = race_ethnicity),
        width = 0.7
      ) +
      geom_text(
        aes(label = percent(rate, accuracy = 0.1)),
        hjust = -0.1,
        size = 3,
        color = "black"
      ) +
      scale_fill_manual(values = race_ethnicity_colors, na.value = "#CCCCCC") +
      coord_flip() +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        expand = expansion(mult = c(0, 0.15))
      ) +
      labs(
        title = title_text,
        caption = " ", # Add empty caption to leave space for logo
        y = "Denial Rate",
        x = "Race/Ethnicity"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 20, 30, 5) # Extra bottom margin for logo
      )
    
    # Add logo
    logo_url <- "https://housingforwardva.org/wp-content/uploads/2024/08/HousingForward-VA-Logo-Files-Horizontal-Gradient-RGB.png"
    
    p_with_logo <- ggdraw(p) +
      draw_image(
        logo_url,
        x = 0.85,
        y = 0.05,
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
    suppressWarnings(create_interactive_plot(create_denial_plot(state_data(), state_title())))
  })
  
  output$cbsa_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_denial_plot(cbsa_data(), cbsa_title())))
  })
  
  output$local_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_denial_plot(locality_data(), locality_title())))
  })
  
  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)