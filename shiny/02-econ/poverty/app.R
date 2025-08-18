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

# Load data outside of server

poverty_age <- read_rds("age_data.rds")
poverty_race <- read_rds("race_data.rds")

# Create lists for filters
state_list <- poverty_age |> 
  filter(geography == "state") |> 
  distinct(NAME) |>  # gets unique values
  arrange(NAME) |>   # sorts them
  pull(NAME)         # extracts the column as a vector
  
cbsa_list <- poverty_age |> 
  filter(geography == "cbsa") |> 
  distinct(NAME) |>  # gets unique values
  arrange(NAME) |>   # sorts them
  pull(NAME)         # extracts the column as a vector

locality_list <- poverty_age |> 
  filter(geography == "locality") |> 
  distinct(NAME) |>  # gets unique values
  arrange(NAME) |>   # sorts them
  pull(NAME)         # extracts the column as a vector

age_list <- poverty_age |> 
  filter(geography == "locality") |> 
  distinct(age) |>  # gets unique values
  arrange(age) |>   # sorts them
  pull(age)         # extracts the column as a vector

race_list <- poverty_race |> 
  filter(geography == "locality") |> 
  distinct(race) |>  # gets unique values
  arrange(race) |>   # sorts them
  pull(race)         # extracts the column as a vector


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
      h4("Poverty Rate Analysis", class = "hfv-title")
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
        
        # Analysis type selector
        div(
          style = "margin-bottom: 16px;",
          radioButtons("analysis_type", "Analysis Type:",
                       choices = list("By Race/Ethnicity" = "race", 
                                      "By Age Group" = "age"),
                       selected = "race",
                       inline = FALSE)
        ),
        
        # Geography selectors
        div(
          style = "margin-bottom: 16px;",
          conditionalPanel(
            condition = "input.tabs == 'cbsa' && input.analysis_type == 'race'",
            selectInput("race_cbsa_select", "Metro Area:", 
                        choices = cbsa_list,
                        selected = if("Richmond, VA" %in% cbsa_list) "Richmond, VA" else cbsa_list[1],
                        width = "100%", 
                        selectize = FALSE)
          ),
          conditionalPanel(
            condition = "input.tabs == 'local' && input.analysis_type == 'race'",
            selectInput("race_locality_select", "Locality:", 
                        choices = locality_list,
                        selected = if("Richmond city" %in% locality_list) "Richmond city" else locality_list[1],
                        width = "100%", 
                        selectize = FALSE)
          ),
          conditionalPanel(
            condition = "input.tabs == 'cbsa' && input.analysis_type == 'age'",
            selectInput("age_cbsa_select", "Metro Area:", 
                        choices = cbsa_list,
                        selected = if("Richmond, VA" %in% cbsa_list) "Richmond, VA" else cbsa_list[1],
                        width = "100%", 
                        selectize = FALSE)
          ),
          conditionalPanel(
            condition = "input.tabs == 'local' && input.analysis_type == 'age'",
            selectInput("age_locality_select", "Locality:", 
                        choices = locality_list,
                        selected = if("Richmond city" %in% locality_list) "Richmond city" else locality_list[1],
                        width = "100%", 
                        selectize = FALSE)
          )
        ),
        
        # Divider
        hr(style = "margin: 24px 0; border-color: #ced4da;"),
        
        # Tooltip info
        div(
          style = "margin-bottom: 16px; font-size: 0.75rem;",
          p("Hover over points to see details", style = "margin-bottom: 8px;"),
          verbatimTextOutput("hover_info", placeholder = TRUE)
        ),
        
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
              conditionalPanel(
                condition = "input.analysis_type == 'race'",
                girafeOutput("race_state_plot", height = "100%")
              ),
              conditionalPanel(
                condition = "input.analysis_type == 'age'",
                girafeOutput("age_state_plot", height = "100%")
              )
            )
          ),
          
          nav_panel(
            title = "Metro Area",
            value = "cbsa", 
            div(
              class = "hfv-chart-container",
              style = "height: 450px; margin-top: 16px;",
              conditionalPanel(
                condition = "input.analysis_type == 'race'",
                girafeOutput("race_cbsa_plot", height = "100%")
              ),
              conditionalPanel(
                condition = "input.analysis_type == 'age'",
                girafeOutput("age_cbsa_plot", height = "100%")
              )
            )
          ),
          
          nav_panel(
            title = "Locality",
            value = "local",
            div(
              class = "hfv-chart-container",
              style = "height: 450px; margin-top: 16px;",
              conditionalPanel(
                condition = "input.analysis_type == 'race'",
                girafeOutput("race_locality_plot", height = "100%")
              ),
              conditionalPanel(
                condition = "input.analysis_type == 'age'",
                girafeOutput("age_locality_plot", height = "100%")
              )
            )
          )
        )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Define race colors
  race_colors <- c(
    "White alone, not Hispanic" = "#40C0C0",
    "Black alone" = "#011E41",
    "Asian alone" = "#259591",
    "Hispanic (any race)" = "#E0592A",
    "Two or more races" = "#B1005F",
    "American Indian alone" = "#8B85CA",
    "Pacific Islander alone" = "#FFC658",  # Additional color
    "Some other race alone" = "#FF7276"    # Additional color
  )
  
  # Define age colors
  age_colors <- c(
    "17 years and under" = "#FFC658",    # Desert variant
    "18 to 24 years" = "#E0592A", # Desert
    "25 to 34 years" = "#259591",  # Grass
    "35 to 44 years" = "#40C0C0",    # Sky
    "45 to 54 years" = "#8B85CA",  # Lilac
    "55 to 64 years" = "#B1005F",  # Berry
    "65 years and over" = "#011E41" # Shadow
  )
  
  #----- RACE DATA PROCESSING -----#
  
  # Process race state data
  race_state_data <- reactive({
    # Process race state data
    pov_race_state <- poverty_race %>% 
      filter(geography == "state") |> 
      filter(name ==)
      group_by(year, race) %>% 
      summarise(estimate = sum(estimate),
                totalrace = sum(totalrace),
                .groups = "drop") %>% 
      mutate(rate = estimate/totalrace)
    
    # Calculate the mean rate for each race to help determine order of facets
    state_summary <- pov_race_state %>%
      group_by(race) %>%
      summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
      arrange(desc(mean_rate))
    
    # Add ordered factor
    pov_race_state %>%
      mutate(race_ordered = factor(race, levels = state_summary$race))
  })
  
  # Process race CBSA data
  filtered_race_cbsa_data <- reactive({
    req(input$race_cbsa_select)
    
    cbsa <- poverty_race %>% 
      group_by(year, race, cbsa_title) %>% 
      summarise(estimate = sum(estimate),
                totalrace = sum(totalrace),
                .groups = "drop") %>% 
      mutate(rate = estimate/totalrace) %>% 
      filter(cbsa_title == input$race_cbsa_select)
    
    # Calculate the mean rate for each race to help determine order of facets
    cbsa_summary <- cbsa %>%
      group_by(race) %>%
      summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
      arrange(desc(mean_rate))
    
    # Add ordered factor
    cbsa %>%
      mutate(race_ordered = factor(race, levels = cbsa_summary$race))
  })
  
  # Process race locality data
  filtered_race_locality_data <- reactive({
    req(input$race_locality_select)
    
    local <- poverty_race %>% 
      filter(locality == input$race_locality_select)
    
    # Calculate the mean rate for each race to help determine order of facets
    local_summary <- local %>%
      group_by(race) %>%
      summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
      arrange(desc(mean_rate))
    
    # Add ordered factor
    local %>%
      mutate(race_ordered = factor(race, levels = local_summary$race))
  })
  
  #----- AGE DATA PROCESSING -----#
  
  # Process age state data
  age_state_data <- reactive({
    poverty_age %>% 
      group_by(year, age, age_group) %>% 
      summarise(estimate = sum(estimate),
                totalage = sum(totalage),
                .groups = "drop") %>% 
      mutate(rate = estimate/totalage)
  })
  
  # Process age CBSA data
  filtered_age_cbsa_data <- reactive({
    req(input$age_cbsa_select)
    
    poverty_age %>% 
      group_by(year, age, age_group, cbsa_title) %>% 
      summarise(estimate = sum(estimate),
                totalage = sum(totalage),
                .groups = "drop") %>% 
      mutate(rate = estimate/totalage) %>% 
      filter(cbsa_title == input$age_cbsa_select)
  })
  
  # Process age locality data
  filtered_age_locality_data <- reactive({
    req(input$age_locality_select)
    
    poverty_age %>% 
      filter(locality == input$age_locality_select) %>%
      group_by(year, age, age_group) %>%
      summarise(estimate = sum(estimate),
                totalage = sum(totalage),
                .groups = "drop") %>%
      mutate(rate = estimate/totalage)
  })
  
  # Function to create race poverty rate plots
  create_race_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Get latest year data for labels
    latest_year <- max(data$year)
    latest_data <- data %>% filter(year == latest_year)
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", race, "\n",
        "Year: ", year, "\n",
        "Poverty Rate: ", scales::percent(rate, accuracy = 0.1), "\n",
        "Number in Poverty: ", format(estimate, big.mark = ",")
      ))
    
    latest_data <- latest_data %>%
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", race, "\n",
        "Year: ", year, "\n",
        "Poverty Rate: ", scales::percent(rate, accuracy = 0.1), "\n",
        "Number in Poverty: ", format(estimate, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = year,
                    y = rate,
                    color = race_ordered,
                    group = race_ordered)) +
      geom_line_interactive(
        aes(tooltip = tooltip, data_id = paste(year, race)),
        linewidth = 1
      ) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = paste(year, race)),
        size = 2
      ) +
      # Add labels for latest values
      geom_text(data = latest_data, 
                aes(label = scales::percent(rate, accuracy = 0.1)),
                hjust = -0.3, vjust = 0.5) +
      facet_wrap(~race_ordered, nrow = 1) +
      scale_color_manual(values = race_colors) +
      # Format y-axis as percentage
      scale_y_continuous(labels = scales::percent_format(), 
                         limits = c(0, NA)) +
      labs(
        title = title_text,
        caption = " ", # Add empty caption to leave space for logo
        y = "Poverty Rate",
        x = "Year"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10, face = "bold"),
        panel.spacing = unit(1, "lines"),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 15, 30, 5) # Extra right margin for labels and bottom margin for logo
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
  
  # Function to create age poverty rate plots
  create_age_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Get latest year data for labels
    latest_year <- max(data$year)
    latest_data <- data %>% filter(year == latest_year)
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Age Group: ", age, "\n",
        "Year: ", year, "\n",
        "Poverty Rate: ", scales::percent(rate, accuracy = 0.1), "\n",
        "Number in Poverty: ", format(estimate, big.mark = ",")
      ))
    
    latest_data <- latest_data %>%
      mutate(tooltip = paste0(
        "Age Group: ", age, "\n",
        "Year: ", year, "\n",
        "Poverty Rate: ", scales::percent(rate, accuracy = 0.1), "\n",
        "Number in Poverty: ", format(estimate, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(
                  x = year,
                  y = rate,
                  color = age,
                  group = age
                )) +
      geom_line_interactive(
        aes(tooltip = tooltip, data_id = paste(year, age)),
        linewidth = 1
      ) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = paste(year, age)),
        size = 2
      ) +
      # Add labels for latest values
      geom_text(data = latest_data, 
                aes(label = scales::percent(rate, accuracy = 0.1)),
                hjust = -0.3, vjust = 0.5) +
      facet_wrap(~age_group, nrow = 1) +
      scale_color_manual(values = age_colors) +
      # Format y-axis as percentage
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = title_text,
        caption = " ", # Add empty caption to leave space for logo
        y = "Poverty Rate",
        x = "Year"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10, face = "bold"),
        panel.spacing = unit(1, "lines"),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 15, 30, 5) # Extra right margin for labels and bottom margin for logo
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
  
  # Plot titles
  race_state_title <- reactive({
    "Poverty Rate by Race/Ethnicity in Virginia"
  })
  
  race_cbsa_title <- reactive({
    paste("Poverty Rate by Race/Ethnicity in", input$race_cbsa_select)
  })
  
  race_locality_title <- reactive({
    paste("Poverty Rate by Race/Ethnicity in", input$race_locality_select)
  })
  
  age_state_title <- reactive({
    "Poverty Rate by Age Group in Virginia"
  })
  
  age_cbsa_title <- reactive({
    paste("Poverty Rate by Age Group in", input$age_cbsa_select)
  })
  
  age_locality_title <- reactive({
    paste("Poverty Rate by Age Group in", input$age_locality_select)
  })
  
  # Render race plots
  output$race_state_plot <- renderGirafe({
    create_interactive_plot(create_race_plot(race_state_data(), race_state_title()))
  })
  
  output$race_cbsa_plot <- renderGirafe({
    create_interactive_plot(create_race_plot(filtered_race_cbsa_data(), race_cbsa_title()))
  })
  
  output$race_locality_plot <- renderGirafe({
    create_interactive_plot(create_race_plot(filtered_race_locality_data(), race_locality_title()))
  })
  
  # Render age plots
  output$age_state_plot <- renderGirafe({
    create_interactive_plot(create_age_plot(age_state_data(), age_state_title()))
  })
  
  output$age_cbsa_plot <- renderGirafe({
    create_interactive_plot(create_age_plot(filtered_age_cbsa_data(), age_cbsa_title()))
  })
  
  output$age_locality_plot <- renderGirafe({
    create_interactive_plot(create_age_plot(filtered_age_locality_data(), age_locality_title()))
  })
  
  # Handle hover info for all plots
  get_hover_data <- reactive({
    if (input$analysis_type == "race") {
      if (input$tabs == "state") {
        data <- race_state_data()
        geo_name <- "Virginia"
        demographic_type <- "Race/Ethnicity"
        demographic_field <- "race"
      } else if (input$tabs == "cbsa") {
        data <- filtered_race_cbsa_data()
        geo_name <- input$race_cbsa_select
        demographic_type <- "Race/Ethnicity"
        demographic_field <- "race"
      } else { # locality
        data <- filtered_race_locality_data()
        geo_name <- input$race_locality_select
        demographic_type <- "Race/Ethnicity"
        demographic_field <- "race"
      }
    } else { # age
      if (input$tabs == "state") {
        data <- age_state_data()
        geo_name <- "Virginia"
        demographic_type <- "Age Group"
        demographic_field <- "age"
      } else if (input$tabs == "cbsa") {
        data <- filtered_age_cbsa_data()
        geo_name <- input$age_cbsa_select
        demographic_type <- "Age Group"
        demographic_field <- "age"
      } else { # locality
        data <- filtered_age_locality_data()
        geo_name <- input$age_locality_select
        demographic_type <- "Age Group"
        demographic_field <- "age"
      }
    }
    
    list(
      data = data,
      geo_name = geo_name,
      demographic_type = demographic_type,
      demographic_field = demographic_field
    )
  })
  
  # Display hover information
  output$hover_info <- renderText({
    hover_data <- get_hover_data()
    data <- hover_data$data
    
    # If there's no hover data, show a placeholder message
    if (is.null(data) || nrow(data) == 0) {
      return("Hover over a point for details")
    }
    
    geo_name <- hover_data$geo_name
    demo_type <- hover_data$demographic_type
    demo_field <- hover_data$demographic_field
    
    # Format some example hover data for display
    if (nrow(data) > 0) {
      # Take one row as an example
      example <- data[1,]
      demo_value <- example[[demo_field]]
      
      paste0(
        geo_name, "\n",
        demo_type, ": ", demo_value, "\n",
        "Hover for more details"
      )
    } else {
      "Hover over a point for details"
    }
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)