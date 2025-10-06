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
library(forcats)
library(gfonts)

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
  shadow_light = "#102C54",  # Lighter shade of shadow color
  berry = "#B1005F",
  desert = "#E0592A"
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
ui <- function(request) {
  page_fillable(
    theme = hfv_theme,
    useShinyjs(),

    # Main container using HFV classes
    div(
      class = "hfv-container",

      # Header using HFV styling
      div(
        class = "hfv-header",
        h4("Housing Supply and Demand Gap", class = "hfv-title")
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
            selectInput(
              "year",
              "Select Year:",
              choices = NULL,
              selected = NULL,
              width = "100%",
              selectize = TRUE
            )
          ),

          # Divider
          hr(style = "margin: 24px 0; border-color: #ced4da;"),

          # Data source
          div(
            style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
            p(
              strong("Data Source:"), br(),
              "U.S. Department of Housing and Urban Development (HUD), Comprehensive Housing Affordability Strategy (CHAS) data",
              style = "margin-bottom: 0;"
            )
          )
        ),

        # Main Panel with single plot
        div(
          class = "hfv-chart-container",
          style = "height: 450px; margin-top: 16px;",
          girafeOutput("plot", height = "100%")
        )
      )
    )
  )
}

# Server function
server <- function(input, output, session) {
  
  # Define the desired factor order
  match_order <- c("Very affordable", "Affordable", "Unaffordable")
  
  # Load the data
  gap <- reactive({
    read_rds(here("data", "rds", "table18c_chas.rds")) %>% 
      mutate(
        household_income = factor(household_income, 
                                  levels = c("30% AMI or less", 
                                             "31 to 50% AMI", 
                                             "51 to 80% AMI", 
                                             "81% AMI or greater")),
        # Apply factor ordering immediately when loading data
        match = factor(match, levels = match_order)
      )
  })
  
  # Load lookup table
  lookup <- reactive({
    read_csv(here("data", "local_lookup.csv")) %>% 
      mutate(fips = fips_full)
  })
  
  # Join data with lookup
  gap_join <- reactive({
    gap() %>% 
      left_join(lookup(), by = "fips")
  })
  
  # Pre-compute state, CBSA, and local data 
  state_data <- reactive({
    gap_join() %>% 
      group_by(year, household_income, match, gapcode) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      mutate(
        estimate = case_when(
          gapcode == "Gap" ~ -estimate,
          TRUE ~ estimate
        ),
        # Ensure factor order is preserved after grouping
        match = factor(match, levels = match_order)
      )
  })
  
  cbsa_data <- reactive({
    gap_join() %>% 
      group_by(year, cbsa_title, household_income, match, gapcode) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      mutate(
        estimate = case_when(
          gapcode == "Gap" ~ -estimate,
          TRUE ~ estimate
        ),
        # Ensure factor order is preserved after grouping
        match = factor(match, levels = match_order)
      )
  })
  
  local_data <- reactive({
    gap_join() %>% 
      group_by(year, name_long, household_income, match, gapcode) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      mutate(
        estimate = case_when(
          gapcode == "Gap" ~ -estimate,
          TRUE ~ estimate
        ),
        # Ensure factor order is preserved after grouping
        match = factor(match, levels = match_order)
      )
  })
  
  # Get available years
  observe({
    years <- unique(gap()$year)
    updateSelectInput(session, "year", 
                      choices = sort(years, decreasing = TRUE),
                      selected = max(years))
  })
  
  # Get available CBSAs
  cbsa_list <- reactive({
    cbsa_data() %>% 
      filter(year == input$year) %>%
      pull(cbsa_title) %>%
      unique() %>%
      sort()
  })
  
  locality_list <- reactive({
    local_data() %>% 
      filter(year == input$year) %>%
      pull(name_long) %>%
      unique() %>%
      sort()
  })
  
  # Parse geography from URL
  current_geo <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    list(
      type = query$geo %||% "state",
      cbsa = query$cbsa,
      locality = query$locality
    )
  })

  # Filter data based on current geography
  filtered_data <- reactive({
    req(input$year)
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      cbsa_data() %>%
        filter(cbsa_title == geo$cbsa,
               year == input$year)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      local_data() %>%
        filter(name_long == geo$locality,
               year == input$year)
    } else {
      state_data() %>%
        filter(year == input$year)
    }
  })

  # Plot title based on geography
  plot_title <- reactive({
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      paste("Housing Supply and Demand Gap in", geo$cbsa)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      paste("Housing Supply and Demand Gap in", geo$locality)
    } else {
      "Virginia Housing Supply and Demand Gap"
    }
  })
  
  # Function to create plots
  create_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Add tooltips to the data
    plot_data <- data %>%
      mutate(
        abs_estimate = abs(estimate),
        tooltip = paste0(
          "Income Level: ", household_income, "\n",
          "Affordability: ", match, "\n",
          "Type: ", gapcode, "\n",
          "Units: ", format(abs_estimate, big.mark = ",")
        )
      )
    
    # Create base plot
    p <- ggplot(plot_data, 
                aes(x = household_income,
                    y = estimate,
                    fill = match,
                    group = match)) +  # ADD group = match for ggiraph
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = paste(household_income, match)),
        position = "stack"
      ) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      scale_fill_manual(
        values = c(
          "Very affordable" = hfv_colors$sky,
          "Affordable" = hfv_colors$grass,
          "Unaffordable" = hfv_colors$berry
        ),
        limits = match_order,  # Use limits instead of breaks to control both legend and plot order
        drop = FALSE  # Don't drop unused levels
      ) +
      scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
      labs(
        title = title_text,
        subtitle = paste("Year:", input$year, "| Negative values represent housing gap"),
        caption = " ", # Add empty caption to leave space for logo
        y = "Number of Housing Units",
        x = "Household Income Level"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title.position = "plot",
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
  
  # Render the plot
  output$plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_plot(filtered_data(), plot_title())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")