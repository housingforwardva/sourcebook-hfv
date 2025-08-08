library(shiny)
library(tidyverse)
library(forcats)     # For factor reordering
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

# Define HFV color palette
hfv_colors <- list(
  sky = "#40C0C0",
  grass = "#259591",
  lilac = "#8B85CA", 
  shadow = "#011E41",
  shadow_light = "#102C54",  # Lighter shade of shadow color
  berry = "#B1005F",
  desert = "#E0592A",
  grey = "#E8E9EB"
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
      h4("Cost Burden by Race and Ethnicity", class = "hfv-title")
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

        # Geography selectors
        div(
          style = "margin-bottom: 16px;",
          conditionalPanel(
            condition = "input.tabs == 'cbsa'",
            selectInput(
              "cbsa",
              "Metro Area:",
              choices = NULL,
              width = "100%",
              selectize = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.tabs == 'local'",
            selectInput(
              "locality",
              "Locality:",
              choices = NULL,
              width = "100%",
              selectize = TRUE
            )
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

# Define the order of cost burden levels
cost_burden_order <- c("Not cost-burdened", "No or negative income", "Cost-burdened", "Severely cost-burdened")

# Server function
server <- function(input, output, session) {
  # Load the data
  cb9 <- reactive({
    read_rds(here("data", "rds", "table9_chas.rds")) %>% 
      mutate(cost_burden = fct_reorder(factor(cost_burden, levels = cost_burden_order), match(cost_burden, cost_burden_order)))
  })
  
  # Load lookup table
  lookup <- reactive({
    read_csv(here("data", "local_lookup.csv")) %>% 
      mutate(fips = fips_full)
  })
  
  # Join data with lookup
  cb9_join <- reactive({
    cb9() %>% 
      left_join(lookup(), by = "fips")
  })
  
  # Pre-compute state, CBSA, and local data
  state_data <- reactive({
    cb9() %>% 
      group_by(year, race, cost_burden, cb_group) %>% 
      summarise(estimate = sum(estimate),
                moe = sqrt(sum(moe^2, na.rm = TRUE)), .groups = "drop")
  })
  
  cbsa_data <- reactive({
    cb9_join() %>% 
      group_by(year, cbsa_title, race, cost_burden, cb_group) %>% 
      summarise(estimate = sum(estimate),
                moe = sqrt(sum(moe^2, na.rm = TRUE)), .groups = "drop")
  })
  
  local_data <- reactive({
    cb9_join() %>% 
      group_by(year, name_long, race, cost_burden, cb_group) %>% 
      summarise(estimate = sum(estimate),
                moe = sqrt(sum(moe^2, na.rm = TRUE)), .groups = "drop")
  })
  
  # Get available years
  observe({
    years <- unique(cb9()$year)
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
  
  # Filter data for plots
  filtered_state <- reactive({
    req(input$year)
    
    state_data() %>%
      filter(year == input$year) %>%
      group_by(race) %>%
      mutate(percent = estimate/sum(estimate)) %>%
      ungroup() %>%
      group_by(race, year) %>%
      mutate(total_cb = sum(estimate[cb_group == "Cost-burdened"])/sum(estimate)) %>%
      ungroup() %>%
      mutate(cost_burden = fct_reorder(factor(cost_burden, levels = cost_burden_order), match(cost_burden, cost_burden_order)))
  })
  
  filtered_cbsa <- reactive({
    req(input$cbsa, input$year)
    
    cbsa_data() %>%
      filter(cbsa_title == input$cbsa,
             year == input$year) %>%
      group_by(race) %>%
      mutate(percent = estimate/sum(estimate)) %>%
      ungroup() %>%
      group_by(race, year) %>%
      mutate(total_cb = sum(estimate[cb_group == "Cost-burdened"])/sum(estimate)) %>%
      ungroup() %>%
      mutate(cost_burden = fct_reorder(factor(cost_burden, levels = cost_burden_order), match(cost_burden, cost_burden_order)))
  })
  
  filtered_local <- reactive({
    req(input$locality, input$year)
    
    local_data() %>%
      filter(name_long == input$locality,
             year == input$year) %>%
      group_by(race) %>%
      mutate(percent = estimate/sum(estimate)) %>%
      ungroup() %>%
      group_by(race, year) %>%
      mutate(total_cb = sum(estimate[cb_group == "Cost-burdened"])/sum(estimate)) %>%
      ungroup() %>%
      mutate(cost_burden = fct_reorder(factor(cost_burden, levels = cost_burden_order), match(cost_burden, cost_burden_order)))
  })
  
# Function to create plots
create_plot <- function(data, title_text) {
  req(nrow(data) > 0)
  
  # Add tooltips to the data
  plot_data <- data %>%
    mutate(tooltip = paste0(
      "Race/Ethnicity: ", race, "\n",
      "Cost Burden: ", cost_burden, "\n",
      "Percentage: ", format(percent * 100, digits = 1), "%\n",
      "Households: ", format(estimate, big.mark = ",")
    ))
    
    # Create base plot
    p <- ggplot(plot_data, 
                aes(x = reorder(race, -total_cb),
                    y = percent,
                    fill = cost_burden,
                    tooltip = tooltip)) +
      geom_col_interactive(position = "stack") +
      scale_fill_manual(
        values = c(
          "Not cost-burdened" = hfv_colors$grey,
          "No or negative income" = hfv_colors$lilac,
          "Cost-burdened" = hfv_colors$desert,
          "Severely cost-burdened" = hfv_colors$berry
        ),
        breaks = cost_burden_order
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = title_text,
        subtitle = paste("Year:", input$year, "| Ordered by total cost burden rate"),
        caption = " ", # Add empty caption to leave space for logo
        y = "Percentage of Households",
        x = "Race and Ethnicity"
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
  
  # Render the plots
  output$state_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_plot(filtered_state(), "Virginia Cost Burden by Race and Ethnicity")))
  })
  
  output$cbsa_plot <- renderGirafe({
    title_text <- paste("Cost Burden by Race and Ethnicity in", input$cbsa)
    suppressWarnings(create_interactive_plot(create_plot(filtered_cbsa(), title_text)))
  })
  
  output$local_plot <- renderGirafe({
    title_text <- paste("Cost Burden by Race and Ethnicity in", input$locality)
    suppressWarnings(create_interactive_plot(create_plot(filtered_local(), title_text)))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)