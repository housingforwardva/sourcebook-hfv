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
ui <- page_fillable(
  theme = hfv_theme,
  useShinyjs(), # Initialize shinyjs

  # Main container using HFV classes
  div(
    class = "hfv-container",
    
    # Header using HFV styling
    div(
      class = "hfv-header",
      h4("HUD AMI Limits", class = "hfv-title")
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
        
        # County select
        div(
          style = "margin-bottom: 16px;",
          selectInput(
            "county",
            "Select County/City:",
            choices = NULL,
            selected = NULL,
            width = "100%",
            selectize = TRUE
          )
        ),

        # Household Size select
        div(
          style = "margin-bottom: 16px;",
          selectInput(
            "hh_size",
            "Household Size:",
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
            "U.S. Department of Housing and Urban Development (HUD), Section 8 Income Limits.",
            style = "margin-bottom: 0;"
          )
        )
      ),
        
      # Main Panel with plot
      div(
        div(
          class = "hfv-chart-container",
          style = "height: 450px; margin-top: 16px;",
          girafeOutput("income_plot", height = "100%")
        )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  # Load the data
  hud_il <- reactive({
    read_rds(here("data", "rds", "va_hud_ami.rds")) %>% 
      mutate(ami = factor(ami, levels = c("Extremely low-income",
                                          "Very low-income",
                                          "Low-income"))) %>% 
      mutate(ami_pct = case_when(
        ami == "Extremely low-income" ~ "30% AMI",
        ami == "Very low-income" ~ "50% AMI",
        ami == "Low-income" ~ "80% AMI"
      ))
  })
  
  # Get available counties
  observe({
    counties <- unique(hud_il()$county_name) %>% sort()
    updateSelectInput(session, "county", 
                      choices = counties,
                      selected = if("Richmond city" %in% counties) "Richmond city" else counties[1])
  })
  
  # Get available household sizes
  observe({
    hh_sizes <- unique(hud_il()$hh_size) %>% sort()
    updateSelectInput(session, "hh_size", 
                      choices = hh_sizes,
                      selected = if("One-person" %in% hh_sizes) "One-person" else hh_sizes[1])
  })
  
  # Filter data for plots
  filtered_data <- reactive({
    req(input$county, input$hh_size)
    
    hud_il() %>%
      filter(county_name == input$county,
             hh_size == input$hh_size)
  })
  
  # Function to create plot
  create_plot <- function(data) {
    req(nrow(data) > 0)
    
    # Add tooltips to the data
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Year: ", year, "\n",
        "Income Category: ", ami_pct, "\n",
        "Income Limit: $", format(limit, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data, 
                aes(x = year,
                    y = limit,
                    fill = ami_pct)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = paste(year, ami_pct)),
        position = "dodge"
      ) +
      facet_wrap(~ami_pct) +
      scale_fill_manual(
        values = c(
          "30% AMI" = hfv_colors$berry,
          "50% AMI" = hfv_colors$desert, 
          "80% AMI" = hfv_colors$sky
        ),
        breaks = c("30% AMI", "50% AMI", "80% AMI")
      ) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(
        title = paste("HUD AMI Limits for", input$county),
        subtitle = paste("Household Size:", input$hh_size),
        caption = " ", # Add empty caption to leave space for logo
        y = "Income Limit",
        x = "Year",
        fill = "Income Category"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        legend.title = element_blank(),
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
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
  
  # Render the income plot
  output$income_plot <- renderGirafe({
    suppressWarnings(create_interactive_plot(create_plot(filtered_data())))
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)