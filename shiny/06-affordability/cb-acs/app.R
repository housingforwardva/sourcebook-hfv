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
library(gdtools)
library(gfonts)

# =============================================================================
# COST BURDEN BY AMI VISUALIZATION
# =============================================================================

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

# =============================================================================
# LOAD DATA OUTSIDE SERVER
# =============================================================================

# Define the order of cost burden levels
cost_burden_order <- c("Not cost-burdened", "Cost-burdened")

# Load the data
cb <- read_rds("./data.rds") %>% 
  mutate(cost_burden = fct_reorder(factor(cost_burden, levels = cost_burden_order), match(cost_burden, cost_burden_order))) %>% 
  mutate(household_income = factor(income, 
                                   levels = c("Less than $20,000", 
                                              "$20,000 to $34,999", 
                                              "$35,000 to $49,999", 
                                              "$50,000 to $74,999",
                                              "$75,000 or more")))


# Get available options
year_list <- sort(unique(cb$year), decreasing = TRUE)
cbsa_list <- sort(unique(cb$cbsa_title))
locality_list <- sort(unique(cb$name_long))

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

# =============================================================================
# USER INTERFACE
# =============================================================================

# Define UI
ui <- function(request) {
  page_fillable(
    theme = hfv_theme,
    includeCSS("www/styles/hfv-theme.css"),
    useShinyjs(),

    # Main container using HFV classes
    div(
      class = "hfv-container",

      # Header using HFV styling
      div(
        class = "hfv-header",
        h4("Cost Burden by Income Level", class = "hfv-title")
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

          h5("Filters",
             class = "text-primary", style = "margin-bottom: 16px;"),

          # Year select
          div(
            style = "margin-bottom: 16px;",
            selectInput(
              "year",
              "Select Year:",
              choices = year_list,
              selected = max(year_list),
              width = "100%",
              selectize = FALSE
            )
          ),

          # Tenure select
          div(
            style = "margin-bottom: 16px;",
            selectInput(
              "tenure",
              "Select Tenure:",
              choices = c("Homeowner", "Renter"),
              selected = "Renter",
              width = "100%",
              selectize = FALSE
            )
          ),

          # Divider
          hr(style = "margin: 24px 0; border-color: #ced4da;"),

          # Data source
          div(
            style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
            p(
              strong("Data Source:"), br(),
              "U.S. Census Bureau, American Community Survey 5-Year Estimates, Table B25106.",
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

# =============================================================================
# SERVER FUNCTION
# =============================================================================
server <- function(input, output, session) {

  
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
    req(input$year, input$tenure)
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      cb %>%
        filter(cbsa_title == geo$cbsa,
               year == input$year,
               tenure == input$tenure) %>%
        group_by(household_income, cost_burden) %>%
        summarise(estimate = sum(estimate)) %>%
        ungroup() %>%
        group_by(household_income) %>%
        mutate(percent = estimate/sum(estimate)) %>%
        ungroup() %>%
        mutate(cost_burden = fct_reorder(factor(cost_burden, levels = cost_burden_order), match(cost_burden, cost_burden_order)))
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      cb %>%
        filter(name_long == geo$locality,
               year == input$year,
               tenure == input$tenure) %>%
        group_by(household_income) %>%
        ungroup() %>%
        group_by(household_income) %>%
        mutate(percent = estimate/sum(estimate)) %>%
        ungroup() %>%
        mutate(cost_burden = fct_reorder(factor(cost_burden, levels = cost_burden_order), match(cost_burden, cost_burden_order)))
    } else {
      cb %>%
        filter(year == input$year,
               tenure == input$tenure) %>%
        group_by(household_income, cost_burden) %>%
        summarise(estimate = sum(estimate)) %>%
        ungroup() %>%
        group_by(household_income) %>%
        mutate(percent = estimate/sum(estimate)) %>%
        ungroup() %>%
        mutate(cost_burden = fct_reorder(factor(cost_burden, levels = cost_burden_order), match(cost_burden, cost_burden_order)))
    }
  })

  # Plot title based on geography
  plot_title <- reactive({
    geo <- current_geo()

    if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      paste("Cost Burden by Income Level in", geo$cbsa)
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      paste("Cost Burden by Income Level in", geo$locality)
    } else {
      "Virginia Cost Burden by Income Level"
    }
  })
  
  # Function to create plots
  create_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Add tooltips to the data
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "AMI: ", household_income, "\n",
        "Cost Burden: ", cost_burden, "\n",
        "Percentage: ", format(percent * 100, digits = 1), "%\n",
        "Households: ", format(estimate, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data, 
                aes(x = household_income,
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
        subtitle = paste("Year:", input$year, "| Tenure:", input$tenure),
        caption = " ", # Add empty caption to leave space for logo
        y = "Percentage of Households",
        x = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title.position = "plot",
        axis.text = element_text(size = 9),
        strip.text = element_text(size = 8),
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