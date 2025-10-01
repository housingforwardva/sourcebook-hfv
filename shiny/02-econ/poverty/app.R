library(shiny)
library(tidyverse)
library(ggiraph)
library(here)
library(grid)
library(png)
library(bslib)
library(cowplot)
library(scales)
library(shinyjs)
library(magick)
library(sass)
library(gdtools)
library(gfonts)

# =============================================================================
# HFV STYLING SYSTEM INTEGRATION
# =============================================================================

# Register Google Fonts
register_gfont("Open Sans")
register_gfont("Poppins")

tryCatch(
  {
    message("Google Fonts registered for web rendering")
  },
  error = function(e) {
    message("Font registration warning: ", e$message)
  }
)

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

# Create HFV bslib theme
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

# Load data
poverty_age <- read_rds("age_data.rds")
poverty_race <- read_rds("race_data.rds")

# Create filter lists - consolidated function
create_filter_lists <- function() {
  list(
    states = poverty_age %>%
      ungroup() %>%
      filter(geography == "state") %>%
      distinct(NAME) %>%
      arrange(NAME) %>%
      pull(NAME),

    cbsas = poverty_age %>%
      ungroup() %>%
      filter(geography == "cbsa") %>%
      distinct(NAME) %>%
      arrange(NAME) %>%
      pull(NAME),

    localities = poverty_age %>%
      ungroup() %>%
      filter(geography == "county") %>%
      distinct(NAME) %>%
      arrange(NAME) %>%
      pull(NAME),

    ages = poverty_age %>%
      ungroup() %>%
      filter(geography == "county") %>%
      distinct(age_group) %>%
      arrange(age_group) %>%
      pull(age_group),

    races = poverty_race %>%
      ungroup() %>%
      filter(geography == "county") %>%
      distinct(race) %>%
      arrange(race) %>%
      pull(race)
  )
}

filter_lists <- create_filter_lists()

# Define colors
race_colors <- c(
  "White, Not Hispanic Or Latino" = "#40C0C0",
  "Black" = "#011E41",
  "Asian" = "#259591",
  "Some Other Race" = "#E0592A",
  "Multiracial" = "#B1005F",
  "American Indian/Alaska Native" = "#8B85CA",
  "Native Hawaiian/Pacific Islander" = "#FFC658",
  "All households" = "#FF7276"
)

# Updated age colors for the broader age groups in your data
age_colors <- c(
  "Young (Under 35)" = "#40C0C0",
  "Middle-aged (35-64)" = "#259591",
  "Older adults (65+)" = "#011E41"
)

# Define UI
ui <- function(request) {
  page_fillable(
    theme = hfv_theme,
    useShinyjs(),

    div(
      class = "hfv-container",

      div(
        class = "hfv-header",
        h4("Poverty Rate Analysis", class = "hfv-title")
      ),

      layout_columns(
        col_widths = c(lg = c(3, 9), md = c(4, 8), sm = 12),
        gap = "16px",

        # Sidebar Panel
        div(
          h5(
            "Dashboard Controls",
            class = "text-primary",
            style = "margin-bottom: 16px;"
          ),

          # Analysis type selector
          div(
            style = "margin-bottom: 16px;",
            radioButtons(
              "analysis_type",
              "Analysis Type:",
              choices = list(
                "By Race/Ethnicity" = "race",
                "By Age Group" = "age"
              ),
              selected = "race"
            )
          ),

          hr(style = "margin: 24px 0; border-color: #ced4da;"),

          # Data source
          div(
            style = "font-size: 0.75rem; color: #6c757d; line-height: 1.4;",
            p(
              strong("Data Source:"),
              br(),
              "U.S. Census Bureau, American Community Survey 5-Year Estimates",
              style = "margin-bottom: 0;"
            )
          )
        ),

        # Main Panel
        div(
          style = "height: 450px; margin-top: 16px;",
          girafeOutput("plot", height = "100%")
        )
      )
    )
  )
}

# Server function
server <- function(input, output, session) {
  # Get current geography from URL
  current_geo <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    list(
      type = query$geo %||% "state",
      cbsa = query$cbsa,
      locality = query$locality
    )
  })

  # Consolidated data processing function
  process_data <- function(data, geography, name_filter = NULL) {
    # Filter by geography and ungroup to avoid grouping issues
    filtered_data <- data %>%
      ungroup() %>%
      filter(geography == !!geography) |>
      filter(poverty == "Below")

    # Apply name filter if provided
    if (!is.null(name_filter)) {
      filtered_data <- filtered_data %>% filter(NAME == !!name_filter)
    }

    # Process data - use existing rates since they're already calculated correctly
    if ("race" %in% names(filtered_data)) {
      # Race data
      result <- filtered_data %>%
        mutate(demographic = race)

      # Order by mean rate for faceting
      demo_summary <- result %>%
        group_by(demographic) %>%
        summarize(mean_rate = mean(rate, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(mean_rate))

      result <- result %>%
        mutate(
          demographic_ordered = factor(
            demographic,
            levels = demo_summary$demographic
          )
        )
    } else {
      # Age data - Fixed: ensure proper ordering and faceting
      result <- filtered_data %>%
        mutate(demographic = age_group)

      # For age data, order by the predefined age_colors order
      result <- result %>%
        mutate(
          demographic_ordered = factor(age_group, levels = names(age_colors))
        )
    }

    # Clean up data for plotting
    result <- result %>%
      filter(!is.na(rate), !is.infinite(rate), rate >= 0) %>%
      arrange(year, demographic_ordered) # Changed to use demographic_ordered

    return(result)
  }

  # Consolidated plot creation function
  create_plot <- function(data, title_text, analysis_type) {
    req(nrow(data) > 0)

    # Get colors based on analysis type
    color_palette <- if (analysis_type == "race") race_colors else age_colors

    # Get latest year data for labels
    latest_year <- max(data$year, na.rm = TRUE)
    latest_data <- data %>% filter(year == latest_year)

    # Create tooltips
    plot_data <- data %>%
      mutate(
        tooltip = paste0(
          if (analysis_type == "race") "Race/Ethnicity: " else "Age Group: ",
          demographic,
          "\n",
          "Year: ",
          year,
          "\n",
          "Poverty Rate: ",
          scales::percent(rate, accuracy = 0.1),
          "\n",
          "Number in Poverty: ",
          format(estimate, big.mark = ",")
        )
      )

    latest_data <- latest_data %>%
      mutate(
        tooltip = paste0(
          if (analysis_type == "race") "Race/Ethnicity: " else "Age Group: ",
          demographic,
          "\n",
          "Year: ",
          year,
          "\n",
          "Poverty Rate: ",
          scales::percent(rate, accuracy = 0.1),
          "\n",
          "Number in Poverty: ",
          format(estimate, big.mark = ",")
        )
      )

    # Create base plot with proper grouping - fix color mapping for age plots
    if (analysis_type == "age") {
      p <- ggplot(
        plot_data,
        aes(
          x = year,
          y = rate,
          color = age, # Use age_group for color in age plots
          group = age
        )
      ) +
        facet_wrap(~age_group) +
        geom_line_interactive(
          aes(tooltip = tooltip, data_id = paste(year, demographic)),
          linewidth = 1,
          na.rm = TRUE
        ) +
        geom_point_interactive(
          aes(tooltip = tooltip, data_id = paste(year, demographic)),
          size = 2,
          na.rm = TRUE
        ) +
        # Add labels for latest values
        geom_text(
          data = latest_data,
          aes(label = scales::percent(rate, accuracy = 0.1), color = age_group),
          hjust = -0.3,
          vjust = 0.5,
          size = 3
        )
    } else {
      p <- ggplot(
        plot_data,
        aes(
          x = year,
          y = rate,
          color = demographic_ordered,
          group = demographic_ordered
        )
      ) +
        geom_line_interactive(
          aes(tooltip = tooltip, data_id = paste(year, demographic)),
          linewidth = 1,
          na.rm = TRUE
        ) +
        geom_point_interactive(
          aes(tooltip = tooltip, data_id = paste(year, demographic)),
          size = 2,
          na.rm = TRUE
        ) +
        # Add labels for latest values
        geom_text(
          data = latest_data,
          aes(label = scales::percent(rate, accuracy = 0.1)),
          hjust = -0.3,
          vjust = 0.5,
          size = 3
        )
    }

    p <- p +
      # Fixed: Ensure faceting happens for both race and age data
      facet_wrap(
        ~demographic_ordered,
        nrow = 1,
        labeller = labeller(demographic_ordered = function(x) {
          str_wrap(x, width = 12)
        })
      ) +
      scale_color_manual(values = color_palette) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
      labs(
        title = title_text,
        caption = " ",
        y = "Poverty Rate",
        x = "Year"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 6, margin = margin(4, 4, 4, 4)),
        strip.text.x = element_text(size = 6, margin = margin(b = 5, t = 5)),
        panel.spacing = unit(1, "lines"),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 15, 35, 5)
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

  # Single plot that responds to current geography
  output$plot <- renderGirafe({
    geo <- current_geo()
    data_source <- if (input$analysis_type == "race") {
      poverty_race
    } else {
      poverty_age
    }

    # Determine geography and name based on URL parameters
    if (geo$type == "state") {
      geography <- "state"
      name_filter <- "Virginia"
      location_name <- "Virginia"
    } else if (geo$type == "cbsa" && !is.null(geo$cbsa)) {
      geography <- "cbsa"
      name_filter <- geo$cbsa
      location_name <- geo$cbsa
    } else if (geo$type == "locality" && !is.null(geo$locality)) {
      geography <- "county"
      name_filter <- geo$locality
      location_name <- geo$locality
    } else {
      return(NULL)
    }

    data <- process_data(data_source, geography, name_filter)
    title <- paste(
      "Poverty Rate by",
      if (input$analysis_type == "race") "Race/Ethnicity" else "Age Group",
      "in",
      location_name
    )

    req(nrow(data) > 0)
    plot_obj <- create_plot(data, title, input$analysis_type)

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
  })

  # Hover info (simplified since each tab now has its own plot)
  output$hover_info <- renderText({
    "Hover over a point for details"
  })

  # Handle responsive window events
  observe({
    session$sendCustomMessage(type = "plot-redraw", message = list())
  })
}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")
