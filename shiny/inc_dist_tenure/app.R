# This is a Shiny app that shows the income distribution of
# Virginia geographies by tenure from 2010 to 2022 based on 
# the American Community Survey.

library(shiny)
library(tidyverse)
library(ggplot2)
library(ggiraph)
library(ggtext)
library(glue)
library(hdatools)
library(scales)
library(shinycssloaders)


inc <- read_rds("b25118_data.rds")

cbsa_list <- sort(unique(inc$cbsa_title))
locality_list <- sort(unique(inc$name_long))
year_list <- sort(unique(inc$year))

cbsa_inc <- inc |> 
  group_by(year, cbsa_title, tenure, income) |> 
  summarise(estimate = sum(estimate))

state_inc <- inc |> 
  group_by(year, tenure, income) |> 
  summarise(estimate = sum(estimate))


server <- function(input, output) {
  
  locality <- reactive({ inc |> 
      filter(name_long == input$sel_locality) |> 
      filter(year == input$sel_year)

  })
  
  cbsa <- reactive({ inc |> 
      filter(cbsa_title == input$sel_cbsa) |> 
      filter(year == input$sel_year)
  })
  
  state <- reactive({ inc |> 
      filter(year == input$sel_year)
  })
  
  output$local_plot <- renderGirafe({
    
    gg <- ggplot(locality(),
                 aes(x = income,
                     y = estimate,
                     fill = tenure)) +
      geom_col(position = "dodge") +
      geom_col_interactive(position = "dodge") +
      facet_wrap(~tenure, nrow = 2) + 
      theme_hfv() +
      scale_fill_hfv() +
      labs(title = "Income distribution by tenure",
           caption = "**Source:** U.S. Census Bureau, ACS 5-year estimates, Table B25118. <br>
           **Note:** Income groups are presented in dollars for selected year") +
      scale_y_continuous(labels = number_format(big.mark = ","))  +
      theme(strip.text = element_blank(),
            legend.position = "right")
    
    
    girafe(ggobj = gg, 
           width_svg = 10, 
           height_svg = 8,
           options = list(
             opts_tooltip(css = "background-color:white;color:black;font-family:Verdana;padding:5pt;"),
             opts_sizing(rescale = FALSE),
             opts_toolbar(pngname = input$sel_locality)))
  })
  
  output$cbsa_plot <- renderGirafe({
    
    gg <- ggplot(cbsa(),
                 aes(x = income,
                     y = estimate,
                     fill = tenure)) +
      geom_col(position = "dodge") +
      geom_col_interactive(position = "dodge") +
      facet_wrap(~tenure, nrow = 2) + 
      theme_hfv() +
      scale_fill_hfv() +
      labs(title = "Income distribution by tenure",
           caption = "**Source:** U.S. Census Bureau, ACS 5-year estimates, Table B25118. <br>
           **Note:** Income groups are presented in dollars for selected year") +
      scale_y_continuous(labels = number_format(big.mark = ","))  +
      theme(strip.text = element_blank(),
            legend.position = "right")
    
    girafe(ggobj = gg, 
           width_svg = 10, 
           height_svg = 8,
           options = list(
             opts_tooltip(css = "background-color:white;color:black;font-family:Verdana;padding:5pt;"),
             opts_sizing(rescale = FALSE),
             opts_toolbar(pngname = input$sel_cbsa)))
  })
  
  output$state_plot <- renderGirafe({
    
    gg <- ggplot(state(),
                 aes(x = income,
                     y = estimate,
                     fill = tenure)) +
      geom_col(position = "dodge") +
      geom_col_interactive(position = "dodge") +
      facet_wrap(~tenure, nrow = 2) + 
      theme_hfv() +
      scale_fill_hfv() +
      labs(title = "Income distribution by tenure",
           caption = "**Source:** U.S. Census Bureau, ACS 5-year estimates, Table B25118. <br>
           **Note:** Income groups are presented in dollars for selected year") +
      scale_y_continuous(labels = number_format(big.mark = ","), limits = c(0,200000)) +
      theme(strip.text = element_blank(),
            legend.position = "right")
    
    
    girafe(ggobj = gg, 
           width_svg = 10, 
           height_svg = 8,
           options = list(
             opts_tooltip(css = "background-color:white;color:black;font-family:Verdana;padding:5pt;"),
             opts_sizing(rescale = FALSE),
             opts_toolbar(pngname = "state_pop"))) 
  })
}

ui <- fluidPage(
  sidebarLayout(mainPanel(
    tabsetPanel(type = "tabs", id = "tabselected", selected = 1,
                tabPanel("Statewide", girafeOutput("state_plot") |> withSpinner(hide.ui = FALSE), value =1),
                tabPanel("CBSA", girafeOutput("cbsa_plot"), value =2),
                tabPanel("Locality", girafeOutput("local_plot"), value =3)
    )
  ),
  sidebarPanel(
    conditionalPanel(condition = "input.tabselected==2",
                     selectInput(
                       inputId = "sel_cbsa",
                       label = "Select a CBSA",
                       choices = cbsa_list
                     ),
                     style = "font-family: Verdana;"
    ),
    conditionalPanel(condition = "input.tabselected==3",
                     selectInput(
                       inputId = "sel_locality",
                       label = "Select a locality",
                       choices = locality_list
                     ),
                     style = "font-family: Verdana;"
    ),
    selectInput(
      inputId = "sel_year",
      label = "Select a year",
      choices = year_list
    )
  )
  ))


# Run the application 
shinyApp(ui = ui, server = server)
