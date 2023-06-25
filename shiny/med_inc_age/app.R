# This is a Shiny app that shows the median household income
# of Virginia geographies by tenure from 2010 to 2021 based on 
# the American Community Survey 5-year estimates.

library(shiny)
library(tidyverse)
library(ggplot2)
library(ggiraph)
library(ggtext)
library(glue)
library(hdatools)
library(scales)
library(shinyWidgets)


state_data <- read_rds("b19049_state.rds") |> 
  mutate(year = as.character(year))

cbsa_data <- read_rds("b19049_cbsa.rds") |> 
  mutate(year = as.character(year))

local_data <- read_rds("b19049_locality.rds") |> 
  mutate(year = as.character(year)) |> 
  mutate(adjusted = as.numeric(adjusted))

state_list <- sort(unique(state_data$state))
cbsa_list <- sort(unique(cbsa_data$cbsa))
locality_list <- sort(unique(local_data$locality))

age_list <- sort(unique(local_data$age))

server <- function(input, output) {
  
  locality <- reactive({ local_data |>
      filter(locality == input$sel_locality) |>
      filter(age == input$sel_age)
  })
  
  cbsa <- reactive({ cbsa_data |> 
      filter(cbsa == input$sel_cbsa) |> 
      filter(age == input$sel_age)
  })
  
  state <- reactive({ state_data |> 
      filter(state == input$sel_state) |> 
      filter(age == input$sel_age)
  })
  
  
  output$local_plot <- renderGirafe({
    
    validate(
      need(
        input$sel_age != "",
        "Please select at least one age option."
      )
    )
    
    gg <- ggplot(locality(),
                 aes(x = year,
                     y = adjusted,
                     color = age,
                     group = age)) + 
      geom_line() +
      geom_point_interactive(aes(
        data_id = adjusted,
        tooltip = dollar_format()(adjusted)
      )) + 
      theme_hfv() +
      scale_color_hfv() +
      labs(title = "Median household income by age",
           caption = "**Source:** U.S. Census Bureau, ACS 5-year estimates, Table B19049. <br> 
           **Note:** Income has been adjusted to 2021 dollars using the Consumer Price Index.") + 
      scale_y_continuous(labels = dollar_format())  + 
      theme(legend.position = "right",
            axis.text.x = element_text(angle = 90, vjust = 0.5))
    
    girafe(ggobj = gg, 
           width_svg = 10, 
           height_svg = 8,
           options = list(
             opts_tooltip(css = "background-color:white;color:black;font-family:Verdana;padding:5pt;"),
             opts_sizing(rescale = FALSE),
             opts_toolbar(pngname = input$sel_locality)))
  })
  
  output$cbsa_plot <- renderGirafe({
    
    validate(
      need(
        input$sel_age != "",
        "Please select at least one age option."
      )
    )
    
    gg <- ggplot(cbsa(),
                 aes(x = year,
                     y = adjusted,
                     color = age,
                     group = age)) +
      geom_line() +
      geom_point_interactive(aes(
        data_id = adjusted,
        tooltip = dollar_format()(adjusted)
      )) + 
      theme_hfv() +
      scale_color_hfv() +
      labs(title = "Median household income by age",
           caption = "**Source:** U.S. Census Bureau, ACS 5-year estimates, Table B19049. <br> 
           **Note:** Income has been adjusted to 2021 dollars using the Consumer Price Index.")  +
      theme(legend.position = "right",
            axis.text.x = element_text(angle = 90, vjust = 0.5))
    
    girafe(ggobj = gg, 
           width_svg = 10, 
           height_svg = 8,
           options = list(
             opts_tooltip(css = "background-color:white;color:black;font-family:Verdana;padding:5pt;"),
             opts_sizing(rescale = FALSE),
             opts_toolbar(pngname = input$sel_cbsa)))
  })
  
  output$state_plot <- renderGirafe({
    
    validate(
      need(
        input$sel_age != "",
        "Please select at least one age option."
      )
    )
    
    gg <- ggplot(state(),
                 aes(x = year,
                     y = adjusted,
                     color = age,
                     group = age)) +
      geom_line() +
      geom_point_interactive(aes(
        data_id = adjusted,
        tooltip = dollar_format()(adjusted)
      )) + 
      theme_hfv() +
      scale_color_hfv() +
      labs(title = "Median household income by age",
           caption = "**Source:** U.S. Census Bureau, ACS 5-year estimates, Table B19049. <br> 
           **Note:** Income has been adjusted to 2021 dollars using the Consumer Price Index.") +
      scale_y_continuous(labels = dollar_format())  +
      theme(legend.position = "right",
            axis.text.x = element_text(angle = 90, vjust = 0.5))
    
    
    girafe(ggobj = gg, 
           width_svg = 10, 
           height_svg = 8,
           options = list(
             opts_tooltip(css = "background-color:white;color:black;font-family:Verdana;padding:5pt;"),
             opts_sizing(rescale = FALSE),
             opts_toolbar(pngname = "state_pop"),
             opts_selection(type = "none"))) 
  })
}

ui <- fluidPage(
  sidebarLayout(mainPanel(
    tabsetPanel(type = "tabs", id = "tabselected", selected = 1,
                tabPanel("State", girafeOutput("state_plot"), value =1),
                tabPanel("CBSA", girafeOutput("cbsa_plot"), value =2),
                tabPanel("Locality", girafeOutput("local_plot"), value =3)
    )
  ),
  sidebarPanel(
    conditionalPanel(condition = "input.tabselected==1",
                     selectInput(
                       inputId = "sel_state",
                       label = "Select a state",
                       choices = state_list,
                       selected = "Virginia"
                     ),
                     style = "font-family: Verdana;"
    ),
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
    pickerInput(inputId = "sel_age",
                label = "Select age of householder",
                choices = age_list,
                multiple = TRUE,
                selected = age_list
    ),
    style = "font-family: Verdana;"
  )))


# Run the application 
shinyApp(ui = ui, server = server)
