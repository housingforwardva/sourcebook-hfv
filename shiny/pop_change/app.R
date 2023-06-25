# This is a Shiny app that shows the components of population change for
# Virginia geographies from 2010 to 2022 based on 
# Census Population Estimates Program and the Decennial
# Census.

library(shiny)
library(tidyverse)
library(ggplot2)
library(ggiraph)
library(ggtext)
library(glue)
library(hdatools)
library(scales)


pop_change <- read_rds("pop_change.rds")

cbsa_list <- sort(unique(pop_change$cbsa_title))

locality_list <- sort(unique(pop_change$name_long))

cbsa_pop <- pop_change |> 
  group_by(year, cbsa_title, component) |> 
  summarise(value = sum(value))

state_pop <- pop_change |> 
  group_by(year, component) |> 
  summarise(value = sum(value))


server <- function(input, output) {
  
  locality <- reactive({
    filter(pop_change, name_long == input$sel_locality)
  })
  
  cbsa <- reactive({
    filter(pop_change, cbsa_title == input$sel_cbsa)
  })
  
  output$local_plot <- renderGirafe({
    
    gg <- ggplot(locality(),
                 aes(x = year,
                     y = value,
                     fill = component)) +
      geom_col(position = "stack") +
      geom_col_interactive(position = "stack") +
      theme_hfv() +
      scale_fill_hfv() +
      labs(title = "Local components of population change",
           caption = "**Source:** U.S. Census Bureau, Population Estimates Program and Decennial Census.") +
      scale_y_continuous(labels = number_format(big.mark = ","))
    
    
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
                 aes(x = year,
                     y = value,
                     fill = component)) +
      geom_col(position = "stack") +
      geom_col_interactive(position = "stack") +
      theme_hfv() +
      scale_fill_hfv() +
      labs(title = "Core-based statistical area components of population change",
           caption = "**Source:** U.S. Census Bureau, Population Estimates Program and Decennial Census.") +
      scale_y_continuous(labels = number_format(big.mark = ","))
    
    girafe(ggobj = gg, 
           width_svg = 10, 
           height_svg = 8,
           options = list(
             opts_tooltip(css = "background-color:white;color:black;font-family:Verdana;padding:5pt;"),
             opts_sizing(rescale = FALSE),
             opts_toolbar(pngname = input$sel_cbsa)))
  })
  
  output$state_plot <- renderGirafe({
    
    gg <- ggplot(state_pop,
                 aes(x = year,
                     y = value,
                     fill = component)) +
      geom_col(position = "stack") +
      geom_col_interactive(position = "stack") +
      theme_hfv() +
      scale_fill_hfv() +
      labs(title = "Virginia components of population change",
           caption = "**Source:** U.S. Census Bureau, Population Estimates Program and Decennial Census.") +
      scale_y_continuous(labels = number_format(big.mark = ","))
    
    
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
                tabPanel("Statewide", girafeOutput("state_plot"), value =1),
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
    )
  )
  ))


# Run the application 
shinyApp(ui = ui, server = server)
