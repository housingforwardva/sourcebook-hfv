# This is a Shiny app that shows the total population
# of Virginia geographies from 2010 to 2022 based on 
# Census Population Estimates Program and the Decennial
# Census.

library(shiny)
library(tidyverse)
library(ggplot2)
library(ggiraph)
library(ggtext)
library(glue)
library(hdatools)


total_pop <- read_rds("total_pop.rds")

cbsa_list <- sort(unique(total_pop$cbsa_title))
locality_list <- sort(unique(total_pop$name_long))

cbsa_pop <- total_pop |> 
  group_by(year, cbsa_title, counttype) |> 
  summarise(value = sum(value))

state_pop <- total_pop |> 
  group_by(year, counttype) |> 
  summarise(value = sum(value))


server <- function(input, output) {
  
  locality <- reactive({
    filter(total_pop, name_long == input$sel_locality)
  })
  
  cbsa <- reactive({
    filter(cbsa_pop, cbsa_title == input$sel_cbsa)
  })
  
  output$local_plot <- renderGirafe({
    
    gg <- ggplot(locality(),
                 aes(x = year,
                     y = value,
                     fill = counttype)) +
      geom_col(position = "dodge") +
      geom_col_interactive(position = "dodge") +
      theme_hfv() +
      scale_fill_hfv() +
      labs(title = "Local population",
           caption = "**Source:** U.S. Census Bureau, Population Estimates Program and Decennial Census.")
    
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
                     fill = counttype)) +
      geom_col(position = "dodge") +
      geom_col_interactive(position = "dodge") +
      theme_hfv() +
      scale_fill_hfv()
    
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
                     fill = counttype)) +
      geom_col(position = "dodge") +
      geom_col_interactive(position = "dodge") +
      theme_hfv() +
      scale_fill_hfv()
    
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
