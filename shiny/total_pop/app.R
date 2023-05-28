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


total_pop <- read_rds("total_pop.rds")

locality_list <- sort(unique(total_pop$name_long))


server <- function(input, output) {
  
  locality <- reactive({
    filter(total_pop, name_long == input$sel_locality)
  })
  
  output$pop_plot <- renderGirafe({
    
    gg <- ggplot(locality(),
                 aes(x = year,
                     y = value,
                     fill = counttype)) +
      geom_col(position = "dodge") +
      geom_col_interactive(position = "dodge")
    
    girafe(ggobj = gg, 
           width_svg = 10, 
           height_svg = 8,
    options = list(
      opts_tooltip(css = "background-color:white;color:black;font-family:Verdana;padding:5pt;"),
      opts_sizing(rescale = FALSE),
      opts_toolbar(pngname = input$sel_locality)))
})
}

  ui <- fluidPage(
    sidebarLayout(mainPanel(
      girafeOutput("pop_plot")
    ),
    sidebarPanel(
      wellPanel(
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
