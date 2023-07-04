# This is a Shiny app that shows the total population
# of Virginia geographies by race and ethnicity from 2010 to 2020 based on 
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

race <- read_rds("race_data.rds")

cbsa_list <- sort(unique(race$cbsa_title))

locality_list <- sort(unique(race$name_long))

# cbsa <- race |> 
#   group_by(year, cbsa_title, label) |> 
#   summarise(value = sum(value)) 

# state <- race |> 
#   group_by(year, label) |> 
#   summarise(value = sum(value))



server <- function(input, output) {
  
  
  locality <- reactive({ race |> 
    filter(name_long == input$sel_locality) |> 
      filter(year == input$sel_year) |> 
      mutate(percent = value/sum(value))
      
  })
  
  cbsa <- reactive({race |> 
      group_by(year, cbsa_title, label) |> 
      summarise(value = sum(value))  |> 
      filter(cbsa_title == input$sel_cbsa) |> 
      filter(year == input$sel_year) |> 
      mutate(percent = value/sum(value))
  })
  
  
  state <- reactive({race |> 
      group_by(year, label) |> 
      summarise(value = sum(value)) |> 
      filter(year == input$sel_year) |> 
      mutate(percent = value/sum(value))})
  
  output$local_plot <- renderGirafe({
    
    gg <- ggplot(locality(),
                 aes(x = reorder(label, -percent),
                     y = percent,
                     fill = label)) +
      geom_col(position = "dodge") +
      geom_col_interactive(position = "dodge") +
      theme_hfv(base_size = 15) +
      scale_fill_hfv() +
      labs(title = "Population by race and ethnicity",
           caption = "**Source:** U.S. Census Bureau, Population Estimates Program and Decennial Census.") +
      scale_y_continuous(labels = percent_format(), limits = c(0,1))
    
    
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
                 aes(x = reorder(label, -percent),
                     y = percent,
                     fill = label)) +
      geom_col(position = "dodge") +
      geom_col_interactive(position = "dodge") +
      theme_hfv(base_size = 15) +
      scale_fill_hfv() +
      labs(title = "Population by race and ethnicity",
           caption = "**Source:** U.S. Census Bureau, Population Estimates Program and Decennial Census.") +
      scale_y_continuous(labels = percent_format(), limits = c(0,1))
    
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
                 aes(x = reorder(label, -percent),
                     y = percent,
                     fill = label)) +
      geom_col(position = "dodge") +
      geom_col_interactive(position = "dodge") +
      theme_hfv(base_size = 15) +
      scale_fill_hfv() +
      labs(title = "Population by race and ethnicity",
           caption = "**Source:** U.S. Census Bureau, Population Estimates Program and Decennial Census.") +
      scale_y_continuous(labels = percent_format(), limits = c(0,1))
    
    
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
    ),
    selectInput(inputId = "sel_year",
                label = "Select year",
                choices = as.character(c(2010:2020)),
                selected = "2020"
  ))))


# Run the application 
shinyApp(ui = ui, server = server)
