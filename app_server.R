# load the necessary package
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
source("scripts/visualizations.R")


#server function for sunshine app
server <- function(input, output){
  data <- reactive({read.csv(
    file =
      "data/aac_intakes_outcomes.csv",
    encoding = "UTF-8",
    stringsAsFactor = FALSE
  )})
  
  ################################################################################
  
  animal_list <- reactive({input$animal_list})
  lower_year <- reactive({input$years[1]})
  upper_year <- reactive({input$years[2]})
  outcome <- reactive({input$out})
  
  output$p1 <- renderPlotly({
      ggplotly2(scatter(data(), animal_list(), lower_year(), upper_year(), outcome()))
    }
  )
}

################################################################################
