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
  
################################################################################
  
  animal_list_time <- reactive({input$animal_list_time})
  sep <- reactive({input$sep})
  
  output$p2 <- renderPlotly({
    ggplotly2(time_series(data(), animal_list_time(), sep()))
  })
  
################################################################################
  
  outcome_or_intake <- reactive({input$outcome_intake})
  dog1 <- reactive({input$select_dog1})
  dog2 <- reactive({input$select_dog2})
  cat1 <- reactive({input$select_cat1})
  cat2 <- reactive({input$select_cat2})
  
  output$p3_dog <- renderPlotly({
    radar(data(), dog1(), dog2(), "Dog", outcome_or_intake())
  })
  
  output$p3_cat <- renderPlotly({
    radar(data(), cat1(), cat2(), "Cat", outcome_or_intake())
  })
  
###############################################################################
  output$myIcon <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = 'www/Icon_4_white.png')
    
    # Generate the PNG
    png(outfile, width = 400, height = 300)
    hist(rnorm(input$obs), main = "Generated in renderImage()")
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
}

