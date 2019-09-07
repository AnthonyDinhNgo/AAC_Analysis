# load the necessary package
library(shiny)
# load ui and server files
source("app_ui.R")
source("app_server.R")

# launch the shiny app
shinyApp(ui = ui, server = server)
