### Overlook of Agricultural Production
library(shiny)
library(ggplot2)
library(dplyr)
library(shiny)

Production = 
  

ui <- fluidPage(
  titlePanel(title = "World Agricltural Production",
             windowTitle = "Agri Pro")
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)