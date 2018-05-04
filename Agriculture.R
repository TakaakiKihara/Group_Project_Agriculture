### Overlook of Agricultural Production
library(shiny)
library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)
library(stringr)

production = read.csv("Production_Crops_E_All_Data.csv") 


production = production %>%
  select(-ends_with("F")) %>%
  gather(Y1961:Y2016, key = "Year", value = "production") 

production$Year = str_sub(production$Year,2)

production = production %>%
  spread(key = "Element", value = "production")




ui <- fluidPage(
  titlePanel(title = "World Agricltural Production",
             windowTitle = "Agri Pro"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create agricultural production map"),
      selectInput(inputId = "selected_var",
                  label = "Chooose a variable to display",
                  choices = 
                  selected = "Percent Latino")
    ),
    mainPanel(
      textOutput(outputId = "map_title"),
      plotOutput(outputId = "map")
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

