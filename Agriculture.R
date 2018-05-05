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


world_map = map_data("world")
world_map$region = as.factor(world_map$region)

distinct(world_map,region)
distinct(production, Area)
?distinct
?join
production_map = full_join(world_map, production,
                           by = c("region" = "Area"))

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

