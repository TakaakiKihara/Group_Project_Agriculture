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

# The script below is not correct
# production = production %>%
#  spread(key = "Element", value = "production")

ui <- fluidPage(
  titlePanel(title = "World Agricltural Production",
             windowTitle = "Agri Pro")
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

########################## Maps ############################

library(maps)
world_map = map_data("world")

ggplot(world_map,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "blue", fill = "white")

