### Overlook of Agricultural Production
library(data.table)
library(shiny)
library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)
library(stringr)
library(mapproj)


production = fread("Production_Crops_E_All_Data.csv") 

production = production %>%
  select(-ends_with("F")) %>%
  gather(Y1961:Y2016, key = "Year", value = "production") 

production$Year = str_sub(production$Year,2)
production$Year = as.numeric(production$Year)
production$Area = as.character(production$Area)

production_TPP = production %>%
  filter(Area %in% c("United States of America",
                   "Japan",
                   "Canada",
                   "Mexico",
                   "Peru",
                   "Chile",
                   "New Zealand",
                   "Australia",
                   "Brunei Darussalam",
                   "Singapore",
                   "Malaysia",
                   "Viet Nam",
                   "Thailand") &
           Item %in% c("Rice, paddy",
                       "Cereals (Rice Milled Eqv)",
                       "Wheat",
                       "Barley",
                       "Maize",
                       "Soybeans",
                       "Sugar beet",
                       "Sugar cane",
                       "Oilcrops, Oil Equivalent") &
           Element == "Production" &
           Year %in% c(1961, seq(1960,2010,10),2016)) 

production_NAFTA = production %>%
  filter(Area %in% c("United States of America",
                     "Canada",
                     "Mexico"))

world_map = map_data("world")
world_map$region[world_map$region == "Brunei"] = "Brunei Darussalam"
world_map$region[world_map$region == "USA"] = "United States of America"
world_map$region[world_map$region == "Vietnam"] = "Viet Nam"

production_TPP_map = right_join(world_map, production_TPP,
                               by = c("region" = "Area"))


production_NAFTA_map = full_join(world_map, production_NAFTA,
                                 by = c("region" = "Area"))



production_TPP_map %>%
  filter(Item == "Wheat" & Year == 2010) %>%
  ggplot(aes(x = long,
           y= lat,
           group = group,
           fill = production)) +
  geom_polygon() +
  scale_fill_gradient(low = "white",
                      high = "red") 


ui <- fluidPage(
  titlePanel(title = "TPP countires Agricltural Production",
             windowTitle = "TPP"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create agricultural production map"),
      selectInput(inputId = "Item",
                  label = "Chooose an Item to display",
                  choices = c("Rice, paddy",
                              "Cereals (Rice Milled Eqv)",
                              "Wheat",
                              "Barley",
                              "Maize",
                              "Soybeans",
                              "Sugar beet",
                              "Sugar cane",
                              "Oilcrops, Oil Equivalent"),
                  selected = ""),
      selectInput(inputId = "Year",
                  label = "Chooose a Year to display",
                  choices = c(1961, seq(1960,2010,10),2016),
                  selected = "")
    ),
    mainPanel(
      textOutput(outputId = "map_title"),
      plotOutput(outputId = "map")
    )
  )
)



server <- function(input, output, session) {
  output$map = renderPlot({
    production_TPP_map = ({
      production = fread("Production_Crops_E_All_Data.csv") 
      
      production = production %>%
        select(-ends_with("F")) %>%
        gather(Y1961:Y2016, key = "Year", value = "production") 
      
      production$Year = str_sub(production$Year,2)
      production$Year = as.numeric(production$Year)
      production$Area = as.character(production$Area)
      
      production_TPP = production %>%
        filter(Area %in% c("United States of America",
                           "Japan",
                           "Canada",
                           "Mexico",
                           "Peru",
                           "Chile",
                           "New Zealand",
                           "Australia",
                           "Brunei Darussalam",
                           "Singapore",
                           "Malaysia",
                           "Viet Nam",
                           "Thailand") &
                 Item %in% c("Rice, paddy",
                             "Cereals (Rice Milled Eqv)",
                             "Wheat",
                             "Barley",
                             "Maize",
                             "Soybeans",
                             "Sugar beet",
                             "Sugar cane",
                             "Oilcrops, Oil Equivalent") &
                 Element == "Production" &
                 Year %in% c(1961, seq(1960,2010,10),2016)) 
      
      world_map = map_data("world")
      world_map$region[world_map$region == "Brunei"] = "Brunei Darussalam"
      world_map$region[world_map$region == "USA"] = "United States of America"
      world_map$region[world_map$region == "Vietnam"] = "Viet Nam"
      
      right_join(world_map, production_TPP,
                 by = c("region" = "Area"))

    })
    
    production_TPP_map %>%
      filter(Item == input$Item & Year == input$Year) %>%
    ggplot(aes(x = long,
               y= lat,
               group = group,
               fill = production)) +
      geom_polygon() +
      scale_fill_gradient(low = "white",
                          high = "red")
    })
}

shinyApp(ui, server)

