### Overlook of Agricultural Production
library(data.table)
library(shiny)
library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)
library(stringr)
library(mapproj)


#####NAFTA
ui <- fluidPage(
  titlePanel(title = "NAFTA countires Agricltural Production",
             windowTitle = "NAFTA"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create agricultural production map"),
      selectInput(inputId = "Item",
                  label = "Chooose an Item to display",
                  choices = c("Rice, paddy",
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
  production_NAFTA_map = ({
    production = fread("Production_Crops_E_All_Data.csv") 
    
    production = production %>%
      select(-ends_with("F")) %>%
      gather(Y1961:Y2016, key = "Year", value = "production") 
    
    production$Year = str_sub(production$Year,2)
    production$Year = as.numeric(production$Year)
    production$Area = as.character(production$Area)
    
    production_TPP = production %>%
      filter(Area %in% c("United States of America",
                         "Canada",
                         "Mexico") &
               Item %in% c("Rice, paddy",
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
    
    NAFTA_map = world_map %>%
      filter(region %in% c("United States of America",
                           "Canada",
                           "Mexico"))
    
    right_join(world_map, production_TPP,
               by = c("region" = "Area"))
  })
  
  map_item = reactive({
    production_NAFTA_map %>%
      filter(Item == input$Item)
  })
  map_item_Year = reactive({
    map_item() %>%
      filter(Year == input$Year)
  })
  
  output$map = renderPlot({
    ggplot(NAFTA_map, aes(x = long,
                          y= lat,
                          group = group),
           fill = "white",
           color = "black") +
      geom_polygon() +
      geom_polygon(data = map_item_Year(), 
                   aes(x = long,
                       y= lat,
                       group = group,
                       fill = production)) +
      scale_fill_gradient(low = "white",
                          high = "red",
                          na.value = "white") +
      ggtitle(paste(input$Item, input$Year, sep ="/"))
  })
}

shinyApp(ui, server)

