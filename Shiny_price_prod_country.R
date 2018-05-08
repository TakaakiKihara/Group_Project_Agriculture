## This script is to see the relationship between
## price and production of crops from 1961~2016

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(data.table)

library(shiny)

ui <- fluidPage(
  titlePanel(title = "Price and Production",
             windowTitle = "Price and Production"),
  sidebarLayout(
    sidebarPanel(
      helpText("Analyze the relation between Price and Production"),
      selectInput(inputId = "Item",
                  label = "Chooose an Item to display",
                  choices = c("Rice, paddy",
                              "Wheat",
                              "Barley",
                              "Maize",
                              "Soybeans",
                              "Sugar beet",
                              "Sugar cane",
                              "Avocados"),
                  selected = ""),
      selectInput(inputId = "Country",
                  label = "Chooose a country to display",
                  choices = c("United States of America",
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
                              "Thailand"),
                  selected = "")
    ),
    mainPanel(
      textOutput(outputId = "graph_title"),
      plotOutput(outputId = "graph")
    )
  )
)



server <- function(input, output, session) {
  price_prod = ({
    harvest = fread("Production_Crops_E_All_Data.csv")
    price = fread("Prices_E_All_Data.csv")
    harvest = harvest %>%
      select(-ends_with("F")) %>%
      gather(Y1961:Y2016, key = "Year", value = "production")
    
    harvest$Year = str_sub(harvest$Year,2)
    harvest$Year = as.numeric(harvest$Year)
    harvest$Area = as.character(harvest$Area)
    
    harvest <- harvest %>%
      select("Area", "Item","Element","Year","production") %>%
      spread(key = Element, value = production)
    
    names(harvest) <- gsub(" ", "_", names(harvest))
    
    harvest <- harvest %>%
      select(-Feed, -Area_harvested, -Yield) %>%
      filter(!is.na(Production) &
             Year >= 1991 &
             Area %in% c("United States of America",
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
                           "Wheat",
                           "Barley",
                           "Maize",
                           "Soybeans",
                           "Sugar beet",
                           "Sugar cane",
                           "Avocados",
                           "Blueberries",
                           "Quinoa") )
    
    price = price %>%
      select(-ends_with("F")) %>%
      gather(Y1991:Y2016, key = "Year", value = "Price")
    
    price$Year = str_sub(price$Year,2)
    price$Year = as.numeric(price$Year)
    price$Area = as.character(price$Area)
    
    price <- price %>%
      select("Area", "Item","Element","Year","Price") %>%
      spread(key = Element, value = Price)
    
    names(price) <- gsub(" ", "_", names(price))
    
    price <- price %>%
      filter(!is.na(`Producer_Price_(USD/tonne)`) &
               Area %in% c("United States of America",
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
                           "Wheat",
                           "Barley",
                           "Maize",
                           "Soybeans",
                           "Sugar beet",
                           "Sugar cane",
                           "Avocados",
                           "Blueberries",
                           "Quinoa") )
    
    
    inner_join(price, harvest,
               by = c("Area", "Item","Year"))
  })
  
  price_prod_index = reactive({price_prod %>%
      filter(Item == input$Item & Area == input$Country) %>%
      mutate(index = Production/`Producer_Price_(USD/tonne)`) 
  })
  
  multiple = reactive({mean(price_prod_index()$index) * 0.5
  })
  
  output$graph = renderPlot({
    ggplot(price_prod_index(), aes(x = Year)) +
      geom_line(aes(y = Production, colour = "Prod")) +
      geom_line(aes(y = `Producer_Price_(USD/tonne)`*multiple(), colour = "Price"))  +
      scale_y_continuous(sec.axis = sec_axis(~./multiple(), name = "Price [$USD/Tonne]")) +
      ggtitle(paste(input$Item, "in", input$Country))
  })
}

shinyApp(ui, server)


