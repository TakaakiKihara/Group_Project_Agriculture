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
      helpText("Analyze the relation between Price and Productionm"),
      selectInput(inputId = "Item",
                  label = "Chooose an Item to display",
                  choices = c("Rice, paddy",
                              "Wheat",
                              "Barley",
                              "Maize",
                              "Soybeans",
                              "Sugar beet",
                              "Sugar cane",
                              "Avocados",),
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
    filter(!is.na(Production),
           Year >= 1991)
  
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
    filter(!is.na(`Producer_Price_(USD/tonne)`))
  
  
  yearly_production <- harvest %>%
    group_by(Year, Item) %>%
    summarise(total_prod = sum(Production))
  
  
  yearly_price <- price %>%
    group_by(Year, Item) %>%
    summarise(avg_price = median(`Producer_Price_(USD/tonne)`))
  
  inner_join(yearly_price, yearly_production,
                           by = c("Item","Year"))
  })
  
  price_prod_index = reactive({price_prod %>%
    filter(Item == input$Item) %>%
    mutate(index = total_prod/avg_price) 
  })
  
  multiple = reactive({mean(price_prod_index()$index) * 0.5
  })
  
  output$graph = renderPlot({
      ggplot(price_prod_index(), aes(x = Year)) +
      geom_line(aes(y = total_prod, colour = "Prod")) +
      geom_line(aes(y = avg_price*multiple(), colour = "Price"))  +
      scale_y_continuous(sec.axis = sec_axis(~./multiple(), name = "Price [$USD/Tonne]")) +
      ggtitle(paste(input$Item))
  })
}

shinyApp(ui, server)


