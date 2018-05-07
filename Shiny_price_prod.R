# Shiny Price_production

library(data.table)
library(shiny)
library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)
library(stringr)


#### The UI ####

ui <- fluidPage(
  titlePanel(title = "Worldside Crop Price vs. Production Trends",
             windowTitle = "Price X Production"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select crop to start with"),
      selectInput(inputId = "Item",
                  label = "Choose an Item",
                  choices = c("Pineapples",
                              "Apples",
                              "Avocados",
                              "Wheat"),
                  selected = "")
  ),
  mainPanel(
    textOutput(outputId = "chart_title"),
    plotOutput(outputId = "trend")
  )
  
)
)

server <- function(input, output, session) {
  p_p_chart = ({
    harvest = fread("Production_Crops_E_All_Data.csv")
    price = fread("Prices_E_All_Data.csv")
    
    ######data cleaning for harvest#######
    
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
    
    ######## Cleaning price data ##########
    
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
    
    ######## Yearly production aggregation #########
    
    yearly_production <- harvest %>%
      group_by(Year, Item) %>%
      summarise(total_prod = sum(Production))
    
    levels(yearly_production$Item)
    
    ########  Next Step: aggregate the price 
    
    yearly_price <- price %>%
      group_by(Year, Item) %>%
      summarise(avg_price = mean(`Producer_Price_(USD/tonne)`))
    
    ######## compare produciton and price with geom_line
    
    price_prod <- inner_join(yearly_price, yearly_production,
                             by = c("Item","Year"))
    price_prod$Item <- as.factor(price_prod$Item)
  })
  
  reactive_price_prod = reactive({
    price_prod %>%
      filter(Item == input$Item)
  })
  
  output$trend = renderPlot({
    ggplot(reactive_price_prod, aes(x = Year)) +
      geom_line(aes(y = total_prod, colour = "Prod")) +
      geom_line(aes(y = avg_price*100000, colour = "Price"))  +
      scale_y_continuous(sec.axis = sec_axis(~./100000, name = "Price [$USD/Tonne]"))
  })
}

shinyApp(ui, server)

