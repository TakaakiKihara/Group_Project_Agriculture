## This script is to see the relationship between
## price and production of crops from 1961~2016

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(data.table)

harvest = read.csv("Production_Crops_E_All_Data.csv")
price = read.csv("Prices_E_All_Data.csv")

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
  summarise(avg_price = median(`Producer_Price_(USD/tonne)`))
  
######## compare produciton and price with geom_line

price_prod <- inner_join(yearly_price, yearly_production,
           by = c("Item","Year"))

######## make a shiny for the comparison

price_prod_index = price_prod %>%
  filter(Item == "Wheat") %>%
  mutate(index = total_prod/avg_price) 

multiple = mean(price_prod_index$index) * 0.5

price_prod_index %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y = total_prod, colour = "Prod")) +
  geom_line(aes(y = avg_price*multiple, colour = "Price"))  +
  scale_y_continuous(sec.axis = sec_axis(~./multiple, name = "Price [$USD/Tonne]"))
