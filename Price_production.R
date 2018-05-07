## This script is to see the relationship between
## price and production of crops from 1961~2016

library(dplyr)
library(tidyr)
library(stringr)

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
  spread(key = Element, value = production) %>%
  select(-Feed)

######## Next step: substitue the code below with column name changes
######## The spreaded columns names have " " space. It can be a problems
harvest <- harvest[,c(1,2,3,5)]

######## Cleaning price data ##########

price = price %>%
  select(-ends_with("F")) %>%
  gather(Y1991:Y2016, key = "Year", value = "Price")

#### Next step: spread the elements 

#### Next step: select the right colomns 



######## Yearly production aggregation #########

yearly_production <- harvest %>%
  filter(!is.na(Production)) %>%
  group_by(Year, Item) %>%
  summarise(total_prod = sum(Production))

levels(yearly_production$Item)

########  Next Step: aggregate the price 

######## compare produciton and price with geom_line


######## make a shiny for the comparison
