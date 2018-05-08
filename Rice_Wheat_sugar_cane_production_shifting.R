#### Rice production shifting ####

library(data.table)
library(shiny)
library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)
library(stringr)


production = fread("Production_Crops_E_All_Data.csv") 

production = production %>%
  select(-ends_with("F")) %>%
  gather(Y1961:Y2016, key = "Year", value = "production") 

production$Year = str_sub(production$Year,2)
production$Year = as.numeric(production$Year)
production$Area = as.character(production$Area)
production$production = as.numeric(production$production)


################## Rice ######################
production_TPP_rice = production %>%
  filter(Area %in% c("Japan",
                     "Viet Nam",
                     "Thailand") &
           Item == "Rice, paddy" &
           Element == "Production") 

ggplot(production_TPP_rice, aes(x = Year))+
  geom_line(aes(y = production, colour = Area))

################## Wheat ####################
production_TPP_wheat = production %>%
  filter(Area %in% c("United States of America",
                     "Canada",
                     "Australia") &
           Item == "Wheat" &
           Element == "Production")

ggplot(production_TPP_wheat, aes(x = Year))+
  geom_line(aes(y = production, colour = Area))


###################  Sugar Cane ####################
production_TPP_sugar_cane = production %>%
  filter(Area %in% c("United States of America",
                     "Mexico",
                     "Thailand",
                     "Australia",
                     "Peru") &
           Item == "Sugar cane" &
           Element == "Production")

ggplot(production_TPP_sugar_cane, aes(x = Year))+
  geom_line(aes(y = production, colour = Area))