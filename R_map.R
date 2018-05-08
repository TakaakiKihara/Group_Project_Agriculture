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
                       "Wheat",
                       "Barley",
                       "Maize",
                       "Soybeans",
                       "Sugar beet",
                       "Sugar cane",
                       "Oilcrops, Oil Equivalent",
                       "Avocados",
                       "Quinoa",
                       "Blueberries") &
           Element == "Production" &
           Year %in% c(1961, seq(1960,2010,10),2016)) 

world_map = map_data("world")
world_map$region[world_map$region == "Brunei"] = "Brunei Darussalam"
world_map$region[world_map$region == "USA"] = "United States of America"
world_map$region[world_map$region == "Vietnam"] = "Viet Nam"

TPP_map = world_map %>%
  filter(region %in% c("United States of America",
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
                       "Thailand"))

production_TPP_map = right_join(world_map, production_TPP,
                                by = c("region" = "Area"))

production_TPP_map = production_TPP_map %>%
  filter(Item == "Blueberries" & Year == 2000)

######## Changge "Item" and "Year" and 
######## clean environment and run all to see the change

ggplot(TPP_map, aes(x = long,
                    y= lat,
                    group = group),
       fill = "white",
       color = "black") +
  geom_polygon() +
  geom_polygon(data = production_TPP_map, 
               aes(x = long,
                   y= lat,
                   group = group,
                   fill = production)) +
  scale_fill_gradient(low = "white",
                      high = "red",
                      na.value = "white") +
  ggtitle(paste("Blueberries in 2000", sep ="/"))