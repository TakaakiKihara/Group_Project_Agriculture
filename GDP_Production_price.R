library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)


#### Load the GDP data ####

GDP <- read_csv("API_NY.GDP.MKTP.CD_DS2_en_csv_v2.csv", 
                trim_ws = FALSE, skip = 4)

#### Clean the GDP data ####

names(GDP) <- gsub(" ","_", names(GDP))

GDP <- GDP[,c(1:62)]


GDP <- GDP %>%
  gather('1960':'2017', key = "Year", value = "GDP") %>%
  select(Country_Name, Indicator_Name, Year, GDP)

colnames(GDP)
