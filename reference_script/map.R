library(maps)
library(ggplot2)
states_map = map_data("state")
View(states_map)


#create a US map

ggplot(states_map,
       aes(x = long, y = lat)) +
  geom_point()


ggplot(states_map,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "blue", fill = "white")

# Other maps in ggplot2
# world, france, nz, usa, county

### plot the world map

world_map = map_data("world")
ggplot(world_map,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "blue", fill = "white")

world_map = map_data("world")
ggplot(world_map,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "blue", fill = "white") +
  theme_classic()

iceland_map = map_data("world", region = "iceland")

ggplot(iceland_map,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "blue", fill = "white") +
  theme_classic()

North_EU_map = map_data("world", region = c("iceland","Denmark","greenland"))

ggplot(North_EU_map,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "blue", fill = "white") +
  theme_classic()


data = USArrests                        

View(data)

colnames(states_map)
rownames(USArrests)

USArrests$State = rownames(USArrests)

### Join the two tables
library(dplyr)
crime_map = full_join(states_map, USArrests,
                      by = c("region" = "State"))

######This line above will return NA, because of capitals

### Change the States to lower case so that 
### it is similar to the region variable

USArrests$State = tolower(USArrests$State)

ggplot(crime_map,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "blue",
               aes(fill = Murder))

ggplot(crime_map,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "white",
               aes(fill = Murder))+
  scale_fill_gradient(low = "white",
                      high = "Red")+
  theme_void() +
  coord_map("polyconic")

library(mapproj)

### Google Maps

library(ggmap)

# Two options:
 # 1. get_map() ggmap()
 # 2. qmap() function

qmap("University of Southern California",
     zoom = 14)

### Utility Function is in ggmap

geocode("Popovich Hall, LA, CA")

revgeocode(c(-118.283, 34.01883))

qmap("Houston", zoom =14)

?crime

#gglocator() is not working now but you can choose from the map 

###focus on downtown 

dt_crime = filter(crime, 
                  lon >= -95.39681 & lon <= -95.34188,
                  lat >= 29.73631 & lat <= 29.78400)

### Focus on the following 4 types crimes in dt

violent_crime = filter(dt_crime, 
                       offense %in% c("aggravated assault",
                                     "murder",
                                     "rape",
                                      "robbery"))

Houston_map = qmap("Houston", Zoom = 14 , color = "bw")

Houston_map +
  geom_point(data = violent_crime,
             aes(x = lon,
                 y = lat,
                 color = offense))

Houston_map +
  stat_density2d(data = violent_crime,
                 aes(x = lon, y = lat,
                     fill = ..level..,
                     alpha = ..level..),
                 geom = "polygon") +
  facet_wrap(~day, nrow = 2)
