# ----  WORK IN PROGRESS ----
# DON'T TOUCH IT ^^^
# Link : https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/

#charge the libraries
library(ggplot2)
library(tidyverse)
library(maps)
library(rworldmap)
library(dplyr)

#Solution from this video : https://www.youtube.com/watch?v=AgWgPSZ7Gp0

##create a data frame and mixe it with the world df with coordinates
data <- data.frame(countries = pay_gap_Europe$Country, GDP = pay_gap_Europe$GDP)
colnames(data)[1]  <- "region"

mapdata <- map_data("world")
mapdata <- left_join(mapdata, data, by="region")

##filter the NA values

mapdata1 <- mapdata %>% filter(!is.na(mapdata$GDP))
view(mapdata1)

##creation of the map

map1 <- ggplot(mapdata1, aes (x=long, y = lat, group =group))+
  geom_polygon(aes(fill = GDP), color = "black")
map1

map2 <- map1 + scale_fill_gradient(name= "Gender pay gap with GDP", low = "yellow", high = "red", na.value= "grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank())
map2

# #EU Countries
#
# some.eu.countries <- c(
#   "Portugal", "Spain", "France", "Switzerland", "Germany",
#   "Austria", "Belgium", "UK", "Netherlands",
#   "Denmark", "Poland", "Italy",
#   "Croatia", "Slovenia", "Hungary", "Slovakia",
#   "Czech republic","Luxembourg","Greece","Sweden", "Finland","Estonia",
#   "Lithuania","Lettonia", "Ukraine","Romania", "Bulgaria", "Norway(?!:Svalbard)"
# )

# # Other code lines
# countriesISO <- c("FR","UK","BE")
#
# data <- data.frame(countries = pay_gap_Europe$Country, GDP = pay_gap_Europe$GDP)
# map <-  map_data(map = "world") %>% filter(region %in% countriesISO)
# k <- ggplot(data, aes(fill = countries))
#
# k + geom_map(aes(map_id = countries), map = map) +
#   expand_limits(x = map$long, y = map$lat)

#
# # Retrieve the map data
# some.eu.maps <- map_data("world", region = some.eu.countries)
#
# # Compute the centroid as the mean longitude and lattitude
# # Used as label coordinate for country's names
# region.lab.data <- some.eu.maps %>%
#   group_by(region) %>%
#   summarise(long = mean(long), lat = mean(lat))
#
# ggplot(some.eu.maps, aes(x = long, y = lat)) +
#   geom_polygon(aes( group = group, fill = region))+
#   # geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
#   scale_fill_viridis_d()+
#   theme_void()+
#   theme(legend.position = "none")
#
#
