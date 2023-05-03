# ----  WORK IN PROGRESS ----
# DON'T TOUCH IT ^^^
# Link : https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/


#charge the libraries
library(ggplot2)
library(tidyverse)
library(maps)
library(rworldmap)

# #EU Contries
# some.eu.countries <- c(
#   mapRegion="Europe"
# )
# 
# data <- data.frame(countries = payGapEurope$Country, GDP = payGapEurope$GDP)
# map <- map_data("Europe")
# k <- ggplot(data, aes(fill = murder))
#   
# k + geom_map(aes(map_id = state), map = map) 
# k + expand_limits(x = map$long, y = map$lat)




# Some EU Contries
some.eu.countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy", 
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic","Luxembourg"
)

# Retrieve the map data
some.eu.maps <- map_data("world", region = some.eu.countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(some.eu.maps, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  # geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")


