#charge the libraries
library(ggplot2)
library(tidyverse)
library(maps)
library(rworldmap)

# Some EU Contries
some.eu.countries <- c(
  mapRegion="Europe"
)
  
# Retrieve the map data
some.eu.maps <- map_data("world", region = some.eu.countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

