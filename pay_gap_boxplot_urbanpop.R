
library(dplyr)
library(tidyverse)
library(ggplot2)
library(hexbin)
library(bslib) # for convenient window arrangement in plotting
library(gridExtra)

data <- read_csv("pay_gap_Europe.csv", show_col_types = FALSE)
dataAvg <- read_csv("pay_gap_Europe.csv", show_col_types = FALSE)
central_europe <- c("France","Luxembourg","Netherlands","Belgium","Germany", "Austria", "Czech Republic", "Hungary", "Poland", "Slovakia", "Slovenia")
northern_europe <- c("Denmark", "Estonia", "Finland", "Iceland", "Latvia", "Lithuania", "Norway", "Sweden")
eastern_europe <- c("Bulgaria", "Croatia", "Romania")
southern_europe <- c("Cyprus", "Greece", "Italy", "Malta", "Portugal", "Spain")

dataAvg <- transform(dataAvg, subgroup = case_when(
  Country %in% central_europe ~ "Central Europe",
  Country %in% northern_europe ~ "Northern Europe",
  Country %in% eastern_europe ~ "Eastern Europe",
  Country %in% southern_europe ~ "Southern Europe",
  TRUE ~ "Other"
))

p1 <- ggplot(dataAvg, aes(x=dataAvg$Average, y=Urban_population)) + geom_point(aes(col=dataAvg$subgroup))
p2 <- ggplot(dataAvg, aes(x=subgroup, y=GDP, fill=subgroup)) + geom_boxplot()
p3 <- ggplot(dataAvg, aes(x=subgroup, y=Average, fill=subgroup)) + geom_boxplot()
grid.arrange(p1, p3, nrow = 1, widths = c(1, 1))
par(mfrow=c(1,1))

