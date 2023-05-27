library(tidyverse)
library(ggplot2)

#Let' have a look at the correlations
pay_gap_Europe <- read.csv("pay_gap_Europe.csv")

#create a new column with the country
pay_gap_Europe$Country_factor <- as.factor(pay_gap_Europe$Country)
#give to each country a numeric value
pay_gap_Europe$Country_numeric <- as.numeric(pay_gap_Europe$Country_factor) - 1
pay_gap_Europe

#remove from the past dataset the qualitative values
pay_gap<-subset(pay_gap_Europe,select=-c(Country, Country_factor) )
pay_gap


#see the correlation of pay_gap
pay_gap.corr<-cor(na.omit(pay_gap))
pay_gap.corr

#Make a graph for the correlation
palette=colorRampPalette(c("green", "white", "red")) (20)
heatmap(x=pay_gap.corr,col = palette, symm = TRUE)