#charge the library
library(ggplot2)
library(tidyverse)

#diagram for payGap with x = Year and y = GDP
ggplot(data = pay_gap_Europe, aes(x = pay_gap_Europe$Year, y = pay_gap_Europe$GDP)) + 
  geom_smooth( colour='red')+
  xlab("Years") +
  ylab("Gross Domestic Product (GDP)")






