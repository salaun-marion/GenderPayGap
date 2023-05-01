#charge the library
library(ggplot2)
library(tidyverse)

#diagram for payGap with x = Year and y = GDP
ggplot(data = payGapEurope, aes(x = payGapEurope$Year, y = payGapEurope$GDP)) + 
  geom_smooth( colour='red')+
  xlab("Years") +
  ylab("Gross Domestic Product (GDP)")





