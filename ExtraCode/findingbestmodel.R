library(dplyr)
library(tidyverse)

data <- read_csv("pay_gap_Europe.csv", show_col_types = FALSE)

#create an average column
data$Average <- rowMeans(data[,4:24])

#MODELING
#We have created different models to see which can suit the best to predict the Gender Pay Gap in the Information sector
#starting with the Financial sector, the most correlated variable 
lm1 <- lm(Information~Financial,data=pay_gap)

lm1
summary(lm1)

#second model, including also "Manufacturing" 
lm2 <- lm(Information~Financial+Manufacturing,data=pay_gap)

summary(lm2)

#extending further, including also "Retail trade"
lm3 <- lm(Information~Financial+Manufacturing+Retail,data=pay_gap)

summary(lm3)

# what about using ALL the avaialble variables as predictor set (BAD IDEA, in general)
lm.tot <- lm(Information~.,data=pay_gap)

summary(lm.tot)

#let's prune off the one not significant in the model 
#  --> WE EXPECT this to be the BEST model possible, from the predictive point of view...
lm.red <- lm(Information~.-GDP-Industry-Accommodation-Health-Other-Country_numeric,data=pay_gap)

summary(lm.red)

#we have still non significant variables, so we are going to take off
lm.red2 <- lm(Information~.-GDP-Industry-Accommodation-Health-Other-
                Country_numeric-Business-Mining-Retail-RealEstate-Administrative,data=pay_gap)

summary(lm.red2)

#let's see what happens when we remove other predictors, starting from the less significant ones
lm.red3 <- lm(Information~.-GDP-Industry-Accommodation-Health-Other-
                Country_numeric-Business-Mining-Retail-RealEstate-
                Administrative-ElectricitySupply-WaterSupply-Education,data=pay_gap)


summary(lm.red3)

#let's compare the models obtained, in a structured way
anova(lm1, lm2, lm3) #lm2 is a good model
anova(lm.red2, lm.red3,lm.red, lm.tot) #lm.red3, and lm.red are good models
#We can conclude that lm.red3 is the best model
