library(tidyverse)
library(ggplot2)


#Let' have a look at the correlations
pay_gap_Europe <- read.csv("pay_gap_Europe.csv")

#create a new column with the country
pay_gap_Europe$Country_factor <- as.factor(pay_gap_Europe$Country)
#give to each country a qualitative value
pay_gap_Europe$Country_numeric <- as.numeric(pay_gap_Europe$Country_factor) - 1
pay_gap_Europe

#remove from the past dataset the qualitative values
pay_gap<-subset(pay_gap_Europe,select=-c(Country, Country_factor) )
pay_gap

#cor(na.omit(pay_gap))

#see the correlation of pay_gap
pay_gap.corr<-cor(na.omit(pay_gap))
pay_gap.corr

#Make a graph for the correlation
palette=colorRampPalette(c("green", "white", "red")) (20)
heatmap(x=pay_gap.corr,col = palette, symm = TRUE)

##EZ ERABILI HAU
# Subset the correlation matrix to include only 'Year', 'GDP' and 'Urban_population" 'Country' and "GDP in Information, 'Business'"
#informationSector <- select(pay_gap, Year, GDP, Urban_population, Information, Country_numeric)
#informationSector
#see the correlation of pay_gap
#matrixInf<-cor(na.omit(informationSector))
#matrixInf


#we concentrate on the "Information"  as output for the regression
correlation<-pay_gap.corr[,'Information']
col<-colnames(pay_gap.corr)
inf.corr<-tibble(col, correlation)

ggplot(data=inf.corr) + geom_point(aes(x=col, y=correlation)) + geom_hline(yintercept=0, color="blue", size=2)



#strating with the Manufacturing correlated variable as predictor
lm1 <- lm(Information~Financial,data=pay_gap)

lm1
summary(lm1)

par(mar=c(1, 1, 1, 1)); par(mfrow=c(2,2)); plot(lm1); par(mfrow=c(1,1));

ggplot() + geom_point(data=pay_gap, aes(x=Manufacturing, y=Information)) +#coord_cartesian(ylim=c(0,35))+
  geom_abline(intercept=coef(lm1)[1], slope=coef(lm1)[2], size=2, color="red", alpha=0.5)
##

#Let's do some predictions
idx <- c(0.125, 2.2, 5, 10, 15, 22.75, 30, 37.5, 39.95)
pred1 <- predict(lm1,data.frame(Financial=idx), interval="prediction")
pred2 <- predict(lm1,data.frame(Financial=idx), interval="confidence")

pred.frame <-  tibble(idx,pred1[,"fit"],pred1[,"lwr"],pred1[,"upr"],pred2[,"lwr"],pred2[,"upr"])
colnames(pred.frame) <- c("x","y","y_min","y_max","conf_min","conf_max")

ggplot(data=pay_gap) +  geom_point(data=pay_gap, aes(x=Financial, y=Information)) + 
  geom_abline(intercept=coef(lm1)[1], slope=coef(lm1)[2], size=2, color="grey", alpha=0.5) + 
  geom_smooth(data=pay_gap, aes(x=Financial, y=Information)) +
  geom_point(data=pred.frame, aes(x=x, y=y),color="red", size=2) +
  geom_point(data=pred.frame, aes(x=x, y=y_min),color="red", size=4, alpha=0.5, shape =3) +
  geom_point(data=pred.frame, aes(x=x, y=y_max),color="red", size=4, alpha=0.5, shape =3) +
  geom_point(data=pred.frame, aes(x=x, y=conf_min),color="red", size=4, alpha=0.5, shape =95) +
  geom_point(data=pred.frame, aes(x=x, y=conf_max),color="red", size=4, alpha=0.5, shape =95)

#let's have a look at the variable correlation with GDP, in graphical format
#ggplot(data=inf.corr) + geom_point(aes(x=col, y=correlation)) + geom_hline(yintercept=0, color="blue", size=2)

#second model, including also "Manufacturing" 
lm2 <- lm(Information~Financial+Manufacturing,data=pay_gap)

summary(lm2)

#extending further, including also "Retail.trade"
lm3 <- lm(Information~Financial+Manufacturing+Retail.trade,data=pay_gap)

summary(lm3)

# what about using ALL the avaialble variables as predictor set (BAD IDEA, in general)
lm.tot <- lm(Information~.,data=pay_gap)

summary(lm.tot)

#let's prune off the one not significant in the model 
#  --> WE EXPECT this to be the BEST model possible, from the predictive point of view...
lm.red <- lm(Information~.-GDP-Industry-Accommodation-Human_health-Other-Country_numeric,data=pay_gap)

summary(lm.red)

#we have still non significant variables, so we are going to take off
lm.red2 <- lm(Information~.-GDP-Industry-Accommodation-Human_health-Other-
                Country_numeric-Business-Mining-Retail.trade-Real.estate-Public_administration,data=pay_gap)

summary(lm.red2)

#let's see what happens when we remove other predictors, starting from the less significant ones
lm.red3 <- lm(Information~.-GDP-Industry-Accommodation-Human_health-Other-
                Country_numeric-Business-Mining-Retail.trade-Real.estate-
                Public_administration-Electricity_supply-Water_supply-Education,data=pay_gap)


summary(lm.red3)

#let's compare the models obtained, in a structured way
anova(lm1, lm2, lm3) #lm2 is a good model
anova(lm.red2, lm.red3,lm.red, lm.tot) #lm.red3, and lm.red are good models
#best one lm.red3




