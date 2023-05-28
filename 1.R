update.packages()
##library(MASS)
##library(ISLR)
library(tidyverse)
library(ggplot2)


#Let' have a look at the correlations
pay_gap_Europe <- read.csv('pay_gap_Europe.csv')

getwd()#create a new column with the country
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


#we concentrate on the "GDP"  as output for the regression
correlation<-pay_gap.corr[,'GDP']
col<-colnames(pay_gap.corr)
gdp.corr<-tibble(col, correlation)

ggplot(data=gdp.corr) + geom_point(aes(x=col, y=correlation)) + geom_hline(yintercept=0, color="blue", size=2)



#strating with the Manufacturing correlated variable as predictor
lm1 <- lm(GDP~Manufacturing,data=pay_gap)

lm1
summary(lm1)

par(mar=c(1, 1, 1, 1)); par(mfrow=c(2,2)); plot(lm1); par(mfrow=c(1,1));

ggplot() + geom_point(data=pay_gap, aes(x=Manufacturing, y=GDP)) +#coord_cartesian(ylim=c(0,35))+
  geom_abline(intercept=coef(lm1)[1], slope=coef(lm1)[2], size=2, color="red", alpha=0.5)
    ##

#Let's do some predictions
idx <- c(0.125, 2.2, 5, 10, 15, 22.75, 30, 37.5, 39.95)
pred1 <- predict(lm1,data.frame(Manufacturing=idx), interval="prediction")
pred2 <- predict(lm1,data.frame(Manufacturing=idx), interval="confidence")

pred.frame <-  tibble(idx,pred1[,"fit"],pred1[,"lwr"],pred1[,"upr"],pred2[,"lwr"],pred2[,"upr"])
colnames(pred.frame) <- c("x","y","y_min","y_max","conf_min","conf_max")

ggplot(data=pay_gap) +  geom_point(data=pay_gap, aes(x=Manufacturing, y=GDP)) + 
  geom_abline(intercept=coef(lm1)[1], slope=coef(lm1)[2], size=2, color="grey", alpha=0.5) + 
  geom_smooth(data=pay_gap, aes(x=Financial, y=Information)) +
  geom_point(data=pred.frame, aes(x=x, y=y),color="red", size=2) +
  geom_point(data=pred.frame, aes(x=x, y=y_min),color="red", size=4, alpha=0.5, shape =3) +
  geom_point(data=pred.frame, aes(x=x, y=y_max),color="red", size=4, alpha=0.5, shape =3) +
  geom_point(data=pred.frame, aes(x=x, y=conf_min),color="red", size=4, alpha=0.5, shape =95) +
  geom_point(data=pred.frame, aes(x=x, y=conf_max),color="red", size=4, alpha=0.5, shape =95)

#let's have a look at the variable correlation with GDP, in graphical format
#ggplot(data=inf.corr) + geom_point(aes(x=col, y=correlation)) + geom_hline(yintercept=0, color="blue", size=2)

#second model, including also "Urban_Population" 
lm2 <- lm(GDP~Manufacturing+Urban_population,data=pay_gap)

summary(lm2)

#extending further, including also "Information"
lm3 <- lm(GDP~Manufacturing+Urban_population+Information,data=pay_gap)

summary(lm3)

# what about using ALL the avaialble variables as predictor set (BAD IDEA, in general)
lm.tot <- lm(GDP~.,data=pay_gap)

summary(lm.tot)

#let's prune off the one not significant in the model 
#  --> WE EXPECT this to be the BEST model possible, from the predictive point of view...
lm.red <- lm(GDP~.-Year-Mining-Mining-Electricity_supply-Construction-Information-Financial-
             Real.estate-Professional_scientific-Administrative-Public_administration,data=pay_gap)

summary(lm.red)

#we have still non significant variables, so we are going to take off
lm.red2 <- lm(GDP~.-Year-Mining-Mining-Electricity_supply-Construction-Information-Financial-
                Real.estate-Professional_scientific-Administrative-Public_administration-
                Education-Arts,data=pay_gap)

summary(lm.red2)

#let's see what happens when we remove other predictors, starting from the less significant ones
lm.red3 <- lm(GDP~.-Year-Mining-Mining-Electricity_supply-Construction-Information-Financial-
                Real.estate-Professional_scientific-Administrative-Public_administration-
                Education-Arts-Urban_population-Retail.trade,data=pay_gap)
                

summary(lm.red3)

#let's compare the models obtained, in a structured way
anova(lm1, lm2) #lm2 is a good model
anova(lm.red2,lm.red3, lm.red, lm.tot) #lm.red3, lm.red and lm.tot are good models
                                      #best one lm.red3


# Making predictions
gender_pay_europe$pred_train <- predict(lm.red2)

# Plot predicted values against actual values
ggplot(gender_pay_europe, aes(x = gdp, y = pay_gap_Europe)) +
  geom_point() +
  geom_line(aes(y = pred_train), color = "red")



##############   Logistic   ##########################
library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(hexbin)
library(bslib) # for convenient window arrangement in plotting
library(gridExtra)


data <- read_csv("pay_gap_Europe.csv", show_col_types = FALSE)
dataAvg <- read_csv("pay_gap_EuropeJenny.csv", show_col_types = FALSE)
tidyData3<-dataAvg  %>% filter(Country %in% c('France', 'Spain')) 
view(tidyData3)
  
colnames(data) <- c("region","year","GDP","Urban population","Industry","Business","Mining", 
                    "Manufacturing","Electricity supply","Water supply","Construction",
                    "Retail","Transportation","Accommodation","Information",
                    "Financial","Real estate","Science",
                    "Administrative","Public administration","Education",
                    "Health","Arts","Other")
tidyData <- data %>%
  pivot_longer(cols = -c("region","year","GDP", "Urban population"), names_to = 'Domain', values_to = 'GPG')

tidyData2 <- tidyData %>%
  mutate(JobSectors = case_when(Domain %in% c('Retail') ~ 'Trade and commerce',
                                Domain %in% c('Manufacturing') ~ 'Manufacturing and production',
                                Domain %in% c('Electricty supply', 'Water supply','Mining', 'Construction') ~ 'Primary Industry and Infrastructure',
                                Domain %in% c("Business","Transportation","Accommodation","Information",
                                              "Financial","Real estate","Science",
                                              "Administrative") ~ 'Service and information',
                                
                                is.na(Domain) ~ 'Public sector and social services',
                                TRUE ~ 'Others'
  )) %>%
  ##filter(region %in% c('France', 'Spain')) %>%
  group_by(JobSectors, year, `Urban population`, region,GDP) %>%
  summarize(GPG=mean(GPG,na.rm = TRUE))
view(tidyData2)

# Display the distributions of Year and Country
p1 <- ggplot(tidyData2 %>% filter(region == 'France'), aes(x = GPG)) + 
  geom_histogram(color = "black", fill = "white")+
scale_x_continuous(limits = c(min(tidyData2$GPG), max(tidyData2$GPG)))
p2 <- ggplot(tidyData2 %>% filter(region == 'Spain'), aes(x = GPG)) + 
  geom_histogram(color = "black", fill = "white")

grid.arrange(p1, p2, nrow = 1)
par(mfrow=c(1,1))

#HAU ITEAK EZ DAUKE ZENTZU ASKO
# Display the classes in the Default data ()
p1 <- ggplot(tidyData3, aes(x=Average, y=`Urban_population`)) + geom_point(aes(col=Country))
p2 <- ggplot(tidyData3, aes(x=Country, y=`Urban_population`, fill=Country)) + geom_boxplot()
p3 <- ggplot(tidyData3, aes(x=Country, y=Average, fill=Country)) + geom_boxplot()
grid.arrange(p1, p2, p3, nrow = 1, widths = c(2, 1, 1))
par(mfrow=c(1,1))

p1 <- ggplot(tidyData2, aes(x=GPG, y=`Urban population`)) + geom_point(aes(col=JobSectors))
p2 <- ggplot(tidyData2, aes(x=JobSectors, y=GDP, fill=JobSectors)) + geom_boxplot()
p3 <- ggplot(tidyData2, aes(x=JobSectors, y=Average, fill=JobSectors)) + geom_boxplot()
grid.arrange(p1, p2, p3, nrow = 1, widths = c(2, 1, 1))
par(mfrow=c(1,1))




# Create training and test data
set.seed(213)
indices <- sample(1:10000, 250) # select 250 random samples
test.data <- pay_gap_Europe[indices,]
table(test.data$default)
training.data <- pay_gap_Europe[-indices,]
table(training.data$GDP)
p1 <- ggplot() + geom_point(data = training.data, aes(x=GDP, y=Urban_population), color='steelblue3') + 
  geom_point(data = test.data, aes(x=GDP, y=Urban_population), color='darkred', size=4) 
p2 <- ggplot() + geom_point(data = training.data, aes(x=GDP, y=Country), color='steelblue3') + 
  geom_point(data = test.data, aes(x=GDP, y=Country), color='darkred', size=4) 
grid.arrange(p1, p2, nrow = 1)

###EZ DET ULERTZEN
glm1 <- glm(Country~Information, data=training.data, family = gaussian())
summary(glm1)

# Making predictions

# "Predicting" the TRAINING data 
pred3 = predict(glm1) # No data set is supplied to the predict() function: the probabilities are computed 
# for the training data that was used to fit the logistic regression model. 
# --> Notice: Without the type option specified in predict we get the linear predictor scale (see next plot)
pred3.df <- data.frame(balance=training.data$balance,prediction=pred3,default=training.data$default) 
# make it a data frame for plotting purposes
ggplot() + geom_point(data = pred3.df, aes(x=balance, y=prediction, col=default)) + 
  geom_hline(yintercept = 0) + geom_hline(yintercept = 1) + ylim(-15,2) # Plot. 

pred.train.probs = predict(glm1, type = "response") 
# With type = "response", we get the response variable scale, i.e., the probabilities.
pred.train.probs.df <- data.frame(balance=training.data$balance,pred.train.probs=pred.train.probs,default=training.data$default) 
# make it a data frame for plotting purposes
ggplot() + geom_point(data = pred.train.probs.df, aes(x=balance, y=pred.train.probs, col=default)) + 
  geom_hline(yintercept = 0) + geom_hline(yintercept = 1) # Plot. 

# Predicting the TEST data PROBABILITIES
pred.test.probs <- predict(glm1, test.data, type = "response")
pred.test.probs.df <- data.frame(balance=test.data$balance,pred.test.probs=pred.test.probs, default=test.data$default)
# make it a data frame for plotting purposes
ggplot() + geom_point(data = pred.test.probs.df, aes(x=balance, y=pred.test.probs, col=default), size=5) + 
  geom_hline(yintercept = 0) + geom_hline(yintercept = 1) + geom_hline(yintercept = 0.5, linetype="dashed") + ylim(0,1)

# Predicting the TEST data CLASSES
pred.test.classes <- rep("No",nrow(test.data)) 
# In order to predict the classes, we must convert the predicted into class labels, Yes or No. We start by converting all to No.
pred.test.classes[pred.test.probs > 0.5] = "Yes"  
# Now we set those to Yes whose proobability is greater than 0.5.
pred.test.classes.df <- data.frame(balance=test.data$balance,pred.test.classes=pred.test.classes)
# make it a data frame for plotting purposes
ggplot() + geom_point(data = pred.test.classes.df, aes(x=balance, y=pred.test.classes, col=test.data$default), size=5)
