library(MASS)
library(ISLR)
library(tidyverse)
#Let' have a look at the correlations
pay_gap_Europe <- read.csv("pay_gap_Europe.csv")

#create a new column with the country
pay_gap_Europe$Country_factor <- as.factor(pay_gap_Europe$Country)
#give to each country a qualitative value
pay_gap_Europe$Country_numeric <- as.numeric(pay_gap_Europe$Country_factor) - 1
pay_gap_Europe

#remove from the past dataset the quantitative values
pay_gap<-subset(pay_gap_Europe,select=-c(Country, Country_factor) )
pay_gap

#cor(na.omit(pay_gap))

#see the correlation of pay_gap
#pay_gap.corr<-cor(pay_gap)
#pay_gap.corr

# Subset the correlation matrix to include only 'Year', 'GDP' and 'Urban_population" 'Country' and "GDP in Information, 'Business'"
informationSector <- select(pay_gap, Year, GDP, Urban_population, Information, Country_numeric)
informationSector

#see the correlation of pay_gap
matrixInf<-cor(na.omit(informationSector))
matrixInf

#we concentrate on the "information" sector as output for the regression
correlation<-informationSectorCor[,'Information']
col<-colnames(informationSectorCor)
inf.corr<-tibble(col, correlation)

ggplot(data=inf.corr) + geom_point(aes(x=col, y=correlation)) + geom_hline(yintercept=0, color="blue", size=2)





##in order to do modelling, we have to convert the qualitative values in quantitative
#for that we have used count encoding





pay_gap_Europe.corr <- cor(pay_gap_Europe)
print(pay_gap_Europe)
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = pay_gap_Europe.corr, col = palette, symm = TRUE)

####

library(readr)
# Load example dataset
data <- read.csv("pay_gap_Europe.csv")
print(pay_gap_Europe)
# Calculate row-wise average of all columns except the first two
data$avg <- rowMeans(data[, -c(1, 2, 3)], na.rm = TRUE)

# View updated dataset with new column
print(data)
View(data)

correlation_matrix <- cor(tapply(data$avg, list(data$Year, data$Country), mean))

print(correlation_matrix)

####
# Create new dataset with only specific columns
data <- subset(data, select=c("Country", "Year", "avg"))

# View the new dataset
print(data)

# Calculate average Gender pay gap for each country
avg_GPG <- aggregate(avg ~ Country, data, mean)

print(avg_GPG)


avg_GPG$Country <- as.factor(avg_GPG$Country)

# Check the levels of the factor
levels(avg_GPG$Country)

# Calculate the correlation
cor(as.numeric(avg_GPG$Country), avg_GPG$avg)




# Load necessary packages
library(reshape2)

# Melt data to long format
data_melted <- melt(data, id.vars = c("Country", "Year", "GDP"))

# Cast data to wide format
data_cast <- dcast(data_melted, Country + Year + GDP ~ variable, value.var = "value")

# Calculate correlation matrix
cor_matrix <- cor(data_cast[, 4:ncol(data_cast)], use = "pairwise.complete.obs")

# Create heatmap with ggplot
ggplot(data = data_cast, aes(x = Country, y = Year, fill = `Gender Pay Gap`)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Country", y = "Year", fill = "Gender Pay Gap") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Correlation Matrix of Average Gender Pay Gap by Country")
