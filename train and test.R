
library(readr)
head(data)
show(data)

#create a new column with the country
pay_gap_Europe$Country_factor <- as.factor(pay_gap_Europe$Country)
#give to each country a quantitative value
pay_gap_Europe$Country_numeric <- as.numeric(pay_gap_Europe$Country_factor) - 1
pay_gap_Europe

#remove from the past dataset the qualitative values
pay_gap<-subset(pay_gap_Europe,select=-c(Country, Country_factor) )
pay_gap

variables<-pay_gap[,c("GDP","Industry","Mining","Business","Manufacturing" ,"Electricity_supply",    
                   "Water_supply","Construction","Retail.trade","Transportation" ,"Accommodation","Information",
                   "Financial","Real.estate","Professional_scientific","Administrative","Public_administration",
                   "Education","Human_health","Arts","Other")]
#scale variables
scaled_variables <- as.data.frame(scale(variables))
#create a dataset with the scaled_varibales and the value year of pay_gap
data <- cbind(scaled_variables, Year = pay_gap$Year, Urban_population=pay_gap$Urban_population,Country_numeric=pay_gap$Country_numeric)

data <- na.omit(data)
View(data)

# Set a random seed for reproducibility
set.seed(123)

# Determine the number of rows in the dataset
n <- nrow(data)

# Specify the proportion of data to use for training (e.g., 80% for training)
train_prop <- 0.8

# Calculate the number of rows for training
train_size <- round(train_prop * n)

# Create a vector of indices for random sampling
train_indices <- sample(1:n, train_size)

# Split the data into training and testing sets
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]
lm.red3 <- lm(Information~.-GDP-Industry-Accommodation-Human_health-Other-
                Country_numeric-Business-Mining-Retail.trade-Real.estate-
                Public_administration-Electricity_supply-Water_supply-Education,data=data)
test_predictions <- predict(lm.red3, newdata = test_data)

# Evaluate the model performance
test_actuals <- test_data$Year
mse <- mean((test_predictions - test_actuals)^2)
rmse <- sqrt(mse)
r_squared <- cor(test_predictions, test_data$Information)^2

# Print the evaluation results
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", r_squared, "\n")

########################### Erakutsi lagunei
#split the dataset
set.seed(123)
split <- initial_split(data, prop = 0.7)
train_data <- training(split)
test_data <- testing(split)


##scale variables
scaled_variables
data <- cbind(scaled_variables, pay_gap = Year)
data
pay_gap_complete <- na.omit(data)


set.seed(123)     # Set seed for reproducibility

n <- nrow(pay_gap_complete)
train_indices <- sample(seq_len(n), size = round(0.7 * n), replace = FALSE)
test_indices <- setdiff(seq_len(n), train_indices)

train_data <- lm(Infromation~.-GDP-Industry-Accommodation-Human_health-Other-
                                            Country_numeric-Business-Mining-Retail.trade-Real.estate-
                                            Public_administration-Electricity_supply-Water_supply-Education,data=pay_gap))
test_data <- original_data[test_indices, ]

train_indices <- sample(seq_len(n), size = round(0.7 * n), replace = FALSE)
test_indices <- setdiff(seq_len(n), train_indices)

# Step 3: Split the data into training and testing sets
train_data <- original_data[train_indices, ]
test_data <- original_data[test_indices, ]
sample <- sample(c(TRUE, FALSE), nrow(model), replace=TRUE, prob=c(0.7,0.3))

target_variables<-pay_gap[,c("Year","Urban_population","Manufacturing","Construction","Transportation","Financial","Professional_scientific","Administrative","Arts")]
train_indices <- createDataPartition(pay_gap$target_variables, p = 0.7, list = FALSE)

model <- lm(Information~.-GDP-Industry-Accommodation-Human_health-Other-
              Country_numeric-Business-Mining-Retail.trade-Real.estate-
              Public_administration-Electricity_supply-Water_supply-Education, data = pay_gap)
sample <- sample(c(TRUE, FALSE), nrow(pay_gap), replace=TRUE, prob=c(0.7,0.3))

predictions <- predict(model, newdata = testing_set)
train_indices <- createDataPartition(pay_$target_variable, p = 0.7, list = FALSE)
train_data <- model[sample, ]
test_data <- model[-sample, ]
