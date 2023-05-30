library(readr)
library(ggplot2)
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



# Evaluate the model performance of lm.red3
test_actuals <- test_data$Information
mse <- mean((test_predictions - test_actuals)^2)
rmse <- sqrt(mse)
r_squared <- cor(test_predictions, test_data$Information)^2

### --- WORKS FOR MARION
#Text to integrate into shiny app (as Residual plot and scatter plot below)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Residual Plot
residuals <- test_actuals - test_predictions
residual_plot <- ggplot() +
  geom_point(aes(x = test_predictions, y = residuals), color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Predicted Values", y = "Residuals") +
  ggtitle("Residual Plot")
print(residual_plot)

# Scatter Plot
scatter_plot <- ggplot() +
  geom_point(aes(x = test_predictions, y = test_actuals), color = "green") +
  geom_abline(intercept = 0, slope = 1, color = "blue", linetype = "dashed") +
  labs(x = "Predicted Values", y = "Actual Values") +
  ggtitle("Scatter Plot")
print(scatter_plot)

########################### 