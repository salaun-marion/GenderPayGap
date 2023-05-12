library(readr)
library(ggplot2)
library(tidyr)

# Read the data from CSV
pay_gap_Europe <- read_csv("./pay_gap_Europe.csv", show_col_types = FALSE)

# Select the relevant variables for the heatmap
selected_vars <- c("Industry", "Business", "Mining", "Manufacturing", "Electricity_supply",
                   "Water_supply", "Construction", "Retail trade", "Transportation", "Accommodation",
                   "Information", "Financial", "Real estate", "Professional_scientific",
                   "Administrative", "Public_administration", "Education", "Human_health",
                   "Arts", "Other")

data_subset <- pay_gap_Europe %>% select(all_of(selected_vars))

# Calculate the correlation matrix
cor_matrix <- cor(data_subset)

# Convert correlation matrix to long format
data_long <- as.data.frame(cor_matrix) %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "value")

# Plot the correlation matrix as a heatmap
ggplot(data = data_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
