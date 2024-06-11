# Load necessary libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(42)

# Read the data from CSV file
data <- read.csv("shark_attacks_and_icecream.csv")

# Ensure the 'Month' column is of type character
data$Month <- as.character(data$Month)

# Convert Month to a factor with ordered levels to maintain the chronological order in the plot
data$Month <- factor(data$Month, levels = unique(data$Month))

# Display the first few rows of the dataset
print(head(data))

# Plot the data
ggplot(data, aes(x = Month)) +
  geom_line(aes(y = IceCreamSales, color = "Ice Cream Sales"), size = 1) +
  geom_point(aes(y = IceCreamSales, color = "Ice Cream Sales")) +
  geom_line(aes(y = SharkAttacks * 10, color = "Shark Attacks"), size = 1, linetype = "dashed") +
  geom_point(aes(y = SharkAttacks * 10, color = "Shark Attacks")) +
  scale_y_continuous(
    name = "Ice Cream Sales",
    sec.axis = sec_axis(~ . / 10, name = "Shark Attacks")
  ) +
  labs(
    title = "Ice Cream Sales vs. Shark Attacks (2020-2021)",
    x = "Month"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  ) +
  scale_color_manual(values = c("Ice Cream Sales" = "blue", "Shark Attacks" = "red"))

# Print the full dataset
print(data)

# Correlation analysis
correlation <- cor(data$IceCreamSales, data$SharkAttacks)
print(paste("Correlation between Ice Cream Sales and Shark Attacks: ", round(correlation, 2)))

# Scatter plot with regression line
ggplot(data, aes(x = IceCreamSales, y = SharkAttacks)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Scatter Plot of Ice Cream Sales vs. Shark Attacks",
    x = "Ice Cream Sales",
    y = "Shark Attacks"
  ) +
  theme_minimal()
