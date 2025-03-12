# Load necessary libraries
library(pacman)
p_load(tidyverse,ggplot2)
# Prepare dataset
data <- mtcars %>%
  count(cyl) %>%  # Count the number of cars by cylinder
  rename(observed = n)  # Rename the count column to observed

# Define expected frequencies
# Assume equal distribution for demonstration purposes
data <- data %>%
  mutate(expected = sum(observed) / n())  # Equal expected frequencies for each group

# Perform Chi-Square Goodness-of-Fit Test
chisq_result <- chisq.test(x = data$observed, p = rep(1 / nrow(data), nrow(data)))

# Print the Chi-Square test result
print(chisq_result)

# Add residuals to the dataset
data <- data %>%
  mutate(residuals = chisq_result$stdres)

# Visualize the observed and expected frequencies
ggplot(data, aes(x = as.factor(cyl))) +
  geom_bar(aes(y = observed, fill = "Observed"), stat = "identity", position = "dodge") +
  geom_point(aes(y = expected, color = "Expected"), size = 4) +
  scale_fill_manual(values = c("Observed" = "steelblue")) +
  scale_color_manual(values = c("Expected" = "red")) +
  labs(
    title = "Chi-Square Goodness-of-Fit Test: Cylinders",
    x = "Number of Cylinders",
    y = "Frequency",
    fill = "",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top")
