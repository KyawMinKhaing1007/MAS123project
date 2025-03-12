# Load necessary libraries
library("pacman")
p_load(tidyverse)
# Use the Titanic dataset
data <- as.data.frame(Titanic)
# Select relevant columns and summarize the data
titanic_data <- data %>%
  filter(Class %in% c("1st", "3rd")) %>% # Filter for specific classes
  group_by(Survived, Class) %>%
  summarize(Count = sum(Freq), .groups = "drop") %>%
  pivot_wider(names_from = Class, values_from = Count)

# Print the contingency table
print(titanic_data)

# Convert to a contingency table for Fisher's Exact Test
contingency_table <- titanic_data %>%
  dplyr::select(-Survived) %>%
  as.matrix()

# Perform Fisher's Exact Test
fisher_test_result <- fisher.test(contingency_table)

# Print the test results
print(fisher_test_result)

# Prepare data for ggplot visualization
titanic_plot_data <- data %>%
  filter(Class %in% c("1st", "3rd")) %>%
  group_by(Survived, Class) %>%
  summarize(Count = sum(Freq), .groups = "drop")

# Create a bar plot
ggplot(titanic_plot_data, aes(x = Class, y = Count, fill = Survived)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Survival Counts by Class",
    subtitle = paste0("Fisher's Exact Test p-value: ", round(fisher_test_result$p.value, 4)),
    x = "Passenger Class",
    y = "Count",
    fill = "Survived"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
