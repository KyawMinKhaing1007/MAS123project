install.packages("pacman")
library(pacman) # Include dplyr for select() function
p_load(dplyr,randtests,ggplot2)
# Prepare dataset
data <- mtcars %>%
  select(mpg) %>%   # Select the mpg column
  mutate(binary_mpg = ifelse(mpg > median(mpg), 1, 0))  # Create binary variable

# Print data
print(head(data))
# Perform Runs Test on binary mpg data
runs_test_result <- runs.test(factor(data$binary_mpg))
print(runs_test_result)
# Add run groups for visualization
data <- data %>%
  mutate(run_group = cumsum(c(1, diff(binary_mpg)) != 0))

# Plot the binary runs
ggplot(data, aes(x = seq_along(binary_mpg), y = binary_mpg, group = run_group, color = as.factor(run_group))) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "Runs Visualization",
    x = "Index",
    y = "Binary MPG (1 = Above Median, 0 = Below Median)",
    color = "Run Group"
  ) +
  theme_minimal()

