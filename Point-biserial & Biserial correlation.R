# <<<<< ============================================================= >>>>> 
#         Group-1          MAS-19 Kyaw Min Khaing        Group_1
#                               Presentation
#                           Kruskal-Wallis Test
# <<<<< ============================================================= >>>>> 
library(pacman)
p_load(tidyverse,ltm,corrr)
data <- mtcars
# Create a binary variable for high/low mpg based on the median
data <- data %>%
  mutate(mpg_high = if_else(mpg > median(mpg), 1, 0)) %>%
  mutate(hp_binary = if_else(hp > median(hp), 1, 0))  # Dichotomize hp for biserial example
# <<<<< ============================================================= >>>>> 
# Point-Biserial Correlation
point_biserial_corr <- cor(data$mpg_high, data$hp, method = "pearson")
print(paste("Point-Biserial Correlation:", round(point_biserial_corr, 4)))
# <<<<< ============================================================= >>>>> 
# Biserial Correlation
biserial_corr <- biserial.cor(data$hp, data$hp_binary)
print(paste("Biserial Correlation:", round(biserial_corr, 4)))
# <<<<< ============================================================= >>>>> 
# Plot: Relationship between mpg_high and hp
ggplot(data, aes(x = as.factor(mpg_high), y = hp, fill = as.factor(mpg_high))) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", color = "blue", size = 3) +
  labs(
    title = "Point-Biserial Correlation Visualization",
    x = "High MPG (Binary: 0=Low, 1=High)",
    y = "Horsepower (hp)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
