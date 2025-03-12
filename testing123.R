library(tidyverse)
# Create a data frame for blood pressure reduction with 3 medications
data_one_way <- tibble(
  Medication = rep(c("A", "B", "C"), each = 5),
  Reduction = c(10, 12, 11, 13, 14, 15, 16, 14, 17, 18, 13, 15, 14, 16, 17)
)

# Display data
data_one_way

# Perform One-Way ANOVA
anova_one_way <- aov(Reduction ~ Medication, data = data_one_way)
summary(anova_one_way)
# မြန်မာလို significantဖြစ်သလား
# Perform post-hoc Tukey test
tukey_test <- TukeyHSD(anova_one_way)
print(tukey_test)

ggplot(data_one_way, aes(x = Medication, y = Reduction, color = Medication)) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Mean Blood Pressure Reduction with Confidence Intervals", 
       x = "Medication Type", y = "Mean Reduction (mmHg)")
edit_git_config()