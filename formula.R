
#CHAPTER-4 Page No.100/No.2StemAndLeafMeanMaxMin----
rates <- c()
stem(rates)
Q_a_less_than_9 <- sum(rates < )
Q_a_less_than_9
Q_b_rates_10_to_11 <- rates[rates >=  & rates < ]
Q_b_rates_10_to_11
Q_b_median_rate <- median(rates)
Q_b_median_rate
Q_d_max_rate <- max(rates)
Q_d_min_rate <- min(rates)
Q_d_max_rate
Q_d_min_rate
#CHAPTER-4 Page No.101/No.3MaxMin----
observations <- c()
total_observations <- sum(observations)
total_observations
min_value <- min(observations)
max_value <- max(observations)
dot_counts <- c()
mode_value <- which.max(dot_counts)
cat("Total number of observations:", total_observations, "\n")
cat("Minimum value:", min_value, "\n")
cat("Maximum value:", max_value, "\n")
cat("Observations tend to cluster around value:", mode_value, "\n")
#CHAPTER-4 Page No.101/No.7MeanMediumRowSum----
units <- c()
units
Q_a_num_days <- length(units)
Q_a_num_days
Q_b_first_class <- sum(units >=  & units <)
Q_b_first_class
Q_c_min_value <- min(units)
Q_c_max_value <- max(units)
Q_d_min_value
Q_d_max_value
Q_d_fourth_row <- units[units >=  & units < ]
Q_d_fourth_row
Q_e_second_row <- units[units >=  & units < ]
Q_e_second_row
Q_f_less_than_70 <- sum(units < )
Q_f_less_than_70
Q_g_eighty_or_more <- sum(units >= )
Q_g_eighty_or_more
Q_h_median_value <- median(units)
Q_h_median_value
Q_i_between_60_and_89 <- sum(units >=  & units <= )
Q_i_between_60_and_89
#CHAPTER-4 Page No.102/No.8RowSum----
prescriptions <- c()
prescriptions
Q_a_num_days <- length(prescriptions)
Q_a_num_days
Q_b_last_class <- sum(prescriptions >=  & prescriptions < )
Q_b_last_class
Q_c_min_value <- min(prescriptions)
Q_c_max_value <- max(prescriptions)
Q_c_min_value
Q_c_max_value
Q_d_fourth_row <- prescriptions[prescriptions >=  & prescriptions < ]
Q_d_fourth_row
Q_e_next_to_last_row <- prescriptions[prescriptions >=  & prescriptions < ]
Q_e_next_to_last_row
Q_f_less_than_160 <- sum(prescriptions < )
Q_f_less_than_160
Q_g_more_than_220 <- sum(prescriptions >= )
Q_g_more_than_220
Q_h_middle_value <- median(prescriptions)
Q_h_middle_value
Q_i_between_170_and_210 <- sum(prescriptions >=  & prescriptions <= )
Q_i_between_170_and_210
#CHAPTER-4 Page No.102/No.9StemAndLeafMeanMaxMin----
calls <- c()
calls
stem(calls)
median_calls <- median(calls)
median_calls
max_calls <- max(calls)
min_calls <- min(calls)
max_calls
min_calls
#CHAPTER-4 Page No.102/No.10StemAndLeafMeanMaxMin----
atm_usage <- c()
atm_usage
stem(atm_usage)
median_usage <- median(atm_usage)
median_usage
max_usage <- max(atm_usage)
min_usage <- min(atm_usage)
max_usage
min_usage
#CHAPTER-4 Page No.106/No.11Quartiles----
data <- c()
median_value <- median(data)
quartiles <- quantile(data, probs = c())
quartiles
first_quartile <- quartiles[1]
third_quartile <- quartiles[2]
median_value
first_quartile
third_quartile
#CHAPTER-4 Page No.106/No.18BoxplotQuartile----
data <- c(116, 121, 157, 192, 207, 209, 209, 229, 232, 236, 236, 239, 243, 246, 
          260, 264, 276, 281, 283, 289, 296, 307, 309, 312, 317, 324, 341, 353)
boxplot(data, main = "Box Plot of Given Data", ylab = "Values", col = "lightblue")
quartiles <- quantile(data, probs = c(0.25, 0.5, 0.75))
first_quartile <- quartiles[1]
median_value <- quartiles[2]
third_quartile <- quartiles[3]
first_quartile
median_value
third_quartile
#CHAPTER-4 Page No.106/No.12Quartiles----
data <- c(5.24, 6.02, 6.67, 7.30, 7.59, 7.99, 8.03, 8.35, 8.81, 9.45,
          9.61, 10.37, 10.39, 11.86, 12.22, 12.71, 13.07, 13.59, 13.89, 15.42)
median_value <- median(data)
quartiles <- quantile(data, probs = c(0.25, 0.5, 0.75))
first_quartile <- quartiles[1]
median_value <- quartiles[2]
third_quartile <- quartiles[3]
first_quartile
median_value
third_quartile
#CHAPTER-4 Page No.106/No.13deciles----
Thomas_Supply_Company<- c(13, 13, 13, 20, 26, 27, 31, 34, 34, 34, 35, 35, 36, 37, 38, 41, 41, 41, 45, 47, 47, 47, 50, 51, 53, 54, 56, 62, 67, 82)
quartiles <- quantile(Thomas_Supply_Company, probs = c(0.25, 0.75))
Q_a_first_quartile <- quartiles[1]
Q_a_third_quartile <- quartiles[2]
deciles <- quantile(Thomas_Supply_Company, probs = c(0.2, 0.8))
Q_b_second_decile <- deciles[1]
Q_b_eighth_decile <- deciles[2]
Q_c_percentile_67 <- quantile(Thomas_Supply_Company, probs = 0.67)
Q_a_first_quartile
Q_a_third_quartile
Q_b_second_decile
Q_b_eighth_decile
Q_c_percentile_67
#CHAPTER-4 Page No.106/No.14ManualQuartile----
numberofcalls<- c(38, 40, 41, 45, 48, 48, 50, 50, 51, 51, 52, 52, 53, 54, 55, 55, 55, 56, 56, 57,
                  59, 59, 59, 62, 62, 62, 63, 64, 65, 66, 66, 67, 67, 69, 69, 71, 77, 78, 79, 79)
Q_a_median_value <- median(numberofcalls)
quartiles <- quantile(numberofcalls, probs = c(0.25, 0.75))
Q_b_first_quartile <- quartiles[1]
Q_b_third_quartile <- quartiles[2]
deciles <- quantile(numberofcalls, probs = c(0.1, 0.9))
Q_c_first_decile <- deciles[1]
Q_c_ninth_decile <- deciles[2]
Q_d_percentile_33 <- quantile(numberofcalls, probs = 0.33)
Q_a_median_value
Q_b_first_quartile
Q_b_third_quartile
Q_c_first_decile
Q_c_ninth_decile
Q_d_percentile_33
#CHAPTER-4 Page No.113/No.19skewness----
salaries <- c(36.0, 26.0, 33.0, 28.0, 31.0)
mean_value <- mean(salaries)
mean_value
median_value <- median(salaries)
median_value
sd_value <- sd(salaries)
sd_value
salaries_of_pearson_skewness <- 3 * (mean_value - median_value) / sd_value
salaries_of_pearson_skewness
library(moments) # Install if not already installed
software_skewness <- skewness(salaries)
software_skewness
#CHAPTER-4 Page No.113/No.20skewness----
salaries <- c(516.0, 548.0, 566.0, 534.0, 586.0, 529.0, 546.0, 523.0, 538.0, 523.0, 551.0, 552.0, 486.0, 558.0, 574.0)
mean_value <- mean(salaries)
mean_value
median_value <- median(salaries)
median_value
sd_value <- sd(salaries)
sd_value
pearson_skewness <- 3 * (mean_value - median_value) / sd_value
pearson_skewness
software_skewness <- skewness(salaries)
software_skewness
#CHAPTER-4 Page No.114/No.21skewness----
commissions <- c(3.9, 17.4, 5.7, 17.6, 7.3, 10.6, 13.0, 13.6, 15.1, 22.3, 38.6, 43.2, 87.7, 15.8, 17.1)
mean_value <- mean(commissions)
mean_value
median_value <- median(commissions)
median_value
sd_value <- sd(commissions)
sd_value
pearson_skewness <- 3 * (mean_value - median_value) / sd_value
pearson_skewness
software_skewness <- skewness(commissions)
software_skewnesss
#CHAPTER-4 Page No.119/No.25ChiSquaredTest----
dessert_order_data <- matrix(c(32, 68, 15, 85), nrow = 2, byrow = TRUE)
rownames(dessert_order_data) <- c("Yes", "No")
colnames(dessert_order_data) <- c("Male", "Female")
dessert_order_data
chi_squared_test <- chisq.test(dessert_order_data)
chi_squared_test
#CHAPTER-4 Page No.119/No.26ChiSquaredTest----
NumberofSharesHeld <- matrix(c(8, 6, 2, 6, 8, 1, 6, 12, 1), nrow = 3, byrow = TRUE)
rownames(NumberofSharesHeld ) <- c("Under 200", "200 up to 1000", "Over 1000")
colnames(NumberofSharesHeld ) <- c("Favour", "Oppose", "Undecided")
NumberofSharesHeld 
chi_squared_test <- chisq.test(NumberofSharesHeld )
chi_squared_test
