#CHAPTER-4 Page No.100/No.2----
rates <- c(8.3, 9.6, 9.5, 9.1, 8.8, 11.2, 7.7, 10.1, 9.9, 10.8, 10.2, 8.0, 8.4, 8.1, 11.6, 9.6, 8.8, 8.0, 10.4, 9.8, 9.2)
stem(rates)
Q_a_less_than_9 <- sum(rates < 9.0)
Q_a_less_than_9
Q_b_rates_10_to_11 <- rates[rates >= 10.0 & rates < 11.0]
Q_b_rates_10_to_11
Q_b_median_rate <- median(rates)
Q_b_median_rate
Q_d_max_rate <- max(rates)
Q_d_min_rate <- min(rates)
Q_d_max_rate
Q_d_min_rate
#CHAPTER-4 Page No.101/No.3----
observations <- c(2, 3, 4, 5, 6, 7)
total_observations <- sum(observations)
total_observations
min_value <- min(observations)
max_value <- max(observations)
dot_counts <- c(3, 5, 2, 6, 4, 1, 3)
mode_value <- which.max(dot_counts)
cat("Total number of observations:", total_observations, "\n")
cat("Minimum value:", min_value, "\n")
cat("Maximum value:", max_value, "\n")
cat("Observations tend to cluster around value:", mode_value, "\n")
#CHAPTER-4 Page No.101/No.7----
units <- c(38, 56, 60, 61, 63, 63, 65, 65, 69, 70, 72, 73, 76, 77, 77, 78, 85, 89, 90, 90, 91, 95, 96, 103, 106)
units
Q_a_num_days <- length(units)
Q_a_num_days
Q_b_first_class <- sum(units >= 30 & units < 40)
Q_b_first_class
Q_c_min_value <- min(units)
Q_c_max_value <- max(units)
Q_d_min_value
Q_d_max_value
Q_d_fourth_row <- units[units >= 60 & units < 70]
Q_d_fourth_row
Q_e_second_row <- units[units >= 40 & units < 50]
Q_e_second_row
Q_f_less_than_70 <- sum(units < 70)
Q_f_less_than_70
Q_g_eighty_or_more <- sum(units >= 80)
Q_g_eighty_or_more
Q_h_median_value <- median(units)
Q_h_median_value
Q_i_between_60_and_89 <- sum(units >= 60 & units <= 89)
Q_i_between_60_and_89
#CHAPTER-4 Page No.102/No.8----
prescriptions <- c(126, 128, 129, 131, 132, 133, 146, 148, 148, 149, 155, 158, 159, 163, 165, 172, 174, 175, 176, 178, 182, 186, 188, 191, 193, 194, 195, 196, 200, 203, 204, 206, 207, 209, 212, 212, 213, 219, 227, 228, 229, 230, 230, 231, 237, 239, 248, 251, 253, 270)
prescriptions
Q_a_num_days <- length(prescriptions)
Q_a_num_days
Q_b_last_class <- sum(prescriptions >= 270 & prescriptions < 280)
Q_b_last_class
Q_c_min_value <- min(prescriptions)
Q_c_max_value <- max(prescriptions)
Q_c_min_value
Q_c_max_value
Q_d_fourth_row <- prescriptions[prescriptions >= 150 & prescriptions < 160]
Q_d_fourth_row
Q_e_next_to_last_row <- prescriptions[prescriptions >= 250 & prescriptions < 260]
Q_e_next_to_last_row
Q_f_less_than_160 <- sum(prescriptions < 160)
Q_f_less_than_160
Q_g_more_than_220 <- sum(prescriptions >= 220)
Q_g_more_than_220
Q_h_middle_value <- median(prescriptions)
Q_h_middle_value
Q_i_between_170_and_210 <- sum(prescriptions >= 170 & prescriptions <= 210)
Q_i_between_170_and_210
#CHAPTER-4 Page No.102/No.9----
calls <- c(52, 43, 30, 38, 30, 42, 12, 46, 39, 37, 34, 46, 32, 18, 41, 5)
calls
stem(calls)
median_calls <- median(calls)
median_calls
max_calls <- max(calls)
min_calls <- min(calls)
max_calls
min_calls
#CHAPTER-4 Page No.102/No.10----
atm_usage <- c(83,63,95,64,80,36,84,84,78,76,73,61,84,68,59,54,52,84,75,65,95,59,90,47,70,52,87,61,77,60)
atm_usage
stem(atm_usage)
median_usage <- median(atm_usage)
median_usage
max_usage <- max(atm_usage)
min_usage <- min(atm_usage)
max_usage
min_usage
#CHAPTER-4 Page No.106/No.11----
data <- c(46, 47, 49, 49, 51, 53, 54, 54, 55, 55, 59)
median_value <- median(data)
quartiles <- quantile(data, probs = c(0.25, 0.75))
quartiles
first_quartile <- quartiles[1]
third_quartile <- quartiles[2]
median_value
first_quartile
third_quartile
#CHAPTER-4 Page No.106/No.18----
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
#CHAPTER-4 Page No.106/No.12----
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
#CHAPTER-4 Page No.106/No.13----
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
#CHAPTER-4 Page No.106/No.14----
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
#CHAPTER-4 Page No.113/No.19----
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
#CHAPTER-4 Page No.113/No.20----
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
#CHAPTER-4 Page No.114/No.21----
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
#CHAPTER-4 Page No.119/No.25----
dessert_order_data <- matrix(c(32, 68, 15, 85), nrow = 2, byrow = TRUE)
rownames(dessert_order_data) <- c("Yes", "No")
colnames(dessert_order_data) <- c("Male", "Female")
dessert_order_data
chi_squared_test <- chisq.test(dessert_order_data)
chi_squared_test
#CHAPTER-4 Page No.119/No.26----
NumberofSharesHeld <- matrix(c(8, 6, 2, 6, 8, 1, 6, 12, 1), nrow = 3, byrow = TRUE)
rownames(NumberofSharesHeld ) <- c("Under 200", "200 up to 1000", "Over 1000")
colnames(NumberofSharesHeld ) <- c("Favour", "Oppose", "Undecided")
NumberofSharesHeld 
chi_squared_test <- chisq.test(NumberofSharesHeld )
chi_squared_test

#CHAPTER-5 Page No.140/No.3----
hits <- 30
attempts <- 100
empirical_prob <- hits / attempts
empirical_prob
total_members <- 7
classical_prob <- 1 / total_members
classical_prob
total_tickets <- 5000000
classical_prob_lottery <- 1 / total_tickets
classical_prob_lottery 
subjective_prob_earthquake <- 0.80
subjective_prob_earthquake 
#CHAPTER-5 Page No.140/No.5----
total_students <- 34
management_majors <- 6
prob_management <- management_majors / total_students
prob_management 
probability_type <-  "Empirical"
probability_type
#CHAPTER-5 Page No.140/No.6----
employees <- c("M1", "M2", "M3", "M4", "M5", "M6", "W1", "W2", "W3")
possible_outcomes <- combn(employees, 2)
outcomes_list <- apply(possible_outcomes, 2, paste, collapse = ",")
cat("All possible outcomes of promoting two employees are:\n")
print(outcomes_list)
total_outcomes <- ncol(possible_outcomes)
cat("\nTotal number of outcomes:", total_outcomes, "\n")
cat("\nProbability concept used: Classical probability, where each outcome is equally likely.\n")
#CHAPTER-5 Page No.144/No.5-3----
supervisors <- 120
maintenance <- 50
production <- 1460
management <- 302
secretarial <- 68
total_employees <- supervisors + maintenance + production + management + secretarial
probability_B_or_E <- (maintenance + secretarial) / total_employees
probability_not_D <- 1 - (management / total_employees)
cat("Probability of selecting either maintenance or secretarial:", probability_B_or_E, "\n")
cat("Probability of selecting not in management:", probability_not_D, "\n")
venn_data <- list(Maintenance = maintenance, Secretarial = secretarial)
venn.plot <- venn.diagram(
  x = venn_data,
  category.names = c("Maintenance", "Secretarial"),
  filename = NULL,
  output  = TRUE,
  lwd = 2,
  col = c("blue", "red"),
  fill = c("lightblue", "lightpink"),
  alpha = 0.5,
  cat.col = c("blue", "red"),
  cat.cex = 1.5,
  cat.pos = c(0, 0),
  main = "Venn Diagram of Maintenance and Secretarial Employees",
  main.cex = 2
)
grid.draw(venn.plot)
#CHAPTER-5 Page No.146/No.11----
P_A <- 0.30
P_B <- 0.20
P_A_or_B <- P_A + P_B
P_neither_A_nor_B <- 1 - P_A_or_B
cat("Probability of either A or B occurring:", P_A_or_B, "\n")
cat("Probability of neither A nor B occurring:", P_neither_A_nor_B, "\n")
#CHAPTER-5 Page No.146/No.12----
P_X <- 0.05
P_Y <- 0.02
P_X_or_Y <- P_X + P_Y
P_neither_X_nor_Y <- 1 - P_X_or_Y
cat("Probability of either X or Y occurring:", P_X_or_Y, "\n")
cat("Probability of neither X nor Y occurring:", P_neither_X_nor_Y, "\n")
#CHAPTER-5 Page No.146/No.13----
firms_under_1M <- 102
firms_1M_to_20M <- 61
firms_over_20M <- 37
total_firms <- 200
P_under_1M <- firms_under_1M / total_firms
P_1M_to_20M <- firms_1M_to_20M / total_firms
P_over_20M <- firms_over_20M / total_firms
P_1M_to_20M_or_over_20M <- P_1M_to_20M + P_over_20M
cat("Probability of income under $1 million:", P_under_1M, "\n")
cat("Probability of income between $1 million and $20 million or over $20 million:", P_1M_to_20M_or_over_20M, "\n")
#CHAPTER-5 Page No.146/No.14----
P_profit <- 0.50     
P_break_even <- 0.30 
P_lose_money <- 0.20 
P_not_lose_money_addition <- P_profit + P_break_even
P_not_lose_money_complement <- 1 - P_lose_money
cat("Probability of not losing money (Addition Rule):", P_not_lose_money_addition, "\n")
cat("Probability of not losing money (Complement Rule):", P_not_lose_money_complement, "\n")
#CHAPTER-5 Page No.146/No.15----
P_A <- 0.25  
P_B <- 0.50  
P_above_C <- P_A + P_B
cat("The probability of getting a grade above a C is:", P_above_C, "\n")
#CHAPTER-5 Page No.147/No.18----
P_X <- 0.55     
P_Y <- 0.35     
P_X_and_Y <- 0.20
P_X_or_Y <- P_X + P_Y - P_X_and_Y
cat("The probability of either X or Y occurring is:", P_X_or_Y, "\n")
#CHAPTER-5 Page No.147/No.19----
P_History <- 0.60   
P_Math <- 0.70       
P_Both <- 0.50       
P_At_Least_One <- P_History + P_Math - P_Both
cat("The probability of passing at least one course is:", P_At_Least_One, "\n")
#CHAPTER-5 Page No.147/No.22-----
P_Yellowstone <- 0.50  
P_Tetons <- 0.40       
P_Both <- 0.35         
# (a) Calculate the probability of visiting at least one attraction (Yellowstone or Tetons)
P_At_Least_One <- P_Yellowstone + P_Tetons - P_Both
# Output the result for part (a)
cat("The probability of visiting at least one attraction is:", P_At_Least_One, "\n")
# (b) Explanation of what 0.35 represents
cat("The probability 0.35 represents the joint probability of visiting both Yellowstone and the Tetons.\n")
# (c) Explanation for whether the events are mutually exclusive
if (P_Both > 0) {
  cat("The events are not mutually exclusive because the probability of visiting both attractions is greater than zero.\n")
} else {
  cat("The events are mutually exclusive because the probability of visiting both attractions is zero.\n")
}
#CHAPTER-5 Page No.149/example----
total_shirts <- 12
white_shirts <- 9
prob_first_day <- white_shirts / total_shirts
prob_second_day <- (white_shirts - 1) / (total_shirts - 1)
prob_both_days <- prob_first_day * prob_second_day
prob_both_days
#CHAPTER-5 Page No.156/No.29----
# Define the contingency table as a matrix
sales_data <- matrix(c(16, 12, 22, 45, 60, 45, 93, 72, 135), 
                     nrow = 3, byrow = TRUE)
rownames(sales_data) <- c("Below average", "Average", "Above average")
colnames(sales_data) <- c("Fair", "Good", "Excellent")
sales_data
total_salespeople <- sum(sales_data)
prob_above_avg_and_excellent <- sales_data["Above average", "Excellent"] / total_salespeople
prob_above_avg_and_excellent
# Conditional probabilities
prob_below_avg <- rowSums(sales_data)[1] / total_salespeople
prob_avg <- rowSums(sales_data)[2] / total_salespeople
prob_above_avg <- rowSums(sales_data)[3] / total_salespeople
joint_probs <- sales_data / total_salespeople
joint_probs
# install.packages("data.tree")
library(data.tree)
sales_tree <- Node$new("Sales Ability")
below_avg <- sales_tree$AddChild("Below Average")
below_avg$AddChild("Fair", prob = joint_probs[1, 1])
below_avg$AddChild("Good", prob = joint_probs[1, 2])
below_avg$AddChild("Excellent", prob = joint_probs[1, 3])
average <- sales_tree$AddChild("Average")
average$AddChild("Fair", prob = joint_probs[2, 1])
average$AddChild("Good", prob = joint_probs[2, 2])
average$AddChild("Excellent", prob = joint_probs[2, 3])
above_avg <- sales_tree$AddChild("Above Average")
above_avg$AddChild("Fair", prob = joint_probs[3, 1])
above_avg$AddChild("Good", prob = joint_probs[3, 2])
above_avg$AddChild("Excellent", prob = joint_probs[3, 3])
print(sales_tree, "prob")
#CHAPTER-5 Page No.156/No.31----
# Create a data frame from the provided table
data <- data.frame(
  CollegeType = c("Junior College", "Four-Year College", "Graduate School"),
  Snowboarding = c(68, 84, 59),
  Skiing = c(41, 56, 74),
  IceSkating = c(46, 70, 47)
)
data
# Calculate total students
total_students <- sum(data$Snowboarding + data$Skiing + data$IceSkating)
calculate_probability <- function(event_count, total_count) {
  event_count / total_count
}
# a. Probability of selecting a student whose favorite sport is skiing
prob_skiing <- calculate_probability(sum(data$Skiing), total_students)
prob_skiing 
# b. Probability of selecting a junior-college student
prob_junior_college <- calculate_probability(sum(data$Snowboarding[1] + data$Skiing[1] + data$IceSkating[1]), total_students)
prob_junior_college
# c. Probability of ice skating given four-year college
prob_ice_skating_given_four_year <- calculate_probability(data$IceSkating[2], sum(data$Snowboarding[2] + data$Skiing[2] + data$IceSkating[2]))
# d. Probability of junior college given snowboarding
prob_junior_college_given_snowboarding <- calculate_probability(data$Snowboarding[1], sum(data$Snowboarding))
# e. Probability of skiing or ice skating given graduate school
prob_skiing_or_ice_skating_given_graduate <- calculate_probability(data$Skiing[3] + data$IceSkating[3], sum(data$Snowboarding[3] + data$Skiing[3] + data$IceSkating[3]))
# Print the results
cat("a. Probability of selecting a student whose favorite sport is skiing:", prob_skiing, "\n")
cat("b. Probability of selecting a junior-college student:", prob_junior_college, "\n")
cat("c. Probability of ice skating given four-year college:", prob_ice_skating_given_four_year, "\n")
cat("d. Probability of junior college given snowboarding:", prob_junior_college_given_snowboarding, "\n")
cat("e. Probability of skiing or ice skating given graduate school:", prob_skiing_or_ice_skating_given_graduate, "\n")

#CHAPTER-5 Page No.156/No.33----
# Define probabilities
P_A1 <- 0.60
P_A2 <- 0.40
P_B1_given_A1 <- 0.05
P_B1_given_A2 <- 0.10
# Calculate P(A1 | B1) using Bayes' theorem
P_A1_given_B1 <- (P_B1_given_A1 * P_A1) / (P_B1_given_A1 * P_A1 + P_B1_given_A2 * P_A2)
# Print the result
cat("P(A1 | B1) =", P_A1_given_B1)
#CHAPTER-5 Page No.156/No.34----
# Define probabilities
P_A1 <- 0.20
P_A2 <- 0.40
P_A3 <- 0.40
P_B1_given_A1 <- 0.25
P_B1_given_A2 <- 0.05
P_B1_given_A3 <- 0.10
# Calculate P(A3 | B1) using Bayes' theorem
P_A3_given_B1 <- (P_B1_given_A3 * P_A3) / (P_B1_given_A1 * P_A1 + P_B1_given_A2 * P_A2 + P_B1_given_A3 * P_A3)
# Print the result
cat("P(A3 | B1) =", P_A3_given_B1)
#CHAPTER-5 Page No.156/No.35----
# Define probabilities
P_night <- 0.70
P_day <- 0.30
P_win_given_night <- 0.50
P_win_given_day <- 0.90
# Calculate P(Night | Win) using Bayes' theorem
P_night_given_win <- (P_win_given_night * P_night) / 
  (P_win_given_night * P_night + P_win_given_day * P_day)
# Print the result
cat("P(Night | Win) =", P_night_given_win)
#CHAPTER-5 Page No.156/No.36----
# Define probabilities
P_do_assignments <- 0.80
P_pass_given_do_assignments <- 0.90
P_pass_given_not_do_assignments <- 0.60
# Calculate P(Do Assignments | Pass) using Bayes' theorem
P_do_assignments_given_pass <- (P_pass_given_do_assignments * P_do_assignments) / 
  (P_pass_given_do_assignments * P_do_assignments + 
     P_pass_given_not_do_assignments * (1 - P_do_assignments))
# Print the result
cat("P(Do Assignments | Pass) =", P_do_assignments_given_pass)
#CHAPTER-5 Page No.156/No.37----
# Define probabilities
P_cash <- 0.30
P_credit_card <- 0.30
P_debit_card <- 0.40
P_purchase_over_50_given_cash <- 0.20
P_purchase_over_50_given_credit_card <- 0.90
P_purchase_over_50_given_debit_card <- 0.60

# Calculate P(Cash | Purchase > $50) using Bayes' theorem
P_cash_given_purchase_over_50 <- (P_purchase_over_50_given_cash * P_cash) / 
  (P_purchase_over_50_given_cash * P_cash + 
     P_purchase_over_50_given_credit_card * P_credit_card + 
     P_purchase_over_50_given_debit_card * P_debit_card)
# Print the result
cat("P(Cash | Purchase > $50) =", P_cash_given_purchase_over_50)
#CHAPTER-5 Page No.156/No.38----
# Define probabilities
P_open <- 0.25
P_closed <- 0.75
P_stolen_given_open <- 0.05
P_stolen_given_closed <- 0.01
# Calculate P(Open | Stolen) using Bayes' theorem
P_open_given_stolen <- (P_stolen_given_open * P_open) / 
  (P_stolen_given_open * P_open + P_stolen_given_closed * P_closed)
# Print the result
cat("P(Open | Stolen) =", P_open_given_stolen)

#CHAPTER-5 Page No.166/No.1----
Pro_of_x_equal_2<-dbinom(2, size = 8, prob = 0.30,log = FALSE)#dbinom(x_number_of_success,n_size_number_of_trial,Prob_success_probability)
Pro_of_x_equal_2
Pro_of_x_Smallequal_2<-pbinom(2, size = 8, prob = 0.30,log = FALSE)
Pro_of_x_Largeequal_2<-1 - pbinom(2, size = 8, prob = 0.30,log = FALSE)
#hypergeometric Distribution powerpoint22----
# Define parameters
N <- 15  # Total population
K <- 10  # Total acceptable items
n <- 4   # Sample size
k <- 3   # Acceptable items in the sample
# Calculate the probability using the hypergeometric distribution
probability <- dhyper(k, K, N - K, n)
# Print the probability
cat("The probability that exactly three out of four items are acceptable is:", probability, "\n")
#hypergeometric Distribution powerpoint28----
# Define parameters
N <- 8  # Total faculty members
K <- 6  # Total tenured faculty members
n <- 3  # Committee size
k <- 3  # All committee members are tenured
# Calculate the probability using the hypergeometric distribution
probability <- dhyper(k, K, N - K, n)
# Print the probability
cat("The probability that all members of the committee are tenured is:", probability, "\n")
#Poission Distribution powerpoint33----
# Define parameters
n <- 40          # Total number of loans
p <- 0.025       # Probability of default
# Part (a): Probability of exactly 3 defaults
probability_exactly_3 <- dbinom(3, n, p)
cat("The probability that exactly three loans will default is:", probability_exactly_3, "\n")
# Part (b): Probability of at least 3 defaults
probability_at_least_3 <- 1 - pbinom(2, n, p)
cat("The probability that at least three loans will default is:", probability_at_least_3, "\n")
#Poission Distribution powerpoint34----
# Define parameters
n <- 40          # Total number of loans
p <- 0.025       # Probability of default
# Part (a): Probability of exactly 3 defaults
probability_exactly_3 <- dbinom(3, n, p)
cat("The probability that exactly three loans will default is:", probability_exactly_3, "\n")
# Part (b): Probability of at least 3 defaults
probability_at_least_3 <- 1 - pbinom(2, n, p)
cat("The probability that at least three loans will default is:", probability_at_least_3, "\n")

#CHAPTER-5 Page No.166/No.1
#CHAPTER-5 Page No.166/No.1
#CHAPTER-5 Page No.166/No.1
#CHAPTER-5 Page No.166/No.1
#CHAPTER-5 Page No.166/No.1
