#Risk Assessment in Defense Operations Variables for likelihood and impact levels----
likelihood <- c("Very Low", "Low", "Medium", "High", "Very High")
impact <- c("Insignificant", "Minor", "Moderate", "Major", "Critical")
#  Create a Risk Matrix (5x5) using a combination of likelihood and impact
risk_matrix <- matrix(c(1, 2, 3, 4, 5,  # Very Low Likelihood
                        2, 4, 6, 8, 10,  # Low Likelihood
                        3, 6, 9, 12, 15, # Medium Likelihood
                        4, 8, 12, 16, 20, # High Likelihood
                        5, 10, 15, 20, 25), # Very High Likelihood
                      nrow = 5, byrow = TRUE)

#  Label the rows and columns for Likelihood and Impact
rownames(risk_matrix) <- likelihood
colnames(risk_matrix) <- impact

#  Print the risk matrix to view the combination of risk levels
print("Risk Matrix (Likelihood vs Impact)")
print(risk_matrix)

# Example: Assign random likelihood and impact for 5 scenarios (simulated risk assessment)
set.seed(123)  # For reproducibility
risk_scenarios <- data.frame(
  Scenario = 1:5,
  Likelihood = sample(likelihood, 5, replace = TRUE),
  Impact = sample(impact, 5, replace = TRUE)
)
#  Calculate Risk Level based on Risk Matrix
calculate_risk_level <- function(likelihood, impact) {
  risk_matrix[likelihood, impact]
}
# Apply the function to all scenarios
risk_scenarios$Risk_Level <- mapply(calculate_risk_level, risk_scenarios$Likelihood, risk_scenarios$Impact)
#  Show the calculated risk scenarios
print("Risk Assessment for Defense Operations")
print(risk_scenarios)

#Military Logistics Optimization----
install.packages("lpSolve")
library(lpSolve)
# Define cost coefficients (Objective function)
costs <- c(10, 5, 7)
# Define constraints matrix
constraints <- matrix(c(1, 0, 0,  # Fuel
                        0, 1, 0,  # Food
                        0, 0, 1), # Ammunition
                      nrow = 3, byrow = TRUE)
# Define right-hand side (available resources)
rhs <- c(100, 200, 150)
# Define the sense of inequalities (<=)
directions <- c("<=", "<=", "<=")
# Solve the linear programming problem
lp_solution <- lp(direction = "min", 
                  objective.in = costs, 
                  const.mat = constraints, 
                  const.dir = directions, 
                  const.rhs = rhs)
# Display the solution
lp_solution
lp_solution$solution  # Optimized resource allocation
# Supply quantities
supply <- c(100, 300, 250)  # supply at 3 bases

# Demand quantities
demand <- c(200, 150, 200, 100)  # demand at 4 units

# Cost matrix (rows: supply points, columns: demand points)
costs <- matrix(c(8, 6, 10, 9,
                  9, 12, 13, 7,
                  14, 9, 16, 5), nrow = 3, byrow = TRUE)

# Solve transportation problem using lp.transport from lpSolve package
transport_solution <- lp.transport(costs, "min", row.signs = rep("<=", 3), row.rhs = supply, 
                                   col.signs = rep(">=", 4), col.rhs = demand)
# Display the optimized solution
transport_solution
transport_solution$solution  # Optimal transportation plan
