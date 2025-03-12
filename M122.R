# Load tidyverse
library(tidyverse)

# Input data
data <- tibble(
  AgeGroup = c("0-4", "5-14", "15-49", "50-64", "65+", "85+"),
  Male_2005 = c(85520, 180240, 342600, 42830, 34420, 8260),
  Female_2005 = c(82540, 175300, 332470, 48240, 42580, 5750),
  Male_2010 = c(94420, 199000, 378260, 47280, 38002, 9400),
  Female_2010 = c(91130, 193540, 367080, 53260, 47010, 6050)
)

# Combine male and female population for each year
data <- data %>%
  mutate(
    Total_2005 = Male_2005 + Female_2005,
    Total_2010 = Male_2010 + Female_2010
  )

# Calculate metrics for both 2005 and 2010
results <- data %>%
  summarize(
    # Group totals for 2005
    Total_0_14_2005 = sum(Total_2005[AgeGroup %in% c("0-4", "5-14")]),
    Total_15_64_2005 = sum(Total_2005[AgeGroup %in% c("15-49", "50-64")]),
    Total_65Plus_2005 = sum(Total_2005[AgeGroup %in% c("65+", "85+")]),
    
    # Group totals for 2010
    Total_0_14_2010 = sum(Total_2010[AgeGroup %in% c("0-4", "5-14")]),
    Total_15_64_2010 = sum(Total_2010[AgeGroup %in% c("15-49", "50-64")]),
    Total_65Plus_2010 = sum(Total_2010[AgeGroup %in% c("65+", "85+")]),
    
    # Old-Age Dependency Ratio
    OldAgeDependencyRatio_2005 = (Total_65Plus_2005 / Total_15_64_2005) * 100,
    OldAgeDependencyRatio_2010 = (Total_65Plus_2010 / Total_15_64_2010) * 100,
    
    # Parents Support Ratio
    ParentsSupportRatio_2005 = (sum(Total_2005[AgeGroup %in% c("85+")]) / sum(Total_2005[AgeGroup %in% c("50-64")])) * 100,
    ParentsSupportRatio_2010 = (sum(Total_2010[AgeGroup %in% c("85+")]) / sum(Total_2010[AgeGroup %in% c("50-64")])) * 100,
    
    # Ageing Index
    AgeingIndex_2005 = (Total_65Plus_2005 / Total_0_14_2005) * 100,
    AgeingIndex_2010 = (Total_65Plus_2010 / Total_0_14_2010) * 100,
    
    # Potential Support Ratio
    PotentialSupportRatio_2005 = Total_15_64_2005 / Total_65Plus_2005,
    PotentialSupportRatio_2010 = Total_15_64_2010 / Total_65Plus_2010
  )
# Print the results
print(results)

# <<<<< ============================================================= >>>>> 
# Input data
data <- tibble(
  AgeGroup = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
  MidYearFemalePopulation = c(45410, 52453, 50588, 42693, 38247, 25845, 23545),
  Births = c(2055, 6487, 6256, 3345, 1264, 315, 22)
)

# Step 1: Add cumulative frequency of births
data <- data %>%
  mutate(CumulativeBirths = cumsum(Births))

# Step 2: Calculate total births and identify the median position
total_births <- sum(data$Births)
median_position <- total_births / 2

# Step 3: Identify the age group containing the median
data <- data %>%
  mutate(IsMedianGroup = CumulativeBirths >= median_position & lag(CumulativeBirths, default = 0) < median_position)

median_group <- data %>% filter(IsMedianGroup)

# Step 4: Calculate the median age of childbearing
median_age <- median_group %>%
  mutate(
    LowerLimit = as.numeric(str_extract(AgeGroup, "^[0-9]+")),
    ClassWidth = as.numeric(str_extract(AgeGroup, "[0-9]+$")) - LowerLimit,
    PreviousCumulative = lag(CumulativeBirths, default = 0),
    MedianAge = LowerLimit + ((median_position - PreviousCumulative) / Births) * ClassWidth
  ) %>%
  pull(MedianAge)

# Print results
print(data)
print(paste("Median Age of Childbearing:", round(median_age, 2)))
# <<<<< ============================================================= >>>>> 

# Input data
data <- tibble(
  Age_Group = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
  Mid_Year_Population = c(45410, 52453, 50588, 42693, 38247, 25845, 23545),
  Births = c(2055, 6487, 6256, 3345, 1264, 315, 22)
)

# Step 1: Calculate ASFR (Age-Specific Fertility Rate)
data <- data %>%
  mutate(ASFR = (Births / Mid_Year_Population) * 1000)

# Step 2: Calculate cumulative frequency of ASFR
data <- data %>%
  mutate(Cumulative_ASFR = cumsum(ASFR))

# Step 3: Calculate the Median Age of Childbearing
# The median is the age group where the cumulative ASFR reaches 50% of the total ASFR
total_ASFR <- sum(data$ASFR)
data <- data %>%
  mutate(Percentage_Cumulative_ASFR = (Cumulative_ASFR / total_ASFR) * 100)

median_age_group <- data %>%
  filter(Percentage_Cumulative_ASFR >= 50) %>%
  slice(1) %>%
  pull(Age_Group)

# Print the results
print(data)

# Output the median age group
cat("The median age of childbearing is in the age group:", median_age_group, "\n")
# <<<<< ============================================================= >>>>> 

# Input the data
data <- tibble(
  Village = c("A", "B"),
  Births = c(190, 300),
  Deaths = c(180, 410)
)

# Step 1: Calculate Natural Increase
data <- data %>%
  mutate(Natural_Increase = Births - Deaths)

# Step 2: Determine if the population has decreased
data <- data %>%
  mutate(Population_Decreased = if_else(Natural_Increase < 0, TRUE, FALSE))

# Print the results
print(data)

# Identify the village with population decrease
decreased_village <- data %>%
  filter(Population_Decreased == TRUE) %>%
  pull(Village)

cat("The village(s) with a population decrease is/are:", paste(decreased_village, collapse = ", "), "\n")
# <<<<< ============================================================= >>>>> 
# Load the required library
library(tidyverse)
# Input the data
migration_data <- tibble(
  Country = c("China", "Rep. of Korea", "Philippines"),
  Year = c(1997, 1997, 1997),
  Population = c(1220224, 44909, 58242),  # In Thousands
  Immigration = c(283, 1237, 67),
  Emigration = c(236, 1232, 65)
)

# Step 1: Calculate Crude Immigration Rate (CIR), Crude Emigration Rate (CER), and Net Migration Rate (NMR)
migration_rates <- migration_data %>%
  mutate(
    CMR = (Immigration / Emigration) * 1000,
    CIMR = (Immigration / Population) * 1000,
    COMR = (Emigration / population) * 1000,
  )

# Step 2: Display the results
print(migration_rates)
# <<<<< ============================================================= >>>>> 
# Input the data
population_data <- tibble(
  Year = c(2005),
  Population_2005 = 45486 * 1000,  # Convert '000 to absolute numbers
  Births = 34570,
  Deaths = 13674,
  In_Migrants = 5675,
  Out_Migrants = 2345
)
# Calculate the required metrics
population_analysis <- population_data %>%
  mutate(
    # Calculate Natural Increase
    Natural_Increase = Births - Deaths,
    # Calculate Net Migration
    Net_Migration = In_Migrants - Out_Migrants,
    # Calculate Net Population Increase/Decrease
    Net_Change = Natural_Increase + Net_Migration,
    # Estimate Population for 2010
    Population_2010 = Population_2005 + Net_Change
  )
# Print the results
print(population_analysis)
# <<<<< ============================================================= >>>>> 
# Load the tidyverse library
# Input the migration data
migration_data <- tribble(
  ~Origin, ~A, ~B, ~C, ~D,
  "A", 0, 621, 213, 441,
  "B", 737, 0, 256, 153,
  "C", 245, 104, 0, 357,
  "D", 331, 540, 897, 0
)

# Calculate total in-migrants, out-migrants, and net-migrants
migration_analysis <- migration_data %>%
  pivot_longer(cols = A:D, names_to = "Destination", values_to = "Flows") %>%
  group_by(Destination) %>%
  summarize(In_Migrants = sum(Flows)) %>%
  left_join(
    migration_data %>%
      pivot_longer(cols = A:D, names_to = "Destination", values_to = "Flows") %>%
      group_by(Origin) %>%
      summarize(Out_Migrants = sum(Flows)),
    by = c("Destination" = "Origin")
  ) %>%
  mutate(
    Net_Migrants = In_Migrants - Out_Migrants,
    Population_Change = case_when(
      Net_Migrants > 0 ~ "Increased",
      Net_Migrants < 0 ~ "Decreased",
      TRUE ~ "Unchanged"
    )
  )

# Print the results
print(migration_analysis)
# <<<<< ============================================================= >>>>> 
# Input data
data <- tribble(
  ~Age_in_2000, ~Age_in_2010, ~Population_2000, ~Population_2010, ~Probability_of_Dying,
  "0-4",   "10-14", 5846, 7534, 0.0148,
  "5-9",   "15-19", 5591, 7291, 0.0156,
  "10-14", "20-24", 5243, 6912, 0.0232,
  "15-19", "25-29", 4715, 6754, 0.0295,
  "20-24", "30-34", 4322, 6238, 0.0419,
  "25-29", "35-39", 4012, 5973, 0.0709,
  "30-34", "40-49", 3857, 5415, 0.1251
)

# Step 1: Calculate Probability of Surviving
data <- data %>%
  mutate(Probability_of_Surviving = 1 - Probability_of_Dying)

# Step 2: Calculate Expected Survivors
data <- data %>%
  mutate(Expected_Survivors = Population_2000 * Probability_of_Surviving)

# Step 3: Calculate Net Migration
data <- data %>%
  mutate(Net_Migration = Population_2010 - Expected_Survivors)

# Step 4: Summarize Total Net Migration
total_net_migration <- sum(data$Net_Migration)

# Display the results
print(data)
cat("The estimated total net migration is:", total_net_migration)

# Load necessary library
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)

# Create the data frame
data <- data.frame(
  car = c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710", "Hornet 4 Drive", "Hornet Sportabout", 
          "Valiant", "Duster 360", "Merc 240D", "Merc 230", "Merc 280", "Merc 280C", 
          "Merc 450SE", "Merc 450SL", "Merc 450SLC", "Cadillac Fleetwood", "Lincoln Continental", 
          "Chrysler Imperial", "Fiat 128", "Honda Civic", "Toyota Corolla", "Toyota Corona", 
          "Dodge Challenger", "AMC Javelin", "Camaro Z28", "Pontiac Firebird", "Fiat X1-9", 
          "Porsche 914-2", "Lotus Europa", "Ford Pantera L", "Ferrari Dino", "Maserati Bora", 
          "Volvo 142E"),
  hp = c(110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180, 180, 180, 205, 215, 230, 66, 
         52, 65, 97, 150, 150, 245, 175, 66, 91, 113, 264, 175, 335, 109),
  cyl = c(6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8, 8, 8, 8, 4, 4, 4, 8, 
          6, 8, 4)
)

# Rank the horsepower data
data <- data %>%
  mutate(rank = rank(hp))

# Sum the ranks for each group of cylinders
rank_sums <- data %>%
  group_by(cyl) %>%
  summarise(sum_ranks = sum(rank), count = n())

# Calculate the Kruskal-Wallis H statistic
N <- nrow(data)  # Total number of observations
k <- n_distinct(data$cyl)  # Number of groups
H <- 12 / (N * (N + 1)) * sum((rank_sums$sum_ranks^2) / rank_sums$count) - 3 * (N + 1)

# Degrees of freedom
df <- k - 1

# Calculate p-value
p_value <- 1 - pchisq(H, df)

# Print the results
cat("Kruskal-Wallis chi-squared =", H, "\n")
cat("Degrees of freedom =", df, "\n")
cat("p-value =", p_value, "\n")


