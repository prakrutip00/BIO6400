install.packages("readxl")
install.packages("summarytools")
install.packages("ggplot2")
install.packages("car")
install.packages("dplyr")
install.packages("tidyr")
install.packages("broom")
install.packages("boot")
install.packages("tidyverse")
install.packages("stats")

# import packages
library(readxl)
library(summarytools)
library(ggplot2)
library(car)
library(dplyr)
library(tidyr)
library(broom)
library(boot)
library(tidyverse)
library(stats)

# use read_excel function to read the dataset into varibale data
data <- read_excel("assignment_3/FEV Study.xls")

col_names <- list("Sex", "Age", "FEV1", "PAT", "Country", "Height", "Weight", "Drug_Code", "Smoking", "Gold Classification", "FVC_baseline", "FEV1_Basleine", "Exacerbations", "Number_Exacerbation")

# Ensure DRUG_CODE is a factor before running analyses
data$DRUG_CODE <- as.factor(data$DRUG_CODE)

# 1. Shapiro-Wilk test for normality on the continuous variable AGE, grouped by DRUG_CODE
shapiro_results <- data %>%
  group_by(DRUG_CODE) %>%
  summarise(
    AGE_normality_p = tryCatch(shapiro.test(AGE)$p.value, error = function(e) NA)
  )

# Display Shapiro-Wilk test results for AGE
print("Shapiro-Wilk test result for AGE by DRUG_CODE:")
print(shapiro_results)

#  For categorical variables (SEX, SMOKING, GOLD CLASSIFICATION)
sex_counts <- table(data$SEX)
smoking_counts <- table(data$SMOKING)
gold_classification_counts <- table(data$`GOLD CLASSIFICATION`)

# Display counts for each categorical variable
print("Counts for SEX:")
print(sex_counts)
print("Counts for SMOKING:")
print(smoking_counts)
print("Counts for GOLD CLASSIFICATION:")
print(gold_classification_counts)

# Perform Shapiro-Wilk test for normality for AGE
age_test_result <- shapiro.test(data$AGE)$p.value

# Perform Shapiro-Wilk test for normality for SEX (if you must, but not typically done as it's categorical)
sex_counts <- table(data$SEX)

# Load necessary libraries
library(dplyr)
library(car)
library(ggplot2)

# Load the dataset (replace with actual path and code to load 'FEV study.xls')
# data <- read_excel("/path/to/FEV study.xls")

# Check the structure of the data
str(data)

# Check and convert 'SMOKING' to numeric if it contains categorical values like "current smoker" and "Ex-smoker"
if (!is.numeric(data$SMOKING)) {
    if (all(unique(data$SMOKING) %in% c("Current smoker", "Ex-smoker"))) {
        # Convert "current smoker" to 1 and "Ex-smoker" to 0
        data$SMOKING <- as.numeric(data$SMOKING == "Current smoker")
    }
}

# Check and convert 'GOLD CLASSIFICATION' to numeric if it contains specific categorical values
if (!is.numeric(data$`GOLD CLASSIFICATION`)) {
    # Map "2A" to 1 and "2B" to 2
    if (all(unique(data$`GOLD CLASSIFICATION`) %in% c("2A", "2B"))) {
        data$`GOLD CLASSIFICATION` <- as.numeric(factor(data$`GOLD CLASSIFICATION`,
                                                        levels = c("2A", "2B")))
    }
}

# Shapiro-Wilk test for normality on AGE and define 'age_normality_met'
age_shapiro_test <- shapiro.test(na.omit(data$AGE))
age_normality_met <- age_shapiro_test$p.value > 0.05  # TRUE if normality assumption is met

# Print Shapiro-Wilk test result for AGE
print(paste("Shapiro-Wilk test for AGE: p-value =", age_shapiro_test$p.value))
print(paste("Normality assumption for AGE met:", age_normality_met))

# Function to perform Shapiro-Wilk test by group for a given variable
shapiro_test_by_group <- function(variable, group) {
    shapiro_results <- data %>%
        group_by(!!sym(group)) %>%
        summarise(p_value = shapiro.test(na.omit(!!sym(variable)))$p.value, .groups = 'drop')
    return(shapiro_results)
}

# Perform Shapiro-Wilk test for SMOKING and GOLD CLASSIFICATION by group (DRUG_CODE)
smoking_test_result <- shapiro_test_by_group("SMOKING", "DRUG_CODE")
gold_test_result <- shapiro_test_by_group("GOLD CLASSIFICATION", "DRUG_CODE")

# Print group-wise Shapiro-Wilk test results
print("Shapiro-Wilk test for SMOKING by group:")
print(smoking_test_result)
print("Shapiro-Wilk test for GOLD CLASSIFICATION by group:")
print(gold_test_result)

# Levene's test for equality of variances for AGE and SMOKING
age_levene_result <- leveneTest(AGE ~ DRUG_CODE, data = data)
smoking_levene_result <- leveneTest(SMOKING ~ DRUG_CODE, data = data)

# Define variance equality for AGE and SMOKING based on Levene's test p-values
age_variance_equality_met <- age_levene_result$p.value > 0.05
smoking_variance_equality_met <- smoking_levene_result$p.value > 0.05

# Print Levene's test results and variance equality status
print("Levene's test for AGE:")
print(age_levene_result)
print(paste("Variance equality assumption for AGE met:", age_variance_equality_met))

print("Levene's test for SMOKING:")
print(smoking_levene_result)
print(paste("Variance equality assumption for SMOKING met:", smoking_variance_equality_met))

# Mann-Whitney U Test for non-normal variables (if any)
# Assuming AGE, SMOKING, GOLD CLASSIFICATION might be relevant

# Mann-Whitney test for AGE between groups if normality is not met
if (!age_normality_met) {
    age_mann_whitney_result <- wilcox.test(AGE ~ DRUG_CODE, data = data, exact = FALSE)
    print("Mann-Whitney U Test for AGE:")
    print(age_mann_whitney_result)
}

# Create density plots for AGE and other numeric variables
ggplot(data, aes(x = AGE, fill = DRUG_CODE)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot for AGE by Drug Code") +
    theme_minimal()

ggplot(data, aes(x = SMOKING, fill = DRUG_CODE)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot for SMOKING by Drug Code") +
    theme_minimal()

ggplot(data, aes(x = `GOLD CLASSIFICATION`, fill = DRUG_CODE)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot for GOLD CLASSIFICATION by Drug Code") +
    theme_minimal()

# Chi-Square test for categorical variables (e.g., SEX by DRUG_CODE)
sex_test_result <- chisq.test(table(data$SEX, data$DRUG_CODE))
print("Chi-Square Test for SEX and DRUG_CODE:")
print(sex_test_result)

# Final outputs
cat("Normality assumption for AGE met:", age_normality_met, "\n")
cat("Variance equality for AGE met:", age_variance_equality_met, "\n")
cat("Variance equality for SMOKING met:", smoking_variance_equality_met, "\n")

# Check the structure of the data
str(data)

# Step 1: Chi-Square Test for EXACERBATIONS
table_exacerbations <- table(data$DRUG_CODE, data$EXACERBATIONS)

# Perform Chi-Square test
chi_square_result <- chisq.test(table_exacerbations)

# Print Chi-Square test results
print(chi_square_result)

# Calculate odds ratio and 95% confidence interval
if (all(table_exacerbations > 0)) {
    odds_ratio <- (table_exacerbations[2, 2] / table_exacerbations[2, 1]) / (table_exacerbations[1, 2] / table_exacerbations[1, 1])
    log_odds_ratio <- log(odds_ratio)
    
    # Compute confidence intervals
    conf_interval <- exp(log_odds_ratio + c(-1.96 * sqrt(sum(1 / table_exacerbations))))
    conf_interval <- round(conf_interval, 2)
    
    # Print odds ratio and confidence interval
    cat("Odds Ratio:", round(odds_ratio, 2), "\n")
    cat("95% Confidence Interval:", conf_interval[1], "-", conf_interval[2], "\n")
    
    # Interpret the confidence interval
    if (all(!is.na(conf_interval)) && length(conf_interval) == 2) {
        if (conf_interval[1] > 1) {
            interpretation <- "The odds of exacerbations are greater for Drug A compared to Drug B."
        } else if (conf_interval[2] < 1) {
            interpretation <- "The odds of exacerbations are lower for Drug A compared to Drug B."
        } else {
            interpretation <- "The odds ratio is not significant, indicating no clear difference between the two drugs."
        }
        
        cat("Interpretation:", interpretation, "\n")
    } else {
        cat("Confidence interval could not be calculated due to missing values or an unexpected length.\n")
    }
} else {
    cat("Cannot calculate odds ratio and confidence interval because one or more cells in the contingency table are zero.\n")
}

# Set parameters
power <- 0.85                   # Desired power
alpha <- 0.05                   # Significance level
p1 <- 0.54                      # Proportion in DRUG A group
p2 <- 0.37                      # Proportion in DRUG B group
dropout_rate <- 0.15            # Expected dropout rate

# Calculate the required sample size for each group
n_needed <- power.prop.test(p1 = p1, p2 = p2, power = power, sig.level = alpha, alternative = "two.sided")$n

# Adjust for dropouts
n_total <- ceiling(n_needed / (1 - dropout_rate))

# Print the required sample size
cat("Required sample size per group (without dropout):", ceiling(n_needed), "\n")
cat("Total required sample size (with dropout):", n_total, "\n")

# Evaluate if the target enrollment was met
# Let's assume a target enrollment variable (you can update this with the actual number)
actual_enrollment <- 200  # Example actual enrollment, replace with actual data
if (actual_enrollment >= n_total) {
    cat("The investigators met the target enrollment.\n")
} else {
    cat("The investigators did not meet the target enrollment.\n")
}

# Discussion about the FEV1_post outcome
cat("If the sample size had been based on the FEV1_post outcome, the required number of patients would likely be different. ")
cat("Typically, outcomes with smaller effect sizes or greater variability (like FEV1) would require a larger sample size to detect significant differences.")

# Load necessary library
library(dplyr)
library(knitr)

# Example data: Create a contingency table with hypothetical data
data <- data.frame(
    Drug = c(rep("Drug A", 100), rep("Drug B", 100)),
    Exposure = c(rep(c("Exposed", "Not Exposed"), each = 50, times = 2)),
    Disease = c(rep(c("Disease", "No Disease"), each = 25, times = 4))
)

# Create contingency table
contingency_table <- table(data$Exposure, data$Disease, data$Drug)
print(contingency_table)

# Calculate Odds Ratio for Exposure
or_exposure <- (contingency_table[2, 1, "Drug A"] / contingency_table[1, 1, "Drug A"]) /
                (contingency_table[2, 1, "Drug B"] / contingency_table[1, 1, "Drug B"])

# Calculate Odds Ratio for Disease
or_disease <- (contingency_table[1, 2, "Drug A"] / contingency_table[1, 1, "Drug A"]) /
               (contingency_table[1, 2, "Drug B"] / contingency_table[1, 1, "Drug B"])

# Print Odds Ratios
cat("Odds Ratio for Exposure (Drug A vs Drug B):", or_exposure, "\n")
cat("Odds Ratio for Disease (Drug A vs Drug B):", or_disease, "\n")

# Example of Table 1: Summary statistics for each drug group
summary_table1 <- data %>%
    group_by(Drug) %>%
    summarise(
        Exposed = sum(Exposure == "Exposed"),
        Not_Exposed = sum(Exposure == "Not Exposed"),
        Disease = sum(Disease == "Disease"),
        No_Disease = sum(Disease == "No Disease")
    )

# Create Table 1
kable(summary_table1, caption = "Table 1: Summary of Exposure and Disease by Drug Group")

# Example of Table 2: Report means and standard deviations for numeric variables (if applicable)
# Here we'll create some hypothetical numeric data
set.seed(123)
data_numeric <- data %>%
    mutate(Value = rnorm(n(), mean = ifelse(Drug == "Drug A", 50, 55), sd = 10))

summary_table2 <- data_numeric %>%
    group_by(Drug) %>%
    summarise(
        Mean_Value = mean(Value),
        SD_Value = sd(Value)
    )

# Create Table 2
kable(summary_table2, caption = "Table 2: Summary of Numeric Variables by Drug Group")