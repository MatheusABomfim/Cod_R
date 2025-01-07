library(genetics)
library(dplyr)
library(stringr)
library(epitools)

# Read the CSV file (manual selection performed)
df <- df_gc
print(df)

# Create a new variable 'PVHTLV' based on the values of 'HAM'
df$PVHTLV <- ifelse(df$HAM %in% c(0, 1), "Ass/oligo", ifelse(df$HAM == 3, "GC", NA))

# Check the resulting table
table(df$PVHTLV)

# Function to perform binary logistic regression iteratively
run_logistic_regression <- function(df, genotype_col, ham_col, expression_group, age_col, ham_values = c(1, 0)) {
  
  # Ensure "High" is the reference
  df$expression_group <- factor(df$expression_group, levels = c("High", "Medium", "Low"))
  
  # Filter the data for the desired HAM values
  df_filtered <- subset(df, df[[ham_col]] %in% ham_values)
  
  # Check if there is enough data after filtering
  if (nrow(df_filtered) < 2) {
    stop("Not enough data after filtering. Check the HAM values and expression group.")
  }
  
  # Ensure 'High' is the reference in the model
  df_filtered$expression_group <- factor(df_filtered$expression_group, levels = c("High", "Medium", "Low"))
  
  # Fit the logistic regression model
  formula <- as.formula(paste(ham_col, "~ expression_group +", age_col))
  model <- glm(formula, data = df_filtered, family = binomial)
  
  # Get Odds Ratios (OR) and confidence intervals
  exp_coef <- exp(coef(model))  # Odds Ratios
  conf_int <- exp(confint(model))  # 95% Confidence Intervals
  
  # Create a results table
  result <- data.frame(
    Variable = c("Intercept", "Medium", "Low", "Age"),
    OR = c(exp_coef[1], exp_coef[2], exp_coef[3], exp_coef[4]),
    IC_95_Lower = c(conf_int[1, 1], conf_int[2, 1], conf_int[3, 1], conf_int[4, 1]),
    IC_95_Upper = c(conf_int[1, 2], conf_int[2, 2], conf_int[3, 2], conf_int[4, 2]),
    p_value = summary(model)$coefficients[, 4]
  )
  
  # Manually add the 'High' reference
  result <- rbind(
    data.frame(Variable = "High (reference)", OR = 1, IC_95_Lower = NA, IC_95_Upper = NA, p_value = NA),
    result
  )
  
  # Display the results table
  print(result)
  
  return(result)  # Return the model results
}

# Example of how to pass expression groups to the function
expression_group <- list(
  high_expression = c("GG/TT", "GG/CT", "AG/TT"),  # Example of diplotypes for high expression
  medium_exp = c("GG/CC", "AA/TT", "AG/CT"),  # Example of diplotypes for medium expression
  low_exp = c("AG/CC", "AA/CT", "AA/CC")  # Example of diplotypes for low expression
)

# Run the model for different HAM values (1 and 3)
result_HAM_1_3 <- run_logistic_regression(df, genotype_col = "rs10748643", ham_col = "HAM", expression_group = expression_group, age_col = "Age", ham_values = c(1, 0))
