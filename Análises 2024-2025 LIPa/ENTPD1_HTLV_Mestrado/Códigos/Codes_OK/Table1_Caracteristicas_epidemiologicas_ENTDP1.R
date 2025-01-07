library(genetics)
library(dplyr)
library(stringr)

# Read the CSV file (selection is done manually)
df <- df_gc
print(df)

# Count the number of people with 0 and 1 in the HAM/TSP column
counts <- table(df$`HAM`)

# Display the counts
print(counts)

# Calculate median and IQR for HAM/TSP and Non-HAM/TSP groups
age_result <- df %>%
  group_by(HAM) %>%  # Group by values in the HAM/TSP column
  summarise(
    Median = median(Idade, na.rm = TRUE),             # Calculate the median age
    Q1 = quantile(Idade, 0.25, na.rm = TRUE),         # Calculate the 1st quartile (25%)
    Q3 = quantile(Idade, 0.75, na.rm = TRUE),         # Calculate the 3rd quartile (75%)
    IQR = round(Q3 - Q1, 1)                           # Calculate the interquartile range (IQR)
  ) %>%
  mutate(`Median Age (IQR)` = paste0(Median, " (IQR: ", IQR, ")"))  # Format the result with median and IQR

# Calculate median and IQR for HAM/TSP and Non-HAM/TSP groups by gender
age_gender_result <- df %>%
  group_by(Gênero, HAM) %>%  # Group by Gender and HAM (1 = HAM/TSP, 0 = NON-HAM/TSP)
  summarise(
    Mean_Age = round(mean(Idade, na.rm = TRUE), 1),   # Calculate the mean age
    Median = median(Idade, na.rm = TRUE),             # Calculate the median age
    Q1 = quantile(Idade, 0.25, na.rm = TRUE),         # Calculate the 1st quartile (25%)
    Q3 = quantile(Idade, 0.75, na.rm = TRUE),         # Calculate the 3rd quartile (75%)
    IQR = round(Q3 - Q1, 1),                          # Calculate the interquartile range (IQR)
    Total_Individuals = n(),                           # Count the total number of individuals in each group
    .groups = "drop"                                   # Remove grouping after summarise
  ) %>%
  group_by(Gênero) %>%  # Group again by Gender to calculate the percentage
  mutate(
    Percentage = round(Total_Individuals / sum(Total_Individuals) * 100, 1)  # Calculate the percentage within each Gender
  ) %>%
  ungroup() %>%  # Remove the final grouping
  mutate(
    `Age (Mean and IQR)` = paste0("Mean: ", Mean_Age, " | IQR: ", IQR)  # Format the column with Mean and IQR
  )

# View age results
print(age_result)
print(age_gender_result)

# Count individuals by gender and HAM outcome
gender_ham_count <- df %>%
  group_by(Gênero, HAM) %>%
  summarise(
    count = n()  # Count the number of individuals in each group
  )

# Display the count result
print(gender_ham_count)
