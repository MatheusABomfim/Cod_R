library(genetics)
library(dplyr)
library(stringr)
library(epitools)
library(openxlsx)

# Read the CSV file (manual selection is performed)
df <- df_gc
print(df)

# Create the diplotype column
df$diplotype <- paste(df$rs10748643, df$rs11188513, sep = "/")

# Display the result
print(df)

# Create a contingency table between diplotypes and the outcome (HAM)
freq_diplo <- table(df$diplotype, df$HAM)

# Display the results
print("Frequency of Diplotypes:")
print(freq_diplo)

# Define diplotype groups by expression level
high_expression <- c("GG/TT", "GG/CT", "AG/TT")
medium_expression <- c("GG/CC", "AA/TT", "AG/CT") 
low_expression <- c("AG/CC", "AA/CT", "AA/CC")

# Assign the expression group correctly
df$expression_group <- ifelse(df$diplotype %in% high_expression, "High",
                              ifelse(df$diplotype %in% medium_expression, "Medium", "Low"))

# Count grouped by 'expression_group' and 'HAM'
count_expression_ham <- df %>%
  group_by(expression_group, HAM) %>%
  summarise(count = n())

# Display the result
print(count_expression_ham)

# Filter for two HAM categories (1 and 0) -> Iteratively modify; 1 = HAM, 0 = ASS/OLIGO, 3 = GC
df_filtrado <- df %>% filter(HAM %in% c(0, 1))

# Create a 3x2 contingency table
tabela_3x2 <- table(df_filtrado$expression_group, df_filtrado$HAM)
print(tabela_3x2)

# Fisher's test for the 3x2 table
resultado_fisher <- fisher.test(tabela_3x2)
print(resultado_fisher)

# Compare groups pairwise (High vs Medium, High vs Low, Medium vs Low)

# Comparison 1: High vs Medium
alta_media <- df_filtrado %>%
  filter(expression_group %in% c("High", "Medium")) %>%
  droplevels()

tabela_alta_media <- table(alta_media$expression_group, alta_media$HAM)
or_alta_media <- oddsratio(tabela_alta_media)
print(or_alta_media)

# Comparison 2: High vs Low
alta_baixa <- df_filtrado %>%
  filter(expression_group %in% c("High", "Low")) %>%
  droplevels()

tabela_alta_baixa <- table(alta_baixa$expression_group, alta_baixa$HAM)
or_alta_baixa <- oddsratio(tabela_alta_baixa)
print(or_alta_baixa)

# Comparison 3: Medium vs Low
media_baixa <- df_filtrado %>%
  filter(expression_group %in% c("Medium", "Low")) %>%
  droplevels()

tabela_media_baixa <- table(media_baixa$expression_group, media_baixa$HAM)
or_media_baixa <- oddsratio(tabela_media_baixa)
print(or_media_baixa)

# Evaluate GG/TT and GG/CT (example) --> Iteratively modify diplotypes and evaluated groups
# Filter only samples where HAM is 1 or 3
df_filtrado <- df[df$HAM %in% c(1, 0), ]

# Filter only GG/TT and GG/CT diplotypes in the filtered dataframe
subset_cont_table <- df_filtrado[df_filtrado$diplotype %in% c("GG/TT", "AG/TT"), ]

# Create the contingency table
contingency_table <- table(subset_cont_table$diplotype, subset_cont_table$HAM)

# Perform Fisher's test
fisher_test_contingency_table <- fisher.test(contingency_table)

# Extract p-value, Odds Ratio, and 95% CI
p_value_contingency_table <- fisher_test_contingency_table$p.value
odds_ratio_contigency_table <- fisher_test_contingency_table$estimate
ic95_contigency_table <- fisher_test_contingency_table$conf.int  # 95% CI

# Display the results
print("Contingency Table:")
print(contingency_table)
print(paste("P-value (Fisher's Test):", p_value_contingency_table))
print(paste("Odds Ratio:", odds_ratio_contigency_table))
print(paste("95% CI:", round(ic95_contigency_table[1], 3), "-", round(ic95_contigency_table[2], 3)))

# Contingency table: High expression vs Medium + Low expression (filtered by HAM grouping)
# Group Low and Medium into "Low/Medium"
df$expression_group_combined <- ifelse(df$expression_group %in% c("Low", "Medium"), "Low/Medium", df$expression_group)

# Filter genotypes and expression for analysis (selected high-expression diplotypes)
subset_expression_HAM0_HAM1 <- df %>% 
  filter(diplotype %in% c("GG/TT", "GG/CT", "AG/TT", "GG/CC", "AA/TT", "AG/CT", "AG/CC", "AA/CT", "AA/CC") & HAM %in% c(1, 3))  # Filtering groups here

# Create a contingency table by combined expression and HAM
contingency_table_expression_HAM <- table(subset_expression_HAM0_HAM1$expression_group_combined, subset_expression_HAM0_HAM1$HAM)

# Perform Fisher's test
fisher_test_expression_HAM <- fisher.test(contingency_table_expression_HAM)

# Extract p-value and Odds Ratio
p_value_expression_HAM <- fisher_test_expression_HAM$p.value
odds_ratio_expression_HAM <- fisher_test_expression_HAM$estimate

# Calculate 95% confidence interval
conf_int_expression_HAM <- fisher_test_expression_HAM$conf.int

# Display the results
print("Contingency Table by Combined Expression (Low/Medium) and HAM:")
print(contingency_table_expression_HAM)
print(paste("P-value (Fisher's Test):", p_value_expression_HAM))
print(paste("Odds Ratio:", odds_ratio_expression_HAM))
print(paste("95% CI:", conf_int_expression_HAM[1], "to", conf_int_expression_HAM[2]))
print(colnames(df))

# Group diplotypes for evaluating haplotypes in sets (heterozygotes with homozygotes) e.g., GG/AG as one category "GG_AG" and AA as "AA"
# Iteratively modify diplotypes of interest
# Function to group diplotypes and perform Fisher's test
analyze_diplotype <- function(df, diplotype_col, ham_col, group1, group2, ham_values) {
  # Create a new grouped column based on diplotypes of interest
  df$diplotype_grouped <- ifelse(df[[diplotype_col]] %in% group1, group1[1], group2[1])
  
  # Filter data based on specific HAM values
  subset_df <- df[df$diplotype_grouped %in% c(group1[1], group2[1]) & df[[ham_col]] %in% ham_values, ]
  
  # Create the contingency table
  contingency_table <- table(subset_df$diplotype_grouped, subset_df[[ham_col]])
  
  # Perform Fisher's test
  fisher_test <- fisher.test(contingency_table)
  
  # Extract p-value, Odds Ratio, and confidence interval
  p_value <- fisher_test$p.value
  odds_ratio <- fisher_test$estimate
  conf_int <- fisher_test$conf.int
  
  # Display the results
  print("Contingency Table:")
  print(contingency_table)
  print(paste("P-value (Fisher's Test):", p_value))
  print(paste("Odds Ratio:", odds_ratio))
  print(paste("95% CI:", conf_int[1], "to", conf_int[2]))
  
  # Return results as a list (optional)
  return(list(
    contingency_table = contingency_table,
    p_value = p_value,
    odds_ratio = odds_ratio,
    conf_int = conf_int
  ))
}

result <- analyze_diplotype(
  df, 
  diplotype_col = "rs10748643", 
  ham_col = "HAM", 
  group1 = c("GG", "AG"), 
  group2 = c("AA"), 
  ham_values = c(1, 3)
)

# Function to count alleles by group (HAM) with selection of the desired value
count_alleles_by_group <- function(df, genotype_col, group_col, ham_value) {
  # Filter dataframe for the desired HAM value
  df_group <- df[df[[group_col]] == ham_value, ]
  
  # Create a list to store results
  allele_counts_list <- list()
  
  # Extract the genotype column and split alleles into a list
  alleles <- unlist(strsplit(paste(df_group[[genotype_col]], collapse = ""), ""))
  
  # Count allele frequencies
  allele_counts <- table(alleles)
  
  # Store results in a dataframe with group (HAM) information
  allele_counts_df <- as.data.frame(allele_counts)
  allele_counts_df$Group <- ham_value
  
  # Display results
  print(paste("Allele Counts for HAM =", ham_value, ":"))
  print(allele_counts_df)
  
  return(allele_counts_df)  # Return dataframe with allele counts
}

# Example usage of the function for HAM = 1
# Iteratively modify evaluated column and HAM grouping
allele_count_result_HAM1 <- count_alleles_by_group(df, genotype_col = "rs10748643", group_col = "HAM", ham_value = 1)
