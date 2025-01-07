library(genetics)
library(dplyr)
library(stringr)
library(epitools)
library(openxlsx)

# Testing Hardy-Weinberg equilibrium for the SNPs
# Create genotype counts from the 'df' dataframe
genotipos_rs107 <- c(rep("A/A", sum(df$rs10748643 == "AA")), 
                     rep("A/G", sum(df$rs10748643 == "AG")), 
                     rep("G/G", sum(df$rs10748643 == "GG")))

# Convert the genotypes into a "genotype" object
genotype_data_rs107 <- genotype(genotipos_rs107)

# Perform the Hardy-Weinberg equilibrium test
resultado_HW_rs107 <- HWE.chisq(genotype_data_rs107)

# Create genotype counts from the 'df' dataframe
genotipos_rs111 <- c(rep("T/T", sum(df$rs11188513 == "TT")), 
                     rep("C/T", sum(df$rs11188513 == "CT")), 
                     rep("C/C", sum(df$rs11188513 == "CC")))

# Convert the genotypes into a "genotype" object
genotype_data_rs111 <- genotype(genotipos_rs111)

# Perform the Hardy-Weinberg equilibrium test
resultado_HW_rs111 <- HWE.chisq(genotype_data_rs111)

# Display the results of the HW equilibrium test
print(resultado_HW_rs107)
print(resultado_HW_rs111)
