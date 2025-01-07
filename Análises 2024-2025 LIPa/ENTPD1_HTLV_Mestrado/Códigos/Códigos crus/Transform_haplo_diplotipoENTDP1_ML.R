library(genetics)
library(dplyr)
library(stringr)
library(epitools)
library(openxlsx)

# Ler o arquivo CSV (manualmente faço a seleção)
df <- cat1_df_clinic_ENTPD1
print(df)

# Criar a coluna de diplótipos
df$diplotype <- paste(df$rs10748643, df$rs11188513, sep = "/")
