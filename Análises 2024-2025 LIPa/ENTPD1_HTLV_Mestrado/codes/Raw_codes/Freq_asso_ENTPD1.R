# Certifique-se de instalar e carregar o pacote genetics
install.packages("genetics")
install.packages("epitools")
library(genetics)
library(dplyr)
library(stringr)
library(epitools)

# Ler o arquivo CSV (manualmente faço a seleção)
df <- Estudo_alelos_ENTPD1_HAM_TSP
print(df)

# Renomear a coluna "HAM/TSP" para "HAM"
colnames(df)[colnames(df) == "HAM/TSP"] <- "HAM"

# Avaliação da frequência dos alelos por desfecho
  # Avaliação para rs10748643
freq_rs107 <- df %>%
  mutate(alleles_A = str_count(rs10748643, "A"),  # Contagem de A
         alleles_G = str_count(rs10748643, "G")) %>% # Contagem de G
  group_by(HAM) %>%
  summarise(
    count_A = sum(alleles_A),  # Contagem total de A por grupo HAM
    count_G = sum(alleles_G),  # Contagem total de G por grupo HAM
    total_alleles = sum(alleles_A) + sum(alleles_G),  # Total de alelos (A + G) por grupo
    prop_A = sum(alleles_A) / total_alleles,  # Proporção de A
    prop_G = sum(alleles_G) / total_alleles   # Proporção de G
  )

  # Avaliação da contagem dos haplótipos por desfecho (rs10748643)
resultados_haplotipos_107 <- df %>%
  mutate(
    haplotype_AA = ifelse(rs10748643 == "AA", 1, 0),  # Contagem de haplótipo AA
    haplotype_AG = ifelse(rs10748643 == "AG", 1, 0),  # Contagem de haplótipo AG
    haplotype_GG = ifelse(rs10748643 == "GG", 1, 0)   # Contagem de haplótipo GG
  ) %>%
  group_by(HAM) %>%
  summarise(
    count_AA = sum(haplotype_AA),  # Contagem total de AA por grupo HAM
    count_AG = sum(haplotype_AG),  # Contagem total de AG por grupo HAM
    count_GG = sum(haplotype_GG),  # Contagem total de GG por grupo HAM
    total_haplotypes = count_AA + count_AG + count_GG,  # Total de haplótipos por grupo
    prop_AA = count_AA / total_haplotypes,  # Proporção de AA
    prop_AG = count_AG / total_haplotypes,  # Proporção de AG
    prop_GG = count_GG / total_haplotypes   # Proporção de GG
  )

# Exibir resultados para rs10748643
print(freq_rs107)
print(resultados_haplotipos_107)

# Avaliação da frequência dos alelos por desfecho
# Avaliação para rs11188513
freq_rs111 <- df %>%
  mutate(alleles_C = str_count(rs11188513, "C"),  # Contagem de A
         alleles_T = str_count(rs11188513, "T")) %>% # Contagem de G
  group_by(HAM) %>%
  summarise(
    count_C = sum(alleles_C),  # Contagem total de C por grupo HAM
    count_T = sum(alleles_T),  # Contagem total de T por grupo HAM
    total_alleles = sum(alleles_C) + sum(alleles_T),  # Total de alelos (C + T) por grupo
    prop_C = sum(alleles_C) / total_alleles,  # Proporção de A
    prop_T = sum(alleles_T) / total_alleles   # Proporção de G
  )

# Avaliação da contagem dos haplótipos por desfecho (rs11188513)
resultados_haplotipos_rs111 <- df %>%
  mutate(
    haplotype_CC = ifelse(rs11188513 == "CC", 1, 0),  # Contagem de haplótipo AA
    haplotype_CT = ifelse(rs11188513 == "CT", 1, 0),  # Contagem de haplótipo AG
    haplotype_TT = ifelse(rs11188513 == "TT", 1, 0)   # Contagem de haplótipo GG
  ) %>%
  group_by(HAM) %>%
  summarise(
    count_CC = sum(haplotype_CC),  # Contagem total de AA por grupo HAM
    count_CT = sum(haplotype_CT),  # Contagem total de AG por grupo HAM
    count_TT = sum(haplotype_TT),  # Contagem total de GG por grupo HAM
    total_haplotypes = count_CC + count_CT + count_TT,  # Total de haplótipos por grupo
    prop_CC = count_CC / total_haplotypes,  # Proporção de AA
    prop_CT = count_CT / total_haplotypes,  # Proporção de AG
    prop_TT = count_TT / total_haplotypes   # Proporção de GG
  )

# Exibir resultados para rs10748643
print(freq_rs111)
print(resultados_haplotipos_rs111)


# Criando uma nova variável que agrupa os genótipos AA e AG como 'A', e GG como 'G'
df$alelo107A <- ifelse(df$rs10748643 == "AA" | df$rs10748643 == "AG", "A", 
                   ifelse(df$rs10748643 == "GG", "G", ""))

# Criando uma nova variável que agrupa os genótipos GG e AG como 'G', e AA como 'A'
df$alelo107G <- ifelse(df$rs10748643 == "GG" | df$rs10748643 == "AG", "G", 
                       ifelse(df$rs10748643 == "AA", "A", ""))

# Criando uma nova variável que agrupa os genótipos CC e CT como 'C', e TT como 'T'
df$alelo111C <- ifelse(df$rs11188513 == "CC" | df$rs11188513 == "CT", "C", 
                      ifelse(df$rs11188513 == "TT", "T", ""))


# Criando uma nova variável que agrupa os genótipos TT e CT como 'T', e CC como 'C'
df$alelo111T <- ifelse(df$rs11188513 == "TT" | df$rs11188513 == "CT", "T", 
                       ifelse(df$rs11188513 == "CC", "C", ""))


#codificação dos SNPs em numéricos
df$rs10748643_numeric <- ifelse(df$rs10748643 == "GG", 0,
                                ifelse(df$rs10748643 == "AG", 1, 2))
df$rs11188513_numeric <- ifelse(df$rs11188513 == "TT", 0,
                                ifelse(df$rs11188513 == "CT", 1, 2))

print(colnames(df))

# Teste de Shapiro-Wilk para variáveis dependentes
shapiro_test_snp1 <- shapiro.test(df$rs10748643_numeric)
shapiro_test_snp2 <- shapiro.test(df$rs11188513_numeric)
shapiro_test_idade <- shapiro.test(df$Idade)

# Teste de Shapiro-Wilk variável independente
shapiro_test_ham <- shapiro.test(df$`HAM/TSP`)

# Função para exibir a interpretação dos resultados, incluindo W e p-value
check_normality <- function(test_result) {
  W_value <- test_result$statistic
  p_value <- test_result$p.value
  
  if (p_value < 0.05) {
    result <- paste("Os dados NÃO seguem uma distribuição normal (p < 0.05).")
  } else {
    result <- paste("Os dados seguem uma distribuição normal (p >= 0.05).")
  }
  
  return(paste(result, "\nValor de W:", round(W_value, 4), "\np-value:",p_value))
}

# Imprimir resultados
cat("Resultado para rs10748643_numeric:\n")
cat(check_normality(shapiro_test_snp1), "\n\n")

cat("Resultado para rs11188513_numeric:\n")
cat(check_normality(shapiro_test_snp2), "\n")

cat("Resultado para idade:\n")
cat(check_normality(shapiro_test_idade), "\n\n")

cat("Resultado para variável independente:\n")
cat(check_normality(shapiro_test_ham), "\n")

# Criação de tabelas de contigência 
tab_cont_rs10748643 <- table(df$rs10748643, df$`HAM/TSP`)
print(tab_cont_rs10748643)

tab_cont_rs11188513 <- table(df$rs11188513, df$`HAM/TSP`)
print(tab_cont_rs11188513)

# Criando a tabela de contingência com apenas os alelos A e G em relação ao status de HAM/TSP
tab_alelors107A <- table(df$alelo107A, df$`HAM`)
tab_alelors107G <- table(df$alelo107G, df$`HAM`)
tab_alelors111C <- table(df$alelo111C, df$`HAM`)
tab_alelors111T <- table(df$alelo111T, df$`HAM`)

# Exibindo as tabelas (dominantes e recessivos)
print(tab_alelors107A) # AG OU AA representado por A e GG representado por G
print(tab_alelors107G) # AG OU GG representado por G e AA representado por A
print(tab_alelors111C) # CC OU CT representado por C e TT representado por T
print(tab_alelors111T) # TT OU CT representado por T e CC representado por C

# Aplicação teste de fisher HAM vs Oligo/ass
  # rs107 modelos dominantes
fisher.test(tab_alelors107A)
fisher.test(tab_alelors107G)
  # rs107 por haplótipos
fisher.test(tab_cont_rs10748643)
OR_rs10748643 <- oddsratio(tab_cont_rs10748643, method = "fisher")
  print(OR_rs10748643)
  
  # rs111
fisher.test(tab_alelors111C)
fisher.test(tab_alelors111T)
fisher.test(tab_cont_rs11188513)
  # rs111 por haplótipos
fisher.test(tab_cont_rs11188513)
OR_rs11188513 <- oddsratio(tab_cont_rs11188513, method = "fisher")
print(OR_rs11188513)

# Aplicação teste de fisher HAM vs Grupo controle
  # Criação das tabelas de contigência com o GC vs HAM
    # rs107
      #107A_GC
tabela_haplotipos_107A_GC <- matrix(c(45, 24, 13, 5), 
                            nrow = 2, 
                            ncol = 2, 
                            dimnames = list("Haplótipo" = c("AG+AA", "GG"), 
                                            "Clínica" = c("GC", "HAM")))
print(tabela_haplotipos_107A_GC)
      #107G_GC
tabela_haplotipos_107G_GC <- matrix(c(58, 11, 15, 3), 
                                    nrow = 2, 
                                    ncol = 2, 
                                    dimnames = list("Haplótipo" = c("AG+GG", "AA"), 
                                                    "Clínica" = c("GC", "HAM")))
print(tabela_haplotipos_107G_GC)
      #107_genotipos

tabela_107_genotipos <- matrix(c(3, 10, 5,   
                                 11, 34, 24),    
                     nrow = 3,     # Número de linhas (3 haplótipos)
                     ncol = 2,     # Número de colunas (3 grupos)
                     dimnames = list("Haplótipo" = c("AA", "AG", "GG"), 
                                     "Grupo" = c("HAM", "GC")))

print(tabela_107_genotipos)


    # rs111
tabela_haplotipos_111C_GC <- matrix(c(36,33 , 11, 7), 
                                    nrow = 2, 
                                    ncol = 2, 
                                    dimnames = list("Haplótipo" = c("AG+AA", "GG"), 
                                                    "Clínica" = c("GC", "HAM")))
print(tabela_haplotipos_111C_GC)

tabela_haplotipos_111T_GC <- matrix(c(64,5, 18, 0), 
                                    nrow = 2, 
                                    ncol = 2, 
                                    dimnames = list("Haplótipo" = c("AG+AA", "GG"), 
                                                    "Clínica" = c("GC", "HAM")))
print(tabela_haplotipos_111T_GC)
      #111_genotipos
tabela_111_genotiposC_GC <- matrix(c(0, 11, 7,   
                                 5, 31, 33),    
                               nrow = 3,     # Número de linhas (3 haplótipos)
                               ncol = 2,     # Número de colunas (3 grupos)
                               dimnames = list("Haplótipo" = c("CC", "CT", "TT"), 
                                               "Grupo" = c("HAM", "GC")))

print(tabela_111_genotiposC_GC)
  

 # Fisher para o 107
fisher.test(tabela_haplotipos_107A_GC)
fisher.test(tabela_haplotipos_107G_GC)
  # rs107 por haplótipos vs GC
fisher.test(tabela_107_genotipos)
OR_107_genotiposC_GC <- oddsratio(tabela_107_genotipos, method = "fisher")
print(OR_107_genotiposC_GC )

  
  # Fisher para o 111
fisher.test(tabela_haplotipos_111C_GC)
fisher.test(tabela_haplotipos_111T_GC)
    # rs111 por haplótipos vs GC
fisher.test(tabela_111_genotiposC_GC)
OR_111_genotiposC_GC <- oddsratio(tabela_111_genotiposC_GC, method = "fisher")
print(OR_111_genotiposC_GC )


# Avaliação da correlação (Spearman)
cor.test(df$rs10748643_numeric, df$Idade, method = "spearman")
cor.test(df$rs11188513_numeric, df$Idade, method = "spearman")


