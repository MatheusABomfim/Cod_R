library(genetics)
library(dplyr)
library(stringr)
library(epitools)
library(openxlsx)

# Ler o arquivo CSV (manualmente faço a seleção)
df <- df_gc
print(df)

# Criar a coluna de diplótipos
df$diplotype <- paste(df$rs10748643, df$rs11188513, sep = "/")

# Visualizar o resultado
print(df)

# Criar uma tabela de contingência entre diplótipos e o desfecho (HAM)
contingency_table <- table(df$diplotype, df$HAM)

# Realizar o teste exato de Fisher
fisher_test <- fisher.test(contingency_table)

# Extrair o p-valor com todos diplótipos sem categorização
p_value <- fisher_test$p.value

# Exibir os resultados
print("Tabela de contingência:")
print(contingency_table)
print(paste("P-valor (Teste de Fisher):", p_value))

# Definir os grupos de diplótipos por expressão
high_expression <- c("GG/TT", "GG/CT", "AG/TT")
medium_expression <- c("GG/CC", "AA/TT", "AG/CT") 
low_expression <- c("AG/CC", "AA/CT", "AA/CC")

# Atribuir o grupo de expressão corretamente
df$expression_group <- ifelse(df$diplotype %in% high_expression, "Alta",
                              ifelse(df$diplotype %in% medium_expression, "Média", "Baixa"))

# Calcular contagem agrupada por 'expression_group' e 'HAM'
contagem_expressao_ham <- df %>%
  group_by(expression_group, HAM) %>%
  summarise(contagem = n())

# Visualizar o resultado
print(contagem_expressao_ham)

# Calcuar contagem agrupada por diplótipos avaliados agrupados por HAM
diplo_count <- df %>%
  group_by(df$diplotype, HAM) %>%
  summarise(contagem = n())
print(diplo_count)

# Filtrar para duas categorias de HAM (1 e 0)
df_filtrado <- df %>% filter(HAM %in% c(0, 1))

# Criar tabela de contingência 3x2
tabela_3x2 <- table(df_filtrado$expression_group, df_filtrado$HAM)
print(tabela_3x2)

# Teste de Fisher para tabela 3x2
resultado_fisher <- fisher.test(tabela_3x2)
print(resultado_fisher)

# Comparar grupos por pares (Alta vs Média, Alta vs Baixa, Média vs Baixa)

# Comparação 1: Alta vs Média
alta_media <- df_filtrado %>%
  filter(expression_group %in% c("Alta", "Média")) %>%
  droplevels()

tabela_alta_media <- table(alta_media$expression_group, alta_media$HAM)
or_alta_media <- oddsratio(tabela_alta_media)
print(or_alta_media)

# Comparação 2: Alta vs Baixa
alta_baixa <- df_filtrado %>%
  filter(expression_group %in% c("Alta", "Baixa")) %>%
  droplevels()

tabela_alta_baixa <- table(alta_baixa$expression_group, alta_baixa$HAM)
or_alta_baixa <- oddsratio(tabela_alta_baixa)
print(or_alta_baixa)

# Comparação 3: Média vs Baixa
media_baixa <- df_filtrado %>%
  filter(expression_group %in% c("Média", "Baixa")) %>%
  droplevels()

tabela_media_baixa <- table(media_baixa$expression_group, media_baixa$HAM)
or_media_baixa <- oddsratio(tabela_media_baixa)
print(or_media_baixa)

    # Filtrar os dados para incluir apenas as linhas com HAM igual a 1 ou 3
filtered_df_HAM_GC <- df[df$HAM %in% c(0, 3), ]

# Criar tabela de contingência
contingency_table_HAM_GC <- table(filtered_df_HAM_GC$expression_group, filtered_df_HAM_GC$HAM)

# Realizar o teste de Fisher
fisher_test <- fisher.test(contingency_table_HAM_GC)

# Extrair o p-valor e o Odds Ratio
p_value <- fisher_test$p.value
odds_ratio <- fisher_test$estimate

# Exibir os resultados
print("Tabela de Contingência:")
print(contingency_table)
print(paste("P-valor (Teste de Fisher):", p_value))
print(paste("Odds Ratio:", odds_ratio))

# Filtrar os dados para incluir apenas as linhas com HAM igual a 1 ou 3
filtered_df_HAM_GC <- df[df$HAM %in% c(1, 3), ]

# Criar tabela de contingência
contingency_table_HAM_GC <- table(filtered_df_HAM_GC$expression_group, filtered_df_HAM_GC$HAM)

# Realizar o teste de Fisher
fisher_test <- fisher.test(contingency_table_HAM_GC)

# Extrair o p-valor e o Odds Ratio
p_value <- fisher_test$p.value
odds_ratio <- fisher_test$estimate

# Exibir os resultados
print("Tabela de Contingência:")
print(contingency_table)
print(paste("P-valor (Teste de Fisher):", p_value))
print(paste("Odds Ratio:", odds_ratio))

  # Definir grupos de expressão
top_high_expression <- c("GG/TT")
df$top_high_expression <- ifelse(df$diplotype %in% top_high_expression, "Alta", "Média/Baixa")

# Criar tabela de contingência
contingency_table_top_high_exp <- table(df$top_high_expression, df$HAM)

# Realizar o teste de Fisher
fisher_test_top_high_exp <- fisher.test(contingency_table_top_high_exp)

# Extrair o p-valor e o Odds Ratio
p_value <- fisher_test_top_high_exp$p.value
odds_ratio <- fisher_test_top_high_exp$estimate

# Exibir os resultados
print("Tabela de Contingência:")
print(contingency_table_top_high_exp)
print(paste("P-valor (Teste de Fisher):", p_value))
print(paste("Odds Ratio:", odds_ratio))


  # Avaliação GG/TT e GG/CT
# Filtrar apenas os diplótipos GG/TT e GG/CT
subset_GGTT_GGCT <- df[df$diplotype %in% c("GG/TT", "GG/CT"), ]
print(contingency_table_GGTT_GGCT)

# Criar a tabela de contingência
contingency_table_GGTT_GGCT <- table(subset_GGTT_GGCT $diplotype, subset_GGTT_GGCT $HAM)

# Realizar o teste de Fisher
fisher_test_GGTT_GGCT <- fisher.test(contingency_table_GGTT_GGCT)

# Extrair o p-valor e o Odds Ratio
p_value_GGTT_GGCT <- fisher_test_GGTT_GGCT$p.value
odds_ratio_GGTT_GGCT <- fisher_test_GGTT_GGCT$estimate

# Exibir os resultados
print("Tabela de Contingência:")
print(contingency_table_GGTT_GGCT)
print(paste("P-valor (Teste de Fisher):", p_value_GGTT_GGCT))
print(paste("Odds Ratio:", odds_ratio_GGTT_GGCT))

  # Avaliação GG/TT e AG/TT
# Filtrar apenas os diplótipos GG/TT e AG/TT
subset_GGTT_AGTT <- df[df$diplotype %in% c("GG/TT", "AG/TT"), ]

# Criar a tabela de contingência
contingency_table_GGTT_AGTT <- table(subset_GGTT_AGTT$diplotype, subset_GGTT_AGTT$HAM)

# Verificar se a tabela possui linhas e colunas suficientes para o teste
if (nrow(contingency_table_GGTT_AGTT) >= 2 && ncol(contingency_table_GGTT_AGTT) >= 2) {
  # Realizar o teste de Fisher
  fisher_test_GGTT_AGTT <- fisher.test(contingency_table_GGTT_AGTT)
  
  # Extrair o p-valor e o Odds Ratio
  p_value_GGTT_AGTT <- fisher_test_GGTT_AGTT$p.value
  odds_ratio_GGTT_AGTT <- fisher_test_GGTT_AGTT$estimate
  
  # Exibir os resultados
  print("Tabela de Contingência (GG/TT vs AG/TT):")
  print(contingency_table_GGTT_AGTT)
  print(paste("P-valor (Teste de Fisher):", p_value_GGTT_AGTT))
  print(paste("Odds Ratio:", odds_ratio_GGTT_AGTT))
} else {
  # Mensagem de erro se a tabela não for válida
  print("A tabela de contingência (GG/TT vs AG/TT) não possui linhas e colunas suficientes para o teste de Fisher.")
  print("Tabela de Contingência:")
  print(contingency_table_GGTT_AGTT)
}

  # Avaliação GG/TT e AA/TT
# Filtrar apenas os diplótipos GG/TT e AA/TT
subset_GGTT_AATT <- df[df$diplotype %in% c("GG/TT", "AA/TT"), ]

# Criar a tabela de contingência
contingency_table_GGTT_AATT <- table(subset_GGTT_AATT$diplotype, subset_GGTT_AATT$HAM)

# Verificar se a tabela possui linhas e colunas suficientes para o teste
if (nrow(contingency_table_GGTT_AATT) >= 2 && ncol(contingency_table_GGTT_AATT) >= 2) {
  # Realizar o teste de Fisher
  fisher_test_GGTT_AATT <- fisher.test(contingency_table_GGTT_AATT)
  
  # Extrair o p-valor e o Odds Ratio
  p_value_GGTT_AATT <- fisher_test_GGTT_AATT$p.value
  odds_ratio_GGTT_AATT <- fisher_test_GGTT_AATT$estimate
  
  # Exibir os resultados
  print("Tabela de Contingência (GG/TT vs AA/TT):")
  print(contingency_table_GGTT_AATT)
  print(paste("P-valor (Teste de Fisher):", p_value_GGTT_AATT))
  print(paste("Odds Ratio:", odds_ratio_GGTT_AATT))
} else {
  # Mensagem de erro se a tabela não for válida
  print("A tabela de contingência (GG/TT vs AA/TT) não possui linhas e colunas suficientes para o teste de Fisher.")
  print("Tabela de Contingência:")
  print(contingency_table_GGTT_AATT)
}

  # # Avaliação GG/TT e AA/CT
# Filtrar apenas os diplótipos GG/TT e AA/TT
subset_GGTT_AATT <- df[df$diplotype %in% c("GG/TT", "AA/TT"), ]

# Criar a tabela de contingência
contingency_table_GGTT_AATT <- table(subset_GGTT_AATT$diplotype, subset_GGTT_AATT$HAM)

# Verificar se a tabela possui linhas e colunas suficientes para o teste
if (nrow(contingency_table_GGTT_AATT) >= 2 && ncol(contingency_table_GGTT_AATT) >= 2) {
  # Realizar o teste de Fisher
  fisher_test_GGTT_AATT <- fisher.test(contingency_table_GGTT_AATT)
  
  # Extrair o p-valor e o Odds Ratio
  p_value_GGTT_AATT <- fisher_test_GGTT_AATT$p.value
  odds_ratio_GGTT_AATT <- fisher_test_GGTT_AATT$estimate
  
  # Exibir os resultados
  print("Tabela de Contingência (GG/TT vs AA/TT):")
  print(contingency_table_GGTT_AATT)
  print(paste("P-valor (Teste de Fisher):", p_value_GGTT_AATT))
  print(paste("Odds Ratio:", odds_ratio_GGTT_AATT))
} else {
  # Mensagem de erro se a tabela não for válida
  print("A tabela de contingência (GG/TT vs AA/TT) não possui linhas e colunas suficientes para o teste de Fisher.")
  print("Tabela de Contingência:")
  print(contingency_table_GGTT_AATT)
}

# AVALIAÇÕES HAM vs OLIGO/ASS
  # GG/TT vs AA/TT
# Filtrar apenas os diplótipos GG/TT e AG/CT
subset_GGTT_AGCT <- df[df$diplotype %in% c("GG/TT", "AG/CT"), ]

# Criar a tabela de contingência
contingency_table_GGTT_AGCT <- table(subset_GGTT_AGCT$diplotype, subset_GGTT_AGCT$HAM)

# Verificar se a tabela possui linhas e colunas suficientes para o teste
if (nrow(contingency_table_GGTT_AGCT) >= 2 && ncol(contingency_table_GGTT_AGCT) >= 2) {
  # Realizar o teste de Fisher
  fisher_test_GGTT_AGCT <- fisher.test(contingency_table_GGTT_AGCT)
  
  # Extrair o p-valor e o Odds Ratio
  p_value_GGTT_AGCT <- fisher_test_GGTT_AGCT$p.value
  odds_ratio_GGTT_AGCT <- fisher_test_GGTT_AGCT$estimate
  
  # Exibir os resultados
  print("Tabela de Contingência (GG/TT vs AG/CT):")
  print(contingency_table_GGTT_AGCT)
  print(paste("P-valor (Teste de Fisher):", p_value_GGTT_AGCT))
  print(paste("Odds Ratio:", odds_ratio_GGTT_AGCT))
} else {
  # Mensagem de erro se a tabela não for válida
  print("A tabela de contingência (GG/TT vs AG/CT) não possui linhas e colunas suficientes para o teste de Fisher.")
  print("Tabela de Contingência:")
  print(contingency_table_GGTT_AGCT)
}

  # GG/TT vs AA/CT
# Filtrar apenas os diplótipos GG/TT e AA/CT
subset_GGTT_AACT <- df[df$diplotype %in% c("GG/TT", "AA/CT"), ]

# Criar a tabela de contingência
contingency_table_GGTT_AACT <- table(subset_GGTT_AACT$diplotype, subset_GGTT_AACT$HAM)

# Verificar se a tabela possui linhas e colunas suficientes para o teste
if (nrow(contingency_table_GGTT_AACT) >= 2 && ncol(contingency_table_GGTT_AACT) >= 2) {
  # Realizar o teste de Fisher
  fisher_test_GGTT_AACT <- fisher.test(contingency_table_GGTT_AACT)
  
  # Extrair o p-valor e o Odds Ratio
  p_value_GGTT_AACT <- fisher_test_GGTT_AACT$p.value
  odds_ratio_GGTT_AACT <- fisher_test_GGTT_AACT$estimate
  
  # Exibir os resultados
  print("Tabela de Contingência (GG/TT vs AA/CT):")
  print(contingency_table_GGTT_AACT)
  print(paste("P-valor (Teste de Fisher):", p_value_GGTT_AACT))
  print(paste("Odds Ratio:", odds_ratio_GGTT_AACT))
} else {
  # Mensagem de erro se a tabela não for válida
  print("A tabela de contingência (GG/TT vs AA/CT) não possui linhas e colunas suficientes para o teste de Fisher.")
  print("Tabela de Contingência:")
  print(contingency_table_GGTT_AACT)
}

  # GG/TT vs AA/CT
# Filtrar apenas os diplótipos GG/TT e AA/CT
subset_GGTT_AACT <- df[df$diplotype %in% c("GG/TT", "AA/CT"), ]

# Criar a tabela de contingência
contingency_table_GGTT_AACT <- table(subset_GGTT_AACT$diplotype, subset_GGTT_AACT$HAM)

# Verificar se a tabela possui linhas e colunas suficientes para o teste
if (nrow(contingency_table_GGTT_AACT) >= 2 && ncol(contingency_table_GGTT_AACT) >= 2) {
  # Realizar o teste de Fisher
  fisher_test_GGTT_AACT <- fisher.test(contingency_table_GGTT_AACT)
  
  # Extrair o p-valor e o Odds Ratio
  p_value_GGTT_AACT <- fisher_test_GGTT_AACT$p.value
  odds_ratio_GGTT_AACT <- fisher_test_GGTT_AACT$estimate
  
  # Exibir os resultados
  print("Tabela de Contingência (GG/TT vs AA/CT):")
  print(contingency_table_GGTT_AACT)
  print(paste("P-valor (Teste de Fisher):", p_value_GGTT_AACT))
  print(paste("Odds Ratio:", odds_ratio_GGTT_AACT))
} else {
  # Mensagem de erro se a tabela não for válida
  print("A tabela de contingência (GG/TT vs AA/CT) não possui linhas e colunas suficientes para o teste de Fisher.")
  print("Tabela de Contingência:")
  print(contingency_table_GGTT_AACT)
}

# AVALIAÇÕES HAM vs GC
  # GG/TT vs AG/TT
# Filtrar apenas os diplótipos GG/TT e GG/CT
subset_GGTT_GGCT <- df[df$diplotype %in% c("GG/TT", "GG/CT") & df$HAM %in% c(1, 3), ]

# Criar a tabela de contingência
contingency_table_GGTT_GGCT <- table(subset_GGTT_GGCT$diplotype, subset_GGTT_GGCT$HAM)

# Verificar se a tabela possui linhas e colunas suficientes para o teste
if (nrow(contingency_table_GGTT_GGCT) >= 2 && ncol(contingency_table_GGTT_GGCT) >= 2) {
  # Realizar o teste de Fisher
  fisher_test_GGTT_GGCT <- fisher.test(contingency_table_GGTT_GGCT)
  
  # Extrair o p-valor e o Odds Ratio
  p_value_GGTT_GGCT <- fisher_test_GGTT_GGCT$p.value
  odds_ratio_GGTT_GGCT <- fisher_test_GGTT_GGCT$estimate
  
  # Exibir os resultados
  print("Tabela de Contingência (GG/TT vs GG/CT para HAM 1 vs 3):")
  print(contingency_table_GGTT_GGCT)
  print(paste("P-valor (Teste de Fisher):", p_value_GGTT_GGCT))
  print(paste("Odds Ratio:", odds_ratio_GGTT_GGCT))
} else {
  # Mensagem de erro se a tabela não for válida
  print("A tabela de contingência não possui linhas e colunas suficientes para o teste de Fisher.")
  print("Tabela de Contingência:")
  print(contingency_table_GGTT_GGCT)
}
  # GG/TT vs AG/TT
# Filtrar apenas os diplótipos GG/TT e AG/TT e os desfechos HAM 1 e 3
subset_GGTT_AGTT <- df[df$diplotype %in% c("GG/TT", "AG/TT") & df$HAM %in% c(1, 3), ]

# Criar a tabela de contingência
contingency_table_GGTT_AGTT <- table(subset_GGTT_AGTT$diplotype, subset_GGTT_AGTT$HAM)

# Verificar se a tabela possui linhas e colunas suficientes para o teste
if (nrow(contingency_table_GGTT_AGTT) >= 2 && ncol(contingency_table_GGTT_AGTT) >= 2) {
  # Realizar o teste de Fisher
  fisher_test_GGTT_AGTT <- fisher.test(contingency_table_GGTT_AGTT)
  
  # Extrair o p-valor e o Odds Ratio
  p_value_GGTT_AGTT <- fisher_test_GGTT_AGTT$p.value
  odds_ratio_GGTT_AGTT <- fisher_test_GGTT_AGTT$estimate
  
  # Exibir os resultados
  print("Tabela de Contingência (GG/TT vs AG/TT para HAM 1 vs 3):")
  print(contingency_table_GGTT_AGTT)
  print(paste("P-valor (Teste de Fisher):", p_value_GGTT_AGTT))
  print(paste("Odds Ratio:", odds_ratio_GGTT_AGTT))
} else {
  # Mensagem de erro se a tabela não for válida
  print("A tabela de contingência não possui linhas e colunas suficientes para o teste de Fisher.")
  print("Tabela de Contingência:")
  print(contingency_table_GGTT_AGTT)
}

  # GG/CC vs AA/TT
# Filtrar apenas os diplótipos AG/TT e GG/CC e desfechos HAM 1 vs 3
subset_AGTT_GGTT_HAM1_3 <- df[df$diplotype %in% c("AG/TT", "GG/TT") & df$HAM %in% c(1, 3), ]

# Criar a tabela de contingência
contingency_table_AGTT_GGCC_HAM1_3 <- table(subset_AGTT_GGCC_HAM1_3$diplotype, subset_AGTT_GGCC_HAM1_3$HAM)
print(contingency_table_AGTT_GGCC_HAM1_3)
# Realizar o teste de Fisher
fisher_test_AGTT_GGCC_HAM1_3 <- fisher.test(contingency_table_AGTT_GGCC_HAM1_3)

# Extrair o p-valor e o Odds Ratio
p_value_AGTT_GGCC_HAM1_3 <- fisher_test_AGTT_GGCC_HAM1_3$p.value
odds_ratio_AGTT_GGCC_HAM1_3 <- fisher_test_AGTT_GGCC_HAM1_3$estimate

# Exibir os resultados
print("Tabela de Contingência (AG/TT vs GG/CC com HAM 1 vs 3):")
print(contingency_table_AGTT_GGCC_HAM1_3)
print(paste("P-valor (Teste de Fisher):", p_value_AGTT_GGCC_HAM1_3))
print(paste("Odds Ratio:", odds_ratio_AGTT_GGCC_HAM1_3))

  #GG/TT vs AG/TT
# Filtrar apenas os diplótipos AG/TT e GG/TT e desfechos HAM 1 vs 3
subset_AGTT_GGTT_HAM1_3 <- df[df$diplotype %in% c("AG/TT", "GG/TT") & df$HAM %in% c(1, 3), ]

# Criar a tabela de contingência
contingency_table_AGTT_GGTT_HAM1_3 <- table(subset_AGTT_GGTT_HAM1_3$diplotype, subset_AGTT_GGTT_HAM1_3$HAM)

# Realizar o teste de Fisher
fisher_test_AGTT_GGTT_HAM1_3 <- fisher.test(contingency_table_AGTT_GGTT_HAM1_3)

# Extrair o p-valor e o Odds Ratio
p_value_AGTT_GGTT_HAM1_3 <- fisher_test_AGTT_GGTT_HAM1_3$p.value
odds_ratio_AGTT_GGTT_HAM1_3 <- fisher_test_AGTT_GGTT_HAM1_3$estimate

# Exibir os resultados
print("Tabela de Contingência (AG/TT vs GG/TT com HAM 1 vs 3):")
print(contingency_table_AGTT_GGTT_HAM1_3)
print(paste("P-valor (Teste de Fisher):", p_value_AGTT_GGTT_HAM1_3))
print(paste("Odds Ratio:", odds_ratio_AGTT_GGTT_HAM1_3))
HAM1_3))
print(paste("Odds Ratio:", odds_ratio_GGCC_AATT_HAM1_3))

  # GG/TT vs AG/CT
# Filtrar apenas os diplótipos GG/TT e AG/CT e desfechos HAM 1 vs 3
subset_GGTT_AGCT_HAM1_3 <- df[df$diplotype %in% c("GG/TT", "AG/CT") & df$HAM %in% c(1, 3), ]

# Criar a tabela de contingência
contingency_table_GGTT_AGCT_HAM1_3 <- table(subset_GGTT_AGCT_HAM1_3$diplotype, subset_GGTT_AGCT_HAM1_3$HAM)

# Realizar o teste de Fisher
fisher_test_GGTT_AGCT_HAM1_3 <- fisher.test(contingency_table_GGTT_AGCT_HAM1_3)

# Extrair o p-valor e o Odds Ratio
p_value_GGTT_AGCT_HAM1_3 <- fisher_test_GGTT_AGCT_HAM1_3$p.value
odds_ratio_GGTT_AGCT_HAM1_3 <- fisher_test_GGTT_AGCT_HAM1_3$estimate

# Exibir os resultados
print("Tabela de Contingência (GG/TT vs AG/CT com HAM 1 vs 3):")
print(contingency_table_GGTT_AGCT_HAM1_3)
print(paste("P-valor (Teste de Fisher):", p_value_GGTT_AGCT_HAM1_3))
print(paste("Odds Ratio:", odds_ratio_GGTT_AGCT_HAM1_3))

# AVALIAÇÃO OLIGO/ASS vs GC
  # GG/TT vs AG/TT
# Filtrar apenas o genótipo GG/TT (HAM 0) e AG/TT (HAM 3)
subset_GGTT_HAM0_AGTT_HAM3 <- df[df$diplotype %in% c("GG/TT", "AG/TT") & df$HAM %in% c(0, 3), ]

# Criar a tabela de contingência
contingency_table_GGTT_HAM0_AGTT_HAM3 <- table(subset_GGTT_HAM0_AGTT_HAM3$diplotype, subset_GGTT_HAM0_AGTT_HAM3$HAM)

# Realizar o teste de Fisher
fisher_test_GGTT_HAM0_AGTT_HAM3 <- fisher.test(contingency_table_GGTT_HAM0_AGTT_HAM3)

# Extrair o p-valor e o Odds Ratio
p_value_GGTT_HAM0_AGTT_HAM3 <- fisher_test_GGTT_HAM0_AGTT_HAM3$p.value
odds_ratio_GGTT_HAM0_AGTT_HAM3 <- fisher_test_GGTT_HAM0_AGTT_HAM3$estimate

# Exibir os resultados
print("Tabela de Contingência (GG/TT com HAM 0 vs AG/TT com HAM 3):")
print(contingency_table_GGTT_HAM0_AGTT_HAM3)
print(paste("P-valor (Teste de Fisher):", p_value_GGTT_HAM0_AGTT_HAM3))
print(paste("Odds Ratio:", odds_ratio_GGTT_HAM0_AGTT_HAM3))

  # GG/TT vs AA/TT
# Filtrar apenas o genótipo GG/TT (HAM 0) e AA/TT (HAM 3)
subset_GGTT_HAM0_AATT_HAM3 <- df[df$diplotype %in% c("GG/TT", "AA/TT") & df$HAM %in% c(0, 3), ]

# Criar a tabela de contingência
contingency_table_GGTT_HAM0_AATT_HAM3 <- table(subset_GGTT_HAM0_AATT_HAM3$diplotype, subset_GGTT_HAM0_AATT_HAM3$HAM)

# Realizar o teste de Fisher
fisher_test_GGTT_HAM0_AATT_HAM3 <- fisher.test(contingency_table_GGTT_HAM0_AATT_HAM3)

# Extrair o p-valor e o Odds Ratio
p_value_GGTT_HAM0_AATT_HAM3 <- fisher_test_GGTT_HAM0_AATT_HAM3$p.value
odds_ratio_GGTT_HAM0_AATT_HAM3 <- fisher_test_GGTT_HAM0_AATT_HAM3$estimate

# Exibir os resultados
print("Tabela de Contingência (GG/TT com HAM 0 vs AA/TT com HAM 3):")
print(contingency_table_GGTT_HAM0_AATT_HAM3)
print(paste("P-valor (Teste de Fisher):", p_value_GGTT_HAM0_AATT_HAM3))
print(paste("Odds Ratio:", odds_ratio_GGTT_HAM0_AATT_HAM3))

  # GG/TT vs AG/CT
# Filtrar apenas o genótipo GG/TT (HAM 0) e AG/CT (HAM 3)
subset_GGTT_HAM0_AGCT_HAM3 <- df[df$diplotype %in% c("GG/TT", "AG/CT") & df$HAM %in% c(0, 3), ]

# Criar a tabela de contingência
contingency_table_GGTT_HAM0_AGCT_HAM3 <- table(subset_GGTT_HAM0_AGCT_HAM3$diplotype, subset_GGTT_HAM0_AGCT_HAM3$HAM)

# Realizar o teste de Fisher
fisher_test_GGTT_HAM0_AGCT_HAM3 <- fisher.test(contingency_table_GGTT_HAM0_AGCT_HAM3)

# Extrair o p-valor e o Odds Ratio
p_value_GGTT_HAM0_AGCT_HAM3 <- fisher_test_GGTT_HAM0_AGCT_HAM3$p.value
odds_ratio_GGTT_HAM0_AGCT_HAM3 <- fisher_test_GGTT_HAM0_AGCT_HAM3$estimate

# Exibir os resultados
print("Tabela de Contingência (GG/TT com HAM 0 vs AG/CT com HAM 3):")
print(contingency_table_GGTT_HAM0_AGCT_HAM3)
print(paste("P-valor (Teste de Fisher):", p_value_GGTT_HAM0_AGCT_HAM3))
print(paste("Odds Ratio:", odds_ratio_GGTT_HAM0_AGCT_HAM3))

# GG/TT vs AG/TT
# Filtrar apenas o genótipo GG/TT (HAM 1) e AG/TT (HAM 3)
subset_GGTT_HAM1_AGTT_HAM3 <- df[df$diplotype %in% c("GG/TT", "AG/TT") & df$HAM %in% c(1, 0), ]

# Criar a tabela de contingência
contingency_table_GGTT_HAM1_AGTT_HAM3 <- table(subset_GGTT_HAM1_AGTT_HAM3$diplotype, subset_GGTT_HAM1_AGTT_HAM3$HAM)

# Realizar o teste de Fisher
fisher_test_GGTT_HAM1_AGTT_HAM3 <- fisher.test(contingency_table_GGTT_HAM1_AGTT_HAM3)

# Extrair o p-valor e o Odds Ratio
p_value_GGTT_HAM1_AGTT_HAM3 <- fisher_test_GGTT_HAM1_AGTT_HAM3$p.value
odds_ratio_GGTT_HAM1_AGTT_HAM3 <- fisher_test_GGTT_HAM1_AGTT_HAM3$estimate

# Extrair o intervalo de confiança de 95% do Odds Ratio
conf_int_GGTT_HAM1_AGTT_HAM3 <- fisher_test_GGTT_HAM1_AGTT_HAM3$conf.int

# Exibir os resultados
print("Tabela de Contingência (GG/TT com HAM 1 vs AG/TT com HAM 3):")
print(contingency_table_GGTT_HAM1_AGTT_HAM3)
print(paste("P-valor (Teste de Fisher):", p_value_GGTT_HAM1_AGTT_HAM3))
print(paste("Odds Ratio:", odds_ratio_GGTT_HAM1_AGTT_HAM3))
print(paste("Intervalo de Confiança de 95% para Odds Ratio:", 
            paste(conf_int_GGTT_HAM1_AGTT_HAM3, collapse = " - ")))

# GG/TT vs GG/CT
# Filtrar apenas o genótipo GG/TT (HAM 1) e GG/CT (HAM 3)
subset_GGTT_HAM1_GGCT_HAM3 <- df[df$diplotype %in% c("GG/TT", "GG/CT") & df$HAM %in% c(1, 3), ]

# Criar a tabela de contingência
contingency_table_GGTT_HAM1_GGCT_HAM3 <- table(subset_GGTT_HAM1_GGCT_HAM3$diplotype, subset_GGTT_HAM1_GGCT_HAM3$HAM)

# Realizar o teste de Fisher
fisher_test_GGTT_HAM1_GGCT_HAM3 <- fisher.test(contingency_table_GGTT_HAM1_GGCT_HAM3)

# Extrair o p-valor e o Odds Ratio
p_value_GGTT_HAM1_GGCT_HAM3 <- fisher_test_GGTT_HAM1_GGCT_HAM3$p.value
odds_ratio_GGTT_HAM1_GGCT_HAM3 <- fisher_test_GGTT_HAM1_GGCT_HAM3$estimate

# Extrair o intervalo de confiança de 95% do Odds Ratio
conf_int_GGTT_HAM1_GGCT_HAM3 <- fisher_test_GGTT_HAM1_GGCT_HAM3$conf.int

# Exibir os resultados
print("Tabela de Contingência (GG/TT com HAM 1 vs GG/CT com HAM 3):")
print(contingency_table_GGTT_HAM1_GGCT_HAM3)
print(paste("P-valor (Teste de Fisher):", p_value_GGTT_HAM1_GGCT_HAM3))
print(paste("Odds Ratio:", odds_ratio_GGTT_HAM1_GGCT_HAM3))
print(paste("Intervalo de Confiança de 95% para Odds Ratio:", 
            paste(conf_int_GGTT_HAM1_GGCT_HAM3, collapse = " - ")))

# GG/TT vs AA/TT
# Filtrar apenas o genótipo GG/TT (HAM 1) e AA/TT (HAM 3)
subset_GGTT_HAM1_AATT_HAM3 <- df[df$diplotype %in% c("GG/TT", "AA/TT") & df$HAM %in% c(0, 3), ]

# Criar a tabela de contingência
contingency_table_GGTT_HAM1_AATT_HAM3 <- table(subset_GGTT_HAM1_AATT_HAM3$diplotype, subset_GGTT_HAM1_AATT_HAM3$HAM)

# Realizar o teste de Fisher
fisher_test_GGTT_HAM1_AATT_HAM3 <- fisher.test(contingency_table_GGTT_HAM1_AATT_HAM3)

# Extrair o p-valor e o Odds Ratio
p_value_GGTT_HAM1_AATT_HAM3 <- fisher_test_GGTT_HAM1_AATT_HAM3$p.value
odds_ratio_GGTT_HAM1_AATT_HAM3 <- fisher_test_GGTT_HAM1_AATT_HAM3$estimate

# Extrair o intervalo de confiança de 95% do Odds Ratio
conf_int_GGTT_HAM1_AATT_HAM3 <- fisher_test_GGTT_HAM1_AATT_HAM3$conf.int

# Exibir os resultados
print("Tabela de Contingência (GG/TT com HAM 1 vs AA/TT com HAM 3):")
print(contingency_table_GGTT_HAM1_AATT_HAM3)
print(paste("P-valor (Teste de Fisher):", p_value_GGTT_HAM1_AATT_HAM3))
print(paste("Odds Ratio:", odds_ratio_GGTT_HAM1_AATT_HAM3))
print(paste("Intervalo de Confiança de 95% para Odds Ratio:", 
            paste(conf_int_GGTT_HAM1_AATT_HAM3, collapse = " - ")))



# Filtrar os genótipos GG/TT (HAM 1) e AG/CT (HAM 3)
subset_GGTT_HAM1_AGCT_HAM3 <- df[df$diplotype %in% c("GG/TT", "AA/CT") & df$HAM %in% c(1, 0), ]

# Criar a tabela de contingência
contingency_table_GGTT_HAM1_AGCT_HAM3 <- table(subset_GGTT_HAM1_AGCT_HAM3$diplotype, subset_GGTT_HAM1_AGCT_HAM3$HAM)

# Realizar o teste de Fisher
fisher_test_GGTT_HAM1_AGCT_HAM3 <- fisher.test(contingency_table_GGTT_HAM1_AGCT_HAM3)

# Extrair o p-valor e o Odds Ratio
p_value_GGTT_HAM1_AGCT_HAM3 <- fisher_test_GGTT_HAM1_AGCT_HAM3$p.value
odds_ratio_GGTT_HAM1_AGCT_HAM3 <- fisher_test_GGTT_HAM1_AGCT_HAM3$estimate

# Calcular o intervalo de confiança de 95%
conf_int_GGTT_HAM1_AGCT_HAM3 <- fisher_test_GGTT_HAM1_AGCT_HAM3$conf.int

# Exibir os resultados
print("Tabela de Contingência (GG/TT com HAM 1 vs AG/CT com HAM 3):")
print(contingency_table_GGTT_HAM1_AGCT_HAM3)
print(paste("P-valor (Teste de Fisher):", p_value_GGTT_HAM1_AGCT_HAM3))
print(paste("Odds Ratio:", odds_ratio_GGTT_HAM1_AGCT_HAM3))
print(paste("Intervalo de Confiança (95%):", conf_int_GGTT_HAM1_AGCT_HAM3[1], "a", conf_int_GGTT_HAM1_AGCT_HAM3[2]))

# Comparação: Alta expressão vs Média + Baixa expressão (com filtragem HAM 1 e HAM 3)
# Agrupar Baixa e Média em "Baixa/Média"
df$expression_group_combined <- ifelse(df$expression_group %in% c("Baixa", "Média"), "Baixa/Média", df$expression_group)

# Filtrar os genótipos e expressão para análise
subset_expression_HAM0_HAM1 <- df %>% 
  filter(diplotype %in% c("GG/TT", "GG/CT", "AG/TT", "GG/CC", "AA/TT", "AG/CT", "AG/CC", "AA/CT", "AA/CC") & HAM %in% c(1, 3))

# Criar a tabela de contingência por expressão combinada e HAM
contingency_table_expression_HAM <- table(subset_expression_HAM0_HAM1$expression_group_combined, subset_expression_HAM0_HAM1$HAM)

# Realizar o teste de Fisher
fisher_test_expression_HAM <- fisher.test(contingency_table_expression_HAM)

# Extrair o p-valor e o Odds Ratio
p_value_expression_HAM <- fisher_test_expression_HAM$p.value
odds_ratio_expression_HAM <- fisher_test_expression_HAM$estimate

# Calcular o intervalo de confiança de 95%
conf_int_expression_HAM <- fisher_test_expression_HAM$conf.int

# Exibir os resultados
print("Tabela de Contingência por expressão combinada (Baixa/Média) e HAM:")
print(contingency_table_expression_HAM)
print(paste("P-valor (Teste de Fisher):", p_value_expression_HAM))
print(paste("Odds Ratio:", odds_ratio_expression_HAM))
print(paste("Intervalo de Confiança (95%):", conf_int_expression_HAM[1], "a", conf_int_expression_HAM[2]))
print(colnames(df))

# Agrupar os genótipos GG/AG como uma categoria "GG_AG" e AA como "AA"
df$diplotype_grouped <- ifelse(df$rs10748643 == "AA", "AA", "GG_AG")

# Filtrar os dados para HAM = 1 e HAM = 3
subset_GGAG_HAM1_AA_HAM3 <- df[df$diplotype_grouped %in% c("GG_AG", "AA") & df$HAM %in% c(1, 0), ]

# Criar a tabela de contingência
contingency_table_GGAG_HAM1_AA_HAM3 <- table(subset_GGAG_HAM1_AA_HAM3$diplotype_grouped, subset_GGAG_HAM1_AA_HAM3$HAM)

# Realizar o teste de Fisher
fisher_test_GGAG_HAM1_AA_HAM3 <- fisher.test(contingency_table_GGAG_HAM1_AA_HAM3)

# Extrair o p-valor, Odds Ratio e intervalo de confiança
p_value_GGAG_HAM1_AA_HAM3 <- fisher_test_GGAG_HAM1_AA_HAM3$p.value
odds_ratio_GGAG_HAM1_AA_HAM3 <- fisher_test_GGAG_HAM1_AA_HAM3$estimate
conf_int_GGAG_HAM1_AA_HAM3 <- fisher_test_GGAG_HAM1_AA_HAM3$conf.int

# Exibir os resultados
print("Tabela de Contingência (GG/AG com HAM 1 vs AA com HAM 3):")
print(contingency_table_GGAG_HAM1_AA_HAM3)
print(paste("P-valor (Teste de Fisher):", p_value_GGAG_HAM1_AA_HAM3))
print(paste("Odds Ratio:", odds_ratio_GGAG_HAM1_AA_HAM3))
print(paste("Intervalo de Confiança (95%):", conf_int_GGAG_HAM1_AA_HAM3[1], "a", conf_int_GGAG_HAM1_AA_HAM3[2]))


# Agrupar os genótipos TT/CT como uma categoria "TT_CT" e CC como "CC" com base no rs11188513
df$diplotype_grouped <- ifelse(df$rs11188513 == "CC", "CC", "TT_CT")

# Filtrar os dados para HAM = 1 e HAM = 3
subset_TTCT_HAM1_CC_HAM3 <- df[df$diplotype_grouped %in% c("TT_CT", "CC") & df$HAM %in% c(1, 0), ]

# Criar a tabela de contingência
contingency_table_TTCT_HAM1_CC_HAM3 <- table(subset_TTCT_HAM1_CC_HAM3$diplotype_grouped, subset_TTCT_HAM1_CC_HAM3$HAM)

# Realizar o teste de Fisher
fisher_test_TTCT_HAM1_CC_HAM3 <- fisher.test(contingency_table_TTCT_HAM1_CC_HAM3)

# Extrair o p-valor, Odds Ratio e intervalo de confiança
p_value_TTCT_HAM1_CC_HAM3 <- fisher_test_TTCT_HAM1_CC_HAM3$p.value
odds_ratio_TTCT_HAM1_CC_HAM3 <- fisher_test_TTCT_HAM1_CC_HAM3$estimate
conf_int_TTCT_HAM1_CC_HAM3 <- fisher_test_TTCT_HAM1_CC_HAM3$conf.int

# Exibir os resultados
print("Tabela de Contingência (TT/CT com HAM 1 vs CC com HAM 3):")
print(contingency_table_TTCT_HAM1_CC_HAM3)
print(paste("P-valor (Teste de Fisher):", p_value_TTCT_HAM1_CC_HAM3))
print(paste("Odds Ratio:", odds_ratio_TTCT_HAM1_CC_HAM3))
print(paste("Intervalo de Confiança (95%):", conf_int_TTCT_HAM1_CC_HAM3[1], "a", conf_int_TTCT_HAM1_CC_HAM3[2]))

# Criar uma coluna que agrupa os genótipos de interesse (CC, CT, TT)
df$diplotype_grouped <- factor(df$rs11188513, levels = c("CC", "CT", "TT"))

# Filtrar os dados para HAM = 1 e HAM = 3
subset_CC_CT_TT_HAM1_HAM3 <- df[df$diplotype_grouped %in% c("CC", "CT", "TT") & df$HAM %in% c(1, 3), ]

# Criar a tabela de contingência para CC vs CT
contingency_CC_CT <- table(subset_CC_CT_TT_HAM1_HAM3$diplotype_grouped == "CC", subset_CC_CT_TT_HAM1_HAM3$HAM)
fisher_test_CC_CT <- fisher.test(contingency_CC_CT)
p_value_CC_CT <- fisher_test_CC_CT$p.value
odds_ratio_CC_CT <- fisher_test_CC_CT$estimate
conf_int_CC_CT <- fisher_test_CC_CT$conf.int

# Criar a tabela de contingência para CC vs TT
contingency_CC_TT <- table(subset_CC_CT_TT_HAM1_HAM3$diplotype_grouped == "CC", subset_CC_CT_TT_HAM1_HAM3$HAM)
fisher_test_CC_TT <- fisher.test(contingency_CC_TT)
p_value_CC_TT <- fisher_test_CC_TT$p.value
odds_ratio_CC_TT <- fisher_test_CC_TT$estimate
conf_int_CC_TT <- fisher_test_CC_TT$conf.int

# Criar a tabela de contingência para CT vs TT
contingency_CT_TT <- table(subset_CC_CT_TT_HAM1_HAM3$diplotype_grouped == "CT", subset_CC_CT_TT_HAM1_HAM3$HAM)
fisher_test_CT_TT <- fisher.test(contingency_CT_TT)
p_value_CT_TT <- fisher_test_CT_TT$p.value
odds_ratio_CT_TT <- fisher_test_CT_TT$estimate
conf_int_CT_TT <- fisher_test_CT_TT$conf.int

# Exibir os resultados de cada comparação
cat("\nComparação CC vs CT:\n")
cat(paste("P-valor:", p_value_CC_CT, "\n"))
cat(paste("Odds Ratio:", odds_ratio_CC_CT, "\n"))
cat(paste("Intervalo de Confiança (95%):", conf_int_CC_CT[1], "a", conf_int_CC_CT[2], "\n"))

cat("\nComparação CC vs TT:\n")
cat(paste("P-valor:", p_value_CC_TT, "\n"))
cat(paste("Odds Ratio:", odds_ratio_CC_TT, "\n"))
cat(paste("Intervalo de Confiança (95%):", conf_int_CC_TT[1], "a", conf_int_CC_TT[2], "\n"))

cat("\nComparação CT vs TT:\n")
cat(paste("P-valor:", p_value_CT_TT, "\n"))
cat(paste("Odds Ratio:", odds_ratio_CT_TT, "\n"))
cat(paste("Intervalo de Confiança (95%):", conf_int_CT_TT[1], "a", conf_int_CT_TT[2], "\n"))



# Criar uma coluna que agrupa os genótipos de interesse (AA, AG, GG)
df$diplotype_grouped_rs107 <- factor(df$rs10748643, levels = c("AA", "AG", "GG"))

# Filtrar os dados para HAM = 1 e HAM = 3
subset_AA_AG_GG_HAM1_HAM3 <- df[df$diplotype_grouped_rs107 %in% c("AA", "AG", "GG") & df$HAM %in% c(1, 3), ]

# Criar a tabela de contingência para AA vs AG
contingency_AA_AG <- table(subset_AA_AG_GG_HAM1_HAM3$diplotype_grouped_rs107 == "AA", subset_AA_AG_GG_HAM1_HAM3$HAM)
fisher_test_AA_AG <- fisher.test(contingency_AA_AG)
p_value_AA_AG <- fisher_test_AA_AG$p.value
odds_ratio_AA_AG <- fisher_test_AA_AG$estimate
conf_int_AA_AG <- fisher_test_AA_AG$conf.int

# Criar a tabela de contingência para AA vs GG
contingency_AA_GG <- table(subset_AA_AG_GG_HAM1_HAM3$diplotype_grouped_rs107 == "AA", subset_AA_AG_GG_HAM1_HAM3$HAM)
fisher_test_AA_GG <- fisher.test(contingency_AA_GG)
p_value_AA_GG <- fisher_test_AA_GG$p.value
odds_ratio_AA_GG <- fisher_test_AA_GG$estimate
conf_int_AA_GG <- fisher_test_AA_GG$conf.int

# Criar a tabela de contingência para AG vs GG
contingency_AG_GG <- table(subset_AA_AG_GG_HAM1_HAM3$diplotype_grouped_rs107 == "AG", subset_AA_AG_GG_HAM1_HAM3$HAM)
fisher_test_AG_GG <- fisher.test(contingency_AG_GG)
p_value_AG_GG <- fisher_test_AG_GG$p.value
odds_ratio_AG_GG <- fisher_test_AG_GG$estimate
conf_int_AG_GG <- fisher_test_AG_GG$conf.int

# Exibir os resultados de cada comparação
cat("\nComparação AA vs AG:\n")
cat(paste("P-valor:", p_value_AA_AG, "\n"))
cat(paste("Odds Ratio:", odds_ratio_AA_AG, "\n"))
cat(paste("Intervalo de Confiança (95%):", conf_int_AA_AG[1], "a", conf_int_AA_AG[2], "\n"))

cat("\nComparação AA vs GG:\n")
cat(paste("P-valor:", p_value_AA_GG, "\n"))
cat(paste("Odds Ratio:", odds_ratio_AA_GG, "\n"))
cat(paste("Intervalo de Confiança (95%):", conf_int_AA_GG[1], "a", conf_int_AA_GG[2], "\n"))

cat("\nComparação AG vs GG:\n")
cat(paste("P-valor:", p_value_AG_GG, "\n"))
cat(paste("Odds Ratio:", odds_ratio_AG_GG, "\n"))
cat(paste("Intervalo de Confiança (95%):", conf_int_AG_GG[1], "a", conf_int_AG_GG[2], "\n"))

