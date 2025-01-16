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
freq_diplo <- table(df$diplotype, df$HAM)

# Exibir os resultados
print("Frequency of Diplotypes:")
print(freq_diplo)

# Definir os grupos de diplótipos por expressão
high_expression <- c("GG/TT", "GG/CT", "AG/TT")
medium_expression <- c("GG/CC", "AA/TT", "AG/CT") 
low_expression <- c("AG/CC", "AA/CT", "AA/CC")

# Atribuir o grupo de expressão corretamente
df$expression_group <- ifelse(df$diplotype %in% high_expression, "Alta",
                              ifelse(df$diplotype %in% medium_expression, "Média", "Baixa"))

# Calcular contagem agrupada por 'expression_group' e 'HAM'
count_expression_ham <- df %>%
  group_by(expression_group, HAM) %>%
  summarise(count = n())

# Visualizar o resultado
print(count_expression_ham)

  # Filtrar para duas categorias de HAM (1 e 0) -> Alterar de forma iterativa, 1 = HAM 0 = ASS/OLIGO 3= GC
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

# Avaliação GG/TT e GG/CT (exemplo) --> Fazer a alteração de forma iterativa dos diplótipos de interesse e grupos avaliados
  # Filtrar apenas amostras onde HAM é 1 ou 3
  df_filtrado <- df[df$HAM %in% c(1, 0), ]
  
  # Filtrar apenas os diplótipos GG/TT e GG/CT no dataframe filtrado
  subset_cont_table <- df_filtrado[df_filtrado$diplotype %in% c("GG/TT", "AG/TT"), ]
  
  # Criar a tabela de contingência
  contingency_table <- table(subset_cont_table$diplotype, subset_cont_table$HAM)
  
  # Realizar o teste de Fisher
  fisher_test_contingency_table <- fisher.test(contingency_table)
  
  # Extrair o p-valor, Odds Ratio e IC95
  p_value_contingency_table <- fisher_test_contingency_table$p.value
  odds_ratio_contigency_table <- fisher_test_contingency_table$estimate
  ic95_contigency_table <- fisher_test_contingency_table$conf.int  # IC 95%
  
  # Exibir os resultados
  print("Tabela de Contingência:")
  print(contingency_table)
  print(paste("P-valor (Teste de Fisher):", p_value_contingency_table))
  print(paste("Odds Ratio:", odds_ratio_contigency_table))
  print(paste("IC 95%:", round(ic95_contigency_table[1], 3), "-", round(ic95_GGTT_GGCT[2], 3)))  # Formatação do IC95
  

# Tabela de contigência: Alta expressão vs Média + Baixa expressão (com filtragem por agrupamento HAM)
# Agrupar Baixa e Média em "Baixa/Média"
df$expression_group_combined <- ifelse(df$expression_group %in% c("Baixa", "Média"), "Baixa/Média", df$expression_group)

# Filtrar os genótipos e expressão para análise (selecionados diplótipos de alta expressão)
subset_expression_HAM0_HAM1 <- df %>% 
  filter(diplotype %in% c("GG/TT", "GG/CT", "AG/TT", "GG/CC", "AA/TT", "AG/CT", "AG/CC", "AA/CT", "AA/CC") & HAM %in% c(1, 3)) # <- Local de filtragem do grupos

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

  # Agrupamento de diplótipos para avaliação de haplótipos em conjuntos (heterozigotos com homozigotos) ex: GG/AG como uma categoria "GG_AG" e AA como "AA"
  # Alterar de forma iterativa diplótipos de interesse
  # Função para agrupar diplótipos e realizar o teste de Fisher
  analyze_diplotype <- function(df, diplotype_col, ham_col, group1, group2, ham_values) {
    # Criar uma nova coluna agrupada com base nos diplótipos de interesse
    df$diplotype_grouped <- ifelse(df[[diplotype_col]] %in% group1, group1[1], group2[1])
    
    # Filtrar os dados com base nos valores específicos de HAM
    subset_df <- df[df$diplotype_grouped %in% c(group1[1], group2[1]) & df[[ham_col]] %in% ham_values, ]
    
    # Criar a tabela de contingência
    contingency_table <- table(subset_df$diplotype_grouped, subset_df[[ham_col]])
    
    # Realizar o teste de Fisher
    fisher_test <- fisher.test(contingency_table)
    
    # Extrair o p-valor, Odds Ratio e intervalo de confiança
    p_value <- fisher_test$p.value
    odds_ratio <- fisher_test$estimate
    conf_int <- fisher_test$conf.int
    
    # Exibir os resultados
    print("Tabela de Contingência:")
    print(contingency_table)
    print(paste("P-valor (Teste de Fisher):", p_value))
    print(paste("Odds Ratio:", odds_ratio))
    print(paste("Intervalo de Confiança (95%):", conf_int[1], "a", conf_int[2]))
    
    # Retornar os resultados como uma lista (opcional)
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

  # Função para contar alelos por grupo (HAM) com seleção do valor desejado
  count_alleles_by_group <- function(df, genotype_col, group_col, ham_value) {
    # Filtrar o dataframe para o valor desejado de HAM
    df_group <- df[df[[group_col]] == ham_value, ]
    
    # Criar uma lista para armazenar os resultados
    allele_counts_list <- list()
    
    # Extrair a coluna de genótipos e separar os alelos em uma lista
    alleles <- unlist(strsplit(paste(df_group[[genotype_col]], collapse = ""), ""))
    
    # Contar a frequência de cada alelo
    allele_counts <- table(alleles)
    
    # Armazenar o resultado no formato dataframe com a informação do grupo (HAM)
    allele_counts_df <- as.data.frame(allele_counts)
    allele_counts_df$Group <- ham_value
    
    # Exibir os resultados
    print(paste("Contagem de Alelos para HAM =", ham_value, ":"))
    print(allele_counts_df)
    
    return(allele_counts_df)  # Retorna o dataframe com a contagem dos alelos
  }
  
  # Exemplo de uso da função para o valor HAM = 1 
  # Alterar de forma iterativa a coluna avaliada e o agrupamento HAM
  allele_count_result_HAM1 <- count_alleles_by_group(df, genotype_col = "rs10748643", group_col = "HAM", ham_value = 1)
  
  