library(genetics)
library(dplyr)
library(stringr)
library(epitools)

# Ler o arquivo CSV (manualmente faço a seleção)
df <- df_gc
print(df)

# Criar uma nova variável 'PVHTLV' baseada nos valores de 'HAM'
df$PVHTLV <- ifelse(df$HAM %in% c(0, 1), "Ass/oligo", ifelse(df$HAM == 3, "GC", NA))

# Verificar a tabela resultante
table(df$PVHTLV)

# Função para realizar a regressão logística binária de forma iterativa
run_logistic_regression <- function(df, genotype_col, ham_col, expression_group, age_col, ham_values = c(1, 0)) {
  
  # Garantir que "Alta" seja a referência
  df$expression_group <- factor(df$expression_group, levels = c("Alta", "Média", "Baixa"))
  
  # Filtrar os dados para os valores de HAM desejados
  df_filtered <- subset(df, df[[ham_col]] %in% ham_values)
  
  # Verificar se há dados suficientes após o filtro
  if (nrow(df_filtered) < 2) {
    stop("Não há dados suficientes após o filtro. Verifique os valores de HAM e a expressão.")
  }
  
  # Garantir que 'Alta' seja a referência no modelo
  df_filtered$expression_group <- factor(df_filtered$expression_group, levels = c("Alta", "Média", "Baixa"))
  
  # Ajustar o modelo de regressão logística
  formula <- as.formula(paste(ham_col, "~ expression_group +", age_col))
  modelo <- glm(formula, data = df_filtered, family = binomial)
  
  # Obter Odds Ratios (OR) e intervalos de confiança
  exp_coef <- exp(coef(modelo))  # Odds Ratios
  conf_int <- exp(confint(modelo))  # Intervalos de Confiança de 95%
  
  # Criar uma tabela de resultados
  resultado <- data.frame(
    Variável = c("Intercepto", "Média", "Baixa", "Idade"),
    OR = c(exp_coef[1], exp_coef[2], exp_coef[3], exp_coef[4]),
    IC_95_Lower = c(conf_int[1, 1], conf_int[2, 1], conf_int[3, 1], conf_int[4, 1]),
    IC_95_Upper = c(conf_int[1, 2], conf_int[2, 2], conf_int[3, 2], conf_int[4, 2]),
    p_value = summary(modelo)$coefficients[, 4]
  )
  
  # Adicionar manualmente a referência 'Alta'
  resultado <- rbind(
    data.frame(Variável = "Alta (referência)", OR = 1, IC_95_Lower = NA, IC_95_Upper = NA, p_value = NA),
    resultado
  )
  
  # Exibir a tabela de resultados
  print(resultado)
  
  return(resultado)  # Retorna o resultado do modelo
}

# Exemplo de como passar os grupos de expressão para a função
expression_group <- list(
  high_expression = c("GG/TT", "GG/CT", "AG/TT"),  # Exemplo de diplótipos para alta expressão
  media_exp = c("GG/CC", "AA/TT", "AG/CT"),  # Exemplo de diplótipos para expressão média
  baixa_exp = c("AG/CC", "AA/CT", "AA/CC")  # Exemplo de diplótipos para expressão baixa
)

# Executar o modelo para diferentes valores de HAM (1 e 3)
resultado_HAM_1_3 <- run_logistic_regression(df, genotype_col = "rs10748643", ham_col = "HAM", expression_group = expression_group, age_col = "Idade", ham_values = c(1, 0))
``
