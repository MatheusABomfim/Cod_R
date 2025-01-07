# Instalar o dplyr se não estiver instalado
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Carregar o pacote dplyr
library(dplyr)

library(readr)
install.packages("openxlsx")
library(openxlsx)
FUNCIONALIDADE_GENOTIPAGEM_total <- read_delim("R/Dados brutos_privados LIPa/FUNCIONALIDADE + GENOTIPAGEM/FUNCIONALIDADE_GENOTIPAGEM_total.csv", 
                                               delim = ".", escape_double = FALSE, col_types = cols(Código = col_skip(), 
                                                                                                    Idade = col_number(), `MIF TOTAL` = col_number(), 
                                                                                                    Gênero = col_character(), ALIMENTAÇÃO = col_number(), 
                                                                                                    `HIGIENE PESSOAL` = col_number(), 
                                                                                                    BANHO = col_number(), `VESTIR METADE SUPERIOR` = col_number(), 
                                                                                                    `VESTIR METADE INFERIOR` = col_number(), 
                                                                                                    `UTILIZAÇÃO DO VASO SANITÁRIO` = col_number(), 
                                                                                                    `CONTROLE DA URINA` = col_number(), 
                                                                                                    `CONTROLE DAS FEZES` = col_number(), 
                                                                                                    `TRANSFERÊNCIA - LEITO, CADEIRA` = col_number(), 
                                                                                                    `TRANSFERÊNCIA - VASO SANITÁRIO` = col_number(), 
                                                                                                    `TRANSFERÊNCIA - BANHEIRA, CHUVEIRO` = col_number(), 
                                                                                                    ESCADAS = col_number(), COMPREENSÃO = col_number(), 
                                                                                                    EXPRESSÃO = col_number(), `INTERAÇÃO SOCIAL` = col_number(), 
                                                                                                    `RESOLUÇÃO DE PROBLEMAS` = col_number(), 
                                                                                                    MEMÓRIA = col_number()), trim_ws = TRUE)
View(FUNCIONALIDADE_GENOTIPAGEM_total)

nic_clean <- na.omit(FUNCIONALIDADE_GENOTIPAGEM_total)
sapply(nic_clean, class)
---------------------------------------------------------------------------
### # Creation of contingency tables
  
# CONTINGENCY rs643 - DOMINANT MIF < 108 and MIF > 108
dados_rs643_dominante_108 <- matrix(c(5, 16, 9, 51), nrow = 2, byrow = TRUE)
rownames(dados_rs643_dominante_108) <- c("GG", "AG + AA")
colnames(dados_rs643_dominante_108) <- c("MIF < 108", "MIF > 108")
tabela_rs643_dominante_108 <- as.table(dados_rs643_dominante_108)

# CONTINGENCY rs643 - DOMINANT MIF < 117 and MIF > 117
dados_rs643_dominante_117 <- matrix(c(7, 14, 12, 48), nrow = 2, byrow = TRUE)
rownames(dados_rs643_dominante_117) <- c("GG", "AG + AA")
colnames(dados_rs643_dominante_117) <- c("MIF < 117", "MIF > 117")
tabela_rs643_dominante_117 <- as.table(dados_rs643_dominante_117)

# CONTINGENCY rs643 - RECESSIVE MIF < 108 and MIF > 108
dados_rs643_recessivo_108 <- matrix(c(12, 54, 2, 13), nrow = 2, byrow = TRUE)
rownames(dados_rs643_recessivo_108) <- c("GG + AG", "AA")
colnames(dados_rs643_recessivo_108) <- c("MIF < 108", "MIF > 108")
tabela_rs643_recessivo_108 <- as.table(dados_rs643_recessivo_108)

# CONTINGENCY rs643 - RECESSIVE MIF < 117 and MIF > 117
dados_rs643_recessivo_117 <- matrix(c(15, 51, 4, 11), nrow = 2, byrow = TRUE)
rownames(dados_rs643_recessivo_117) <- c("GG + AG", "AA")
colnames(dados_rs643_recessivo_117) <- c("MIF < 117", "MIF > 117")
tabela_rs643_recessivo_117 <- as.table(dados_rs643_recessivo_117)

# CONTINGENCY rs513 - DOMINANT MIF < 108 and MIF > 108
dados_rs513_dominante_108 <- matrix(c(6, 28, 8, 39), nrow = 2, byrow = TRUE)
rownames(dados_rs513_dominante_108) <- c("TT", "CT + CC")
colnames(dados_rs513_dominante_108) <- c("MIF < 108", "MIF > 108")
tabela_rs513_dominante_108 <- as.table(dados_rs513_dominante_108)

# CONTINGENCY rs513 - DOMINANT MIF < 117 and MIF > 117
dados_rs513_dominante_117 <- matrix(c(10, 24, 9, 38), nrow = 2, byrow = TRUE)
rownames(dados_rs513_dominante_117) <- c("TT", "CT + CC")
colnames(dados_rs513_dominante_117) <- c("MIF < 117", "MIF > 117")
tabela_rs513_dominante_117 <- as.table(dados_rs513_dominante_117)

# CONTINGENCY rs513 - RECESSIVE MIF < 108 and MIF > 108
dados_rs513_recessivo_108 <- matrix(c(14, 62, 0, 5), nrow = 2, byrow = TRUE)
rownames(dados_rs513_recessivo_108) <- c("TT + CT", "CC")
colnames(dados_rs513_recessivo_108) <- c("MIF < 108", "MIF > 108")
tabela_rs513_recessivo_108 <- as.table(dados_rs513_recessivo_108)

# CONTINGENCY rs513 - RECESSIVE MIF < 117 and MIF > 117
dados_rs513_recessivo_117 <- matrix(c(19, 57, 0, 5), nrow = 2, byrow = TRUE)
rownames(dados_rs513_recessivo_117) <- c("TT + CT", "CC")
colnames(dados_rs513_recessivo_117) <- c("MIF < 117", "MIF > 117")
tabela_rs513_recessivo_117 <- as.table(dados_rs513_recessivo_117)
---------------------------------------------------------------------------
# Fisher tests
  
# Fisher tests rs643 - DOMINANTE MIF < 108 e MIF > 108
teste_fisher_rs643_dominante_108 <- fisher.test(tabela_rs643_dominante_108)
print(teste_fisher_rs643_dominante_108)

# Fisher tests rs643 - DOMINANTE MIF < 117 e MIF > 117
teste_fisher_rs643_dominante_117 <- fisher.test(tabela_rs643_dominante_117)
print(teste_fisher_rs643_dominante_117)

# Fisher tests - RECESSIVO MIF < 108 e MIF > 108
teste_fisher_rs643_recessivo_108 <- fisher.test(tabela_rs643_recessivo_108)
print(teste_fisher_rs643_recessivo_108)

# Fisher tests - RECESSIVO MIF < 117 e MIF > 117
teste_fisher_rs643_recessivo_117 <- fisher.test(tabela_rs643_recessivo_117)
print(teste_fisher_rs643_recessivo_117)

# Fisher tests - DOMINANTE MIF < 108 e MIF > 108
teste_fisher_rs513_dominante_108 <- fisher.test(tabela_rs513_dominante_108)
print(teste_fisher_rs513_dominante_108)

# Fisher tests - DOMINANTE MIF < 117 e MIF > 117
teste_fisher_rs513_dominante_117 <- fisher.test(tabela_rs513_dominante_117)
print(teste_fisher_rs513_dominante_117)

# Fisher tests - RECESSIVO MIF < 108 e MIF > 108
teste_fisher_rs513_recessivo_108 <- fisher.test(tabela_rs513_recessivo_108)
print(teste_fisher_rs513_recessivo_108)

# Fisher tests - RECESSIVO MIF < 117 e MIF > 117
teste_fisher_rs513_recessivo_117 <- fisher.test(tabela_rs513_recessivo_117)
print(teste_fisher_rs513_recessivo_117)
------------------------------------------------------------------------------------------------------------
# Creation of a data frame to store the results of Fisher's tests
resultados_fisher <- data.frame(
  Comparacao = c(
      "rs643 Dominante MIF < 108 e MIF > 108",
      "rs643 Dominante MIF < 117 e MIF > 117",
      "rs643 Recessivo MIF < 108 e MIF > 108",
      "rs643 Recessivo MIF < 117 e MIF > 117",
      "rs513 Dominante MIF < 108 e MIF > 108",
      "rs513 Dominante MIF < 117 e MIF > 117",
      "rs513 Recessivo MIF < 108 e MIF > 108",
      "rs513 Recessivo MIF < 117 e MIF > 117"
    ),
    p_value = c(
      teste_fisher_rs643_dominante_108$p.value,
      teste_fisher_rs643_dominante_117$p.value,
      teste_fisher_rs643_recessivo_108$p.value,
      teste_fisher_rs643_recessivo_117$p.value,
      teste_fisher_rs513_dominante_108$p.value,
      teste_fisher_rs513_dominante_117$p.value,
      teste_fisher_rs513_recessivo_108$p.value,
      teste_fisher_rs513_recessivo_117$p.value
    ),
    Odds_Ratio = c(
      teste_fisher_rs643_dominante_108$estimate,
      teste_fisher_rs643_dominante_117$estimate,
      teste_fisher_rs643_recessivo_108$estimate,
      teste_fisher_rs643_recessivo_117$estimate,
      teste_fisher_rs513_dominante_108$estimate,
      teste_fisher_rs513_dominante_117$estimate,
      teste_fisher_rs513_recessivo_108$estimate,
      teste_fisher_rs513_recessivo_117$estimate
    ),
    Conf_Interval_Lower = c(
      teste_fisher_rs643_dominante_108$conf.int[1],
      teste_fisher_rs643_dominante_117$conf.int[1],
      teste_fisher_rs643_recessivo_108$conf.int[1],
      teste_fisher_rs643_recessivo_117$conf.int[1],
      teste_fisher_rs513_dominante_108$conf.int[1],
      teste_fisher_rs513_dominante_117$conf.int[1],
      teste_fisher_rs513_recessivo_108$conf.int[1],
      teste_fisher_rs513_recessivo_117$conf.int[1]
    ),
    Conf_Interval_Upper = c(
      teste_fisher_rs643_dominante_108$conf.int[2],
      teste_fisher_rs643_dominante_117$conf.int[2],
      teste_fisher_rs643_recessivo_108$conf.int[2],
      teste_fisher_rs643_recessivo_117$conf.int[2],
      teste_fisher_rs513_dominante_108$conf.int[2],
      teste_fisher_rs513_dominante_117$conf.int[2],
      teste_fisher_rs513_recessivo_108$conf.int[2],
      teste_fisher_rs513_recessivo_117$conf.int[2]
    )
  )

# Save the results in an Excel file
output_path <- "C:/Users/mathe/OneDrive/Documentos/R/Análises 2024 LIPa/resultados_fisher.xlsx"
write.xlsx(resultados_fisher, output_path, rowNames = FALSE)

#Columns: rs513 Recessive MIF < 108 and MIF > 108 and rs513 Recessive MIF < 117 and MIF > 117 with Odds_Ratio and Conf_int_Upper
#with results “bugged” by showing cells with a value of 0


# Evaluation of variables that compose the MIF
# Dividir os grupos
FUNCIONALIDADE_GENOTIPAGEM_total <- FUNCIONALIDADE_GENOTIPAGEM_total %>%
  mutate(grupo_MIF = ifelse('MIF TOTAL' > 108, "alta", "baixa"))

# Verificar a estrutura do dataframe para garantir que os grupos foram criados corretamente
str(FUNCIONALIDADE_GENOTIPAGEM_total)

# Lista de variáveis para testar
variaveis <- c("ALIMENTAÇÃO", "HIGIENE PESSOAL", "BANHO", "VESTIR METADE SUPERIOR", "VESTIR METADE INFERIOR", 
               "UTILIZAÇÃO DO VASO SANITÁRIO", "CONTROLE DA URINA", "CONTROLE DAS FEZES", "TRANSFERÊNCIA - LEITO, CADEIRA",
               "TRANSFERÊNCIA - VASO SANITÁRIO", "TRANSFERÊNCIA - BANHEIRA, CHUVEIRO","LOCOMOÇÃO - MARCHA/CADEIRA DE RODAS",
               "ESCADAS", "COMPREENSÃO", "EXPRESSÃO", "INTERAÇÃO SOCIAL","RESOLUÇÃO DE PROBLEMAS", "MEMÓRIA")

# Teste de normalidade para cada variável em cada grupo
resultados_normalidade <- lapply(variaveis, function(var) {
  alta <- FUNCIONALIDADE_GENOTIPAGEM_total %>% filter(grupo_MIF == "alta") %>% pull(var)
  baixa <- FUNCIONALIDADE_GENOTIPAGEM_total %>% filter(grupo_MIF == "baixa") %>% pull(var)
  
  shapiro_alta <- shapiro.test(alta)
  shapiro_baixa <- shapiro.test(baixa)
  
  list(
    variavel = var,
    shapiro_alta = shapiro_alta,
    shapiro_baixa = shapiro_baixa
  )
})

# Exibir resultados dos testes de normalidade
resultados_normalidade

