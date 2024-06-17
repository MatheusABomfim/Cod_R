# Install packages
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}

# Load packages
library(dplyr)
library(readr)
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

nic_clean <- na.omit(FUNCIONALIDADE_GENOTIPAGEM_total)
sapply(nic_clean, class)
------------------------------------------------------------------------------------------------------------------------------------
# Evaluation of variables that compose the MIF
# Divide the groups
nic_clean <- nic_clean %>%
  mutate(grupo_MIF = ifelse(`MIF TOTAL` > 108, "alta", "baixa"))

# Check the structure of the dataframe to ensure that the groups have been created correctly
str(nic_clean)


# List of variables to test
variaveis <- c("ALIMENTAÇÃO", "HIGIENE PESSOAL", "BANHO", "VESTIR METADE SUPERIOR", "VESTIR METADE INFERIOR", 
               "UTILIZAÇÃO DO VASO SANITÁRIO", "CONTROLE DA URINA", "CONTROLE DAS FEZES", "TRANSFERÊNCIA - LEITO, CADEIRA",
               "TRANSFERÊNCIA - VASO SANITÁRIO", "TRANSFERÊNCIA - BANHEIRA, CHUVEIRO","LOCOMOÇÃO - MARCHA/CADEIRA DE RODAS",
               "ESCADAS", "COMPREENSÃO", "EXPRESSÃO", "INTERAÇÃO SOCIAL","RESOLUÇÃO DE PROBLEMAS", "MEMÓRIA")

# Normality test for each variable in each group
resultados_normalidade <- lapply(variaveis, function(var) {
  alta <- nic_clean %>% filter(grupo_MIF == "alta") %>% pull(var)
  baixa <- nic_clean %>% filter(grupo_MIF == "baixa") %>% pull(var)
  
  shapiro_alta <- shapiro.test(alta)
  shapiro_baixa <- shapiro.test(baixa)
  
  list(
    variavel = var,
    shapiro_alta = shapiro_alta,
    shapiro_baixa = shapiro_baixa
  )
})

# Convert results to a dataframe
df_resultados <- do.call(rbind, lapply(resultados_normalidade, function(x) {
  data.frame(
    Variavel = x$variavel,
    Shapiro_Alta_P_Value = x$shapiro_alta$p.value,
    Shapiro_Baixa_P_Value = x$shapiro_baixa$p.value
  )
}))

# Adding columns to indicate significance
df_resultados$Significancia_Alta <- ifelse(df_resultados$Shapiro_Alta_P_Value <= 0.05, "Não-paramêtrica", "Paramêtrica")
df_resultados$Significancia_Baixa <- ifelse(df_resultados$Shapiro_Baixa_P_Value <= 0.05, "Não-paramêtrica", "Paramêtrica")

# Rearrange the columns
df_resultados <- df_resultados[, c("Variavel", "Shapiro_Alta_P_Value", "Significancia_Alta", "Shapiro_Baixa_P_Value", "Significancia_Baixa")]

# Create a workbook and add a spreadsheet with the results
wb <- createWorkbook()
addWorksheet(wb, "Normalidade_cat108")
writeData(wb, "Normalidade_cat108", df_resultados)

# Save the workbook in an Excel file in the specified path
caminho <- "C:/Users/mathe/OneDrive/Documentos/R/Análises 2024 LIPa/"
saveWorkbook(wb, caminho, overwrite = TRUE)
names(nic_clean)
--------------------------------------------------------------------------------------------------------------------------------------------------------------
# Iterate over all the columns of the nic_clean data frame
  for (col in names(nic_clean)) {
    # Checks if the column is of type character
    if (is.character(nic_clean[[col]])) {
      # Remove accents and replace spaces with _
      nic_clean[[col]] <- gsub("Á", "A", nic_clean[[col]])
      nic_clean[[col]] <- gsub("É", "E", nic_clean[[col]])
      nic_clean[[col]] <- gsub("Í", "I", nic_clean[[col]])
      nic_clean[[col]] <- gsub("Ó", "O", nic_clean[[col]])
      nic_clean[[col]] <- gsub("Ú", "U", nic_clean[[col]])
      nic_clean[[col]] <- gsub("Â", "A", nic_clean[[col]])
      nic_clean[[col]] <- gsub("Ê", "E", nic_clean[[col]])
      nic_clean[[col]] <- gsub("Î", "I", nic_clean[[col]])
      nic_clean[[col]] <- gsub("Ô", "O", nic_clean[[col]])
      nic_clean[[col]] <- gsub("Û", "U", nic_clean[[col]])
      nic_clean[[col]] <- gsub("Ã", "A", nic_clean[[col]])
      nic_clean[[col]] <- gsub("Õ", "O", nic_clean[[col]])
      nic_clean[[col]] <- gsub("Ç", "C", nic_clean[[col]])
      nic_clean[[col]] <- gsub("\\s", "_", nic_clean[[col]])
      
      # Adaptation to easy-to-understand names
      nic_clean[[col]] <- gsub("TRANSFERENCIA_-_LEITO,_CADEIRA", "TRANSFERENCIA_LEITO_CADEIRA", nic_clean[[col]])
      nic_clean[[col]] <- gsub("TRANSFERENCIA_-_VASO_SANITARIO", "TRANSFERENCIA_VASO_SANITARIO", nic_clean[[col]])
      nic_clean[[col]] <- gsub("TRANSFERENCIA_-_BANHEIRA,_CHUVEIRO", "TRANSFERENCIA_BANHEIRA_CHUVEIRO", nic_clean[[col]])
      nic_clean[[col]] <- gsub("LOCOMOCAO_-_MARCHA/CADEIRA_DE_RODAS", "LOCOMOCAO_MARCHA_CADEIRA_DE_RODAS", nic_clean[[col]])
      nic_clean[[col]] <- gsub("INTERACAO_SOCIAL", "INTERACAO_SOCIAL", nic_clean[[col]])
      nic_clean[[col]] <- gsub("RESOLUCAO_DE_PROBLEMAS", "RESOLUCAO_DE_PROBLEMAS", nic_clean[[col]])
    }
  }


categorias <- list(
  Mobilidade = c("TRANSFERENCIA_LEITO_CADEIRA", "TRANSFERENCIA_VASO_SANITARIO", 
                 "TRANSFERENCIA_BANHEIRA_CHUVEIRO", "LOCOMOCAO_MARCHA_CADEIRA_DE_RODAS", "ESCADAS"),
  Cognicao = c("COMPREENSAO", "EXPRESSAO", "INTERACAO_SOCIAL", "RESOLUCAO_DE_PROBLEMAS", "MEMORIA")
)

# Apply the function to the variable names
categorias <- lapply(categorias, remove_acentos)

# Function to run the Kruskal-Wallis test for each category and save the results in an Excel spreadsheet
kruskal_wallis_results <- function(nic_clean, categorias, caminho) {
  # List for storing the results of the Kruskal-Wallis test
  resultados_kruskal_wallis <- list()
  
  # Iterate on the categories
  for(categoria in names(categorias)) {
    # Select the variables corresponding to the category
    variaveis_categoria <- categorias[[categoria]]
    
    # Run the Kruskal-Wallis test for the category
    kruskal_result <- kruskal.test(as.formula(paste(paste(variaveis_categoria, collapse = " + "), "~ grupo_MIF")), data = nic_clean)
    
    # Store the results
    resultados_kruskal_wallis[[categoria]] <- kruskal_result
  }
  
  # Create a dataframe with the results of the Kruskal-Wallis test
  resultados_df <- data.frame(
    Categoria = names(resultados_kruskal_wallis),
    Chi_quadrado = sapply(resultados_kruskal_wallis, function(x) x$statistic),
    Grau_liberdade = sapply(resultados_kruskal_wallis, function(x) x$parameter),
    P_valor = sapply(resultados_kruskal_wallis, function(x) x$p.value)
  )
  
  # Save in excel format
  nome_arquivo <- "resultados_kruskal_wallis_por_categoria.xlsx"
  caminho_arquivo <- file.path(caminho, nome_arquivo)
  write.xlsx(resultados_df, caminho_arquivo)
  cat("Os resultados foram salvos no arquivo:", caminho_arquivo, "\n")
}

# Function call
kruskal_wallis_results(nic_clean, categorias, caminho)







