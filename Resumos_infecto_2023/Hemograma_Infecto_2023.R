library(readxl)
library(dplyr)
library(writexl)

# Dados de exemplo
caminho_do_arquivo <- "C:/Users/mathe/OneDrive/Documentos/R/Resumos infecto 2023/Dados_Brutos/dados_hemograma.csv"
dados_hemograma <- read.csv(caminho_do_arquivo)
print(dados_hemograma)

# Identificar os nomes das colunas
nomes_colunas <- colnames(dados_hemograma)

# Exibir os nomes das colunas
print(nomes_colunas)
ordem_desejada <- dados_hemograma

# Ordem desejada das colunas
ordem_desejada <- c("REGISTRO", "...1", "HEMÁCIAS...10..6.", "HEMOGLOBINA..g.dL.", "HEMATÓCRITO....", "VCM..fL.", "HCM..pg..", "CHCM..g.dL.", "PLAQUETAS", "MPV", "OBS.CITOMORFOLÓGICA")
print(ordem_desejada)

# Verificar se é um data frame diretamente
is_data_frame <- is.data.frame(ordem_desejada)
print(is_data_frame)

# Converter a lista com a ordem desejada para um data frame
df_ordem_desejada<- as.data.frame(ordem_desejada)

# Renomear colunas 
colunas_renomeadas <- c("Sexo", "REGISTRO", "HEMÁCIAS", "HEMOGLOBINA", "HEMATÓCRITO", "VCM", "HCM", "CHCM", "PLAQUETAS", "MPV", "Observações eritrocitárias citomorfológicas")
colnames(dados_hemograma) <- colunas_renomeadas

# Fazer Backup
backup_dados_hemograma <- dados_hemograma
print(backup_dados_hemograma)

# Selecionar todas as colunas exceto a coluna Plaquetas
dados_hemograma_final <- dados_hemograma[, -9]
print(dados_hemograma_final)

# Salvar arquivo e indicar onde foi
write_xlsx(dados_hemograma_final,"C:/Users/mathe/OneDrive/Documentos/R/Resumos infecto 2023/Resultados_e_TabelasLimpas/tabela_linhagemVerm_organizada.xlsx")
caminho_arquivo_excel <- "C:/Users/mathe/OneDrive/Documentos/R/Resumos infecto 2023/Resultados_e_TabelasLimpas/tabela_linhagemVerm_organizada.xlsx"


# Verifique se o arquivo já existe
if (file.exists(caminho_arquivo_excel)) {
  cat("O arquivo", caminho_arquivo_excel, "já existe.\n")
  
  # Pergunte ao usuário se deseja sobrescrevê-lo
  resposta <- readline(prompt = "Deseja sobrescrever o arquivo? (s/n): ")
  resposta <- tolower(resposta)
  
  if (resposta == "s" || resposta == "s") {
    write_xlsx(dados_hemograma_final, caminho_arquivo_excel)
    cat("O arquivo foi sobrescrito com sucesso!\n")
  } else {
    cat("Operação cancelada. O arquivo não foi sobrescrito.\n")
  }
} else {
  write_xlsx(dados_hemograma_final, caminho_arquivo_excel)
  cat("O arquivo foi salvo com sucesso!\n")
}

--------------------------------------------------------------------------------------------------------------

#Etapas de integração de tabelas Série vermelha(com MVP) + branca e plaquetas:
caminho_leuco_plaq <- "C:/Users/mathe/OneDrive/Documentos/R/Resumos infecto 2023/Dados_Brutos/Resultados_leuco_plaq.csv"

#Definição de datasets:
caminho_leuco_plaq <- "C:/Users/mathe/OneDrive/Documentos/R/Resumos infecto 2023/Dados_Brutos/Resultados_leuco_plaq.csv" 
leuco_plaq <- read.csv(caminho_leuco_plaq)
#print(dados_leuco_plaq)
#caminho_hemo_vr <- "C:/Users/mathe/OneDrive/Documentos/R/Resumos infecto 2023/Dados_Brutos/tabela_hemo+JP.csv" 
#hemo_vr <- read.csv(caminho_hemo_vr)

#Alterar nome da coluna "Nº do prontuário" na tabela (Resultados hemogramas_leucócitos - HTLV 2023)
#1- identificar:
nomes_colunas_leuco_plaq <- colnames(leuco_plaq)
print(nomes_colunas_leuco_plaq)

#2- Alterar:
renomear <- "REGISTRO"
coluna_alvo <- "Nº.do.prontuário"
colnames(leuco_plaq)[colnames(leuco_plaq) == coluna_alvo] <- renomear
head(leuco_plaq)

#Integração da série vermelha com VR (tabela_hemo_JP_csv) + leucócitos e plaquetas (Resultados hemogramas_leucócitos - HTLV 2023)
#Critério de junção:REGISTRO

#1- Excluir coluna fantasma e sexo (Pois já tem na outro tabela a ser integrada) da leuco_plaq:
leuco_plaq_final <- leuco_plaq[-c(2, 22, 23, 24)]


#2- Fazer Backup
leuco_plaq_bk <- data.frame(leuco_plaq)
hemo_vr_bk <- data.frame(hemo_vr)

#3- Reorganizar colunas leuc_plaq:
ordem_desejada <- c("REGISTRO", "Pacientes", "Data.de.nasc.", "Data.da.coleta", "Idade", "Nº.do.prontuário", "Blastos", "Promielocitos", "Mielocitos", "Metamielocitos", "Bastonetes", "Leucócitos", "Segmentados..neutrofilos.", "Eosinófilos", "Basófilos", "Linfocitos.Tipicos", "Linfocitos.Atipicos", "Monócitos", "Eritroblastos", "Plaquetas","OBS.CITOMORFOLÓGICA...21", "...22", "OBS.CITOMORFOLÓGICA...24")
leuco_plaq_final <- subset(leuco_plaq_final, select = ordem_desejada)

# Erro por não correspondência de colunas com a "ordem_desejada", verificação do nome das colunas do data frame
colunas_presentes <- names(leuco_plaq_final)
colunas_faltantes <- setdiff(ordem_desejada, colunas_presentes)
colunas_faltantes  # Exibir as colunas faltantes, se houver

# Selecionar apenas as colunas presentes no data frame
leuco_plaq_final <- leuco_plaq_final[, colunas_presentes]
print(leuco_plaq_final)
colnames(leuco_plaq_final)

#3.1- Com erro corrigido
ordem_desejada <- c("Pacientes", "Data.de.nasc.", "Data.da.coleta", "Idade", "REGISTRO", "Blastos", "Promielocitos", "Mielocitos", "Metamielocitos", "Bastonetes", "Leucócitos", "Segmentados..neutrofilos.", "Eosinófilos", "Basófilos", "Linfocitos.Tipicos", "Linfocitos.Atipicos", "Monócitos", "Eritroblastos", "OBS.CITOMORFOLÓGICA", "...22", "...23", "...24")
leuco_plaq_final <- subset(leuco_plaq_final, select = ordem_desejada)
head(leuco_plaq_final, n=1)
head(ordem_desejada)

#4- Fusão entre tabelas organizadas
tab_hemo_leuco_plaq <- merge(dados_hemograma_final, leuco_plaq_final, by.x="REGISTRO", by.y="REGISTRO", all.x = FALSE) #combinação das duas tabelas, utilizando o "REGISTRO" como chave de junção, primeira tabela mencionada fica a esquerda
head(tab_hemo_leuco_plaq)

#5- Organização dos nomes da tabela final
colunas_a_renomear <- c(2, 13, 20)
novos_nomes <- c("Sexo", "Neutrófilos", "Observações leucocitárias citomorfológicas")

# Renomear as colunas
colnames(tab_hemo_leuco_plaq)[colunas_a_renomear] <- novos_nomes

#5- Escrever final em excel

# Salvar arquivo e indicar onde foi
write_xlsx(tab_hemo_leuco_plaq,"C:/Users/mathe/OneDrive/Documentos/R/Resumos infecto 2023/Resultados_e_TabelasLimpas/tabela_completa.xlsx")
caminho_arquivo_excel <- "C:/Users/mathe/OneDrive/Documentos/R/Resumos infecto 2023/Resultados_e_TabelasLimpas/tabela_completa.xlsx"


# Verifique se o arquivo já existe
if (file.exists(caminho_arquivo_excel)) {
  cat("O arquivo", caminho_arquivo_excel, "já existe.\n")
  
  # Pergunte ao usuário se deseja sobrescrevê-lo
  resposta <- readline(prompt = "Deseja sobrescrever o arquivo? (s/n): ")
  resposta <- tolower(resposta)
  
  if (resposta == "s" || resposta == "s") {
    write_xlsx(tab_hemo_leuco_plaq, caminho_arquivo_excel)
    cat("O arquivo foi sobrescrito com sucesso!\n")
  } else {
    cat("Operação cancelada. O arquivo não foi sobrescrito.\n")
  }
} else {
  write_xlsx(tab_hemo_leuco_plaq, caminho_arquivo_excel)
  cat("O arquivo foi salvo com sucesso!\n")
}
