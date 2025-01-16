library(genetics)
library(dplyr)
library(stringr)
library(epitools)
library(openxlsx)

# Frequências observadas para a rs10748643
frequencias <- matrix(c(0.65, 0.44,   # HAM
                        0.65, 0.47,   # OLIGO/ASS
                        0.65, 0.40),  # GC
                      nrow = 3, byrow = TRUE)

# Multiplicar as frequências por 100 para obter as contagens inteiras
contagens <- frequencias * 100

# Nomeando as linhas da tabela
rownames(contagens) <- c("HAM", "OLIGO/ASS", "GC")

# Função para rodar o teste de Fisher para cada linha
comparar_linhas_fisher <- function(freq_esperada, freq_observada) {
  # Criar tabela de contingência com as frequências esperadas e observadas
  tabela <- matrix(c(freq_esperada, 100 - freq_esperada,  # Esperados
                     freq_observada, 100 - freq_observada),  # Observados
                   nrow = 2, byrow = TRUE)
  
  # Rodar o teste de Fisher
  resultado_fisher <- fisher.test(tabela)
  return(resultado_fisher)
}

# Aplicar o teste de Fisher para cada linha da tabela
for (i in 1:nrow(contagens)) {
  freq_esperada <- contagens[i, 1]  # Coluna FR AMR - VEP
  freq_observada <- contagens[i, 2]  # Coluna rs10748643
  
  resultado_fisher <- comparar_linhas_fisher(freq_esperada, freq_observada)
  
  # Exibir o resultado com o nome da linha
  cat("\nResultado de Fisher para", rownames(contagens)[i], ":\n")
  print(resultado_fisher)
}

# Frequências observadas para a rs11188513
frequencias <- matrix(c(0.73, 0.69,   # HAM
                        0.73, 0.66,   # OLIGO/ASS
                        0.73, 0.70),  # GC
                      nrow = 3, byrow = TRUE)

# Multiplicar as frequências por 100 para obter as contagens inteiras
contagens <- frequencias * 100

# Nomeando as linhas da tabela
rownames(contagens) <- c("HAM", "OLIGO/ASS", "GC")

# Função para rodar o teste de Fisher para cada linha
comparar_linhas_fisher <- function(freq_esperada, freq_observada) {
  # Criar tabela de contingência com as frequências esperadas e observadas
  tabela <- matrix(c(freq_esperada, 100 - freq_esperada,  # Esperados
                     freq_observada, 100 - freq_observada),  # Observados
                   nrow = 2, byrow = TRUE)
  
  # Rodar o teste de Fisher
  resultado_fisher <- fisher.test(tabela)
  return(resultado_fisher)
}

# Aplicar o teste de Fisher para cada linha da tabela
for (i in 1:nrow(contagens)) {
  freq_esperada <- contagens[i, 1]  # Coluna FR AMR - VEP
  freq_observada <- contagens[i, 2]  # Coluna rs10748643
  
  resultado_fisher <- comparar_linhas_fisher(freq_esperada, freq_observada)
  
  # Exibir o resultado com o nome da linha
  cat("\nResultado de Fisher para", rownames(contagens)[i], ":\n")
  print(resultado_fisher)
}




