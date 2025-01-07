# Tutorial bem completo para usar de refer?ncia:
# https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/

# Banco de dados traduzido de:
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2018/2018-10-23/movie_profit.csv


# Carregamento dos pacotes
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(ggplot2, dplyr)

# Sele??o do diret?rio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

# Carregamento do banco de dados
dados <- read.csv2("C:/Users/mathe/OneDrive/Documentos/R/Video Aula GGPLOT2/LucroFilmes.csv", stringsAsFactors = T)

View(dados)

glimpse(dados)


## Modificando DataLancamento para o formato data
dados$DataLancamento <- as.Date(dados$DataLancamento, format = "%m/%d/%Y")

### Salvando o m?s e o ano em colunas separadas
dados <- dados %>% mutate(AnoLancamento = format(DataLancamento, "%Y"),
                          MesLancamento = format(DataLancamento, "%m"))



# Entendendo a l?gica das camadas do ggplot2

## As três principais camadas: dados, estética e geom !obrigatório os 3!
ggplot(data = dados, aes(x = LucroLocal, y = LucroMundial)) +
  geom_point()


## O aes pode ser definido na camada ggplot ou na geom_
## Opção de escrita 1
ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial))

## Opção de escrita 2
ggplot() +
  geom_point(data = dados, aes(x = LucroLocal, y = LucroMundial))


## Possibilidades de camadas de geom_: https://ggplot2.tidyverse.org/reference/

### Histograma (Orçamentos de produção) - Só precisa fornecer o eixo X
ggplot(data = dados) +
  geom_histogram(aes(x = Orcamento))


### Gr?fico de barras (Quantidade de filmes por g?nero)
ggplot(data = dados) +
  geom_bar(aes(x = Genero, y = ..count..))


### Boxplot (Lucro mundial por g?nero)
ggplot(data = dados) +
  geom_boxplot(aes(x = Genero, y = LucroMundial))


### Linhas (Quantidade de filmes por ano)
ggplot(data = dados) +
  geom_line(aes(x = AnoLancamento, group = 1), stat = "count")



## Modificando argumentos dentro do geom (color, shape, size)
### E a diferen?a entre us?-los dentro ou fora do aes

ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial, color = Genero))


ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial, color = Genero,
                 shape = Genero))


ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial, shape = Genero),
             color = "darkred")
# Cores pr?-definidas no R: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf



ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial, shape = Genero),
             color = "#61988E")
# Site gerador de paletas: https://coolors.co/



ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial),
             color = "#61988E", shape = 18)
# Shapes poss?veis: http://www.sthda.com/english/wiki/ggplot2-point-shapes


ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial),
             color = "#61988E", shape = 18, size = 1)



ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial),
             fill = "#61988E", color = "black", shape = 25, size = 1.5)
# Shape que permite color e fill


## Adicionando outro geom
### geom_line x geom_smooth

ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial),
             color = "#61988E", shape = 16, size = 0.7) +
  geom_line(aes(x = LucroLocal, y = LucroMundial), stat = "smooth",
            method = "lm")



ggplot(data = dados) +
  geom_point(aes(x = LucroLocal, y = LucroMundial),
             color = "#61988E", shape = 16, size = 0.7) +
  geom_smooth(aes(x = LucroLocal, y = LucroMundial), method = "lm", se = F,
              color = "black", size = 0.5)



# Deixando o c?digo mais enxuto
ggplot(data = dados, aes(x = LucroLocal, y = LucroMundial)) +
  geom_point(color = "#61988E", shape = 16, size = 0.7) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.5)



## Modificando a ordem das camadas (a ordem dos geoms importa!)
ggplot(data = dados, aes(x = LucroLocal, y = LucroMundial)) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.5) +
  geom_point(color = "#61988E", shape = 16, size = 0.7)



## Especificando aes espec?ficas para um dos geoms
ggplot(data = dados, aes(x = LucroLocal, y = LucroMundial)) +
  geom_point(color = "#61988E", shape = 16, size = 0.7) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.5,
              aes(linetype = Genero))



## Usando o filtro (dplyr) para selecionar dados para o gr?fico
dados %>% filter(Genero == "Terror") %>% 
ggplot(aes(x = LucroLocal, y = LucroMundial)) +
  geom_point(color = "#61988E", shape = 16, size = 0.7) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.5,
              aes(linetype = Genero))


dados %>% filter(Orcamento <= 9000000 & Classificacao == "PG") %>% 
  ggplot(aes(x = LucroLocal, y = LucroMundial)) +
  geom_point(color = "#61988E", shape = 16, size = 0.7) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.5,
              aes(linetype = Genero))



## Usando o geom para representar um "summary"
### (stat = summary) x stat_summary()


ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  geom_point(stat = "summary", fun = "mean")



ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  stat_summary(geom = "point", fun = "mean")



ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  stat_summary(geom = "point", fun = "median")



## Incluindo barras de erros (usando tamb?m o summary)


ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se")


ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.min = "min", fun.max = "max")


ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.3)


## Usando IC 95% ao inv?s de erro-padr?o (pacote ggpubr)

pacman::p_load(ggpubr)

ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_ci", width = 0.3)


ggplot(data = dados, aes(x = Genero, y = LucroLocal)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.3)


## Incluindo a classifica??o

dados %>% filter(Classificacao %in% c("PG", "PG-13", "R")) %>% 
ggplot(aes(x = Genero, y = LucroLocal, color = Classificacao)) +
  geom_point(stat = "summary", fun = "mean", position = position_dodge(0.4)) +
  geom_errorbar(stat = "summary", fun.data = "mean_ci", width = 0.3,
                position = position_dodge(0.4))


## Renomeando os eixos e legenda

dados %>% filter(Classificacao %in% c("PG", "PG-13", "R")) %>% 
  ggplot(aes(x = Genero, y = LucroLocal, color = Classificacao)) +
  geom_point(stat = "summary", fun = "mean", position = position_dodge(0.4)) +
  geom_errorbar(stat = "summary", fun.data = "mean_ci", width = 0.3,
                position = position_dodge(0.4)) +
  labs(y = "Lucro local (US$)", x = "G?nero do filme", color = "Classifica??o")



## Adicionando t?tulo, subt?tulo e legenda


dados %>% filter(Classificacao %in% c("PG", "PG-13", "R")) %>% 
  ggplot(aes(x = Genero, y = LucroLocal, color = Classificacao)) +
  geom_point(stat = "summary", fun = "mean", position = position_dodge(0.4)) +
  geom_errorbar(stat = "summary", fun.data = "mean_ci", width = 0.3,
                position = position_dodge(0.4)) +
  labs(y = "Lucro local (US$)", x = "G?nero do filme", color = "Classifica??o",
       title = "Lucro local em US$, de acordo com o g?nero e classifica??o do filme",
       subtitle = "Dados representados como m?dia e IC 95%",
       caption = "Fonte: FiveThirtyEight")

