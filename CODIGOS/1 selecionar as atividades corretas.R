#Selecionar as atividades noturnas
##Instalar os pacotes que serão utilizados
install.packages(c("readr", "dplyr", "lubridate", "flextable", "stringr", "stringi", "officer", "tidyr", "fixest"))

##Chamar a library
library(flextable)
library(officer)
library(readr)
library(dplyr)
library(stringr)
library(stringi)
library(tidyr)
library(fixest)

##Ler a base de dados da Prefeitura
alvaras <- read_delim("C:/Users/PICHAU/Desktop/Coisas Pedro/UFRGS/TCC/alvaras_ativos.csv", delim = ";")

##Selecionar somente os dados de atividade
lista_de_atividades <- alvaras %>%
  distinct(atividade) %>%
  arrange(atividade) %>%
  pull()

##Selecionar as atividades que serão utilizadas
atividades_selecionadas <- lista_de_atividades[c(14, 17, 23, 32, 33, 34, 35, 36, 37, 39, 41, 43, 50, 70, 71, 72, 77, 78, 79, 83, 84, 85, 86, 87, 88, 97, 99, 100, 108, 109, 110, 111, 112, 113, 116, 122, 123, 127, 130, 132, 134, 138, 175, 176, 177, 178, 182, 187, 190, 194, 224, 270, 271, 274, 275, 302, 306, 342, 410, 467, 534, 562, 578, 586, 602, 618, 619, 620, 631, 632, 642, 654, 658, 782, 857, 858, 859, 868, 872, 898, 926, 945)]

##Filtrar as atividades na lista da Prefeitura
tabela_atividades <- tibble(atividade = atividades_selecionadas)
alvaras_filtrado <- alvaras %>%
  filter(atividade %in% atividades_selecionadas)

##Criar excel somente com os alvaras com as atividades filtradas
write_delim(alvaras_filtrado, 
            "C:/Users/PICHAU/Desktop/Coisas Pedro/UFRGS/TCC/alvaras_com_atividades_filtradas.csv",
            delim = ";")