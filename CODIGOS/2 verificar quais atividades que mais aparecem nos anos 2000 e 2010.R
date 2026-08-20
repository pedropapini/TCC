library(dplyr)
library(lubridate)
library(tidyr)

##Formatar o estilo de data utilizado, e selecionar suas respectivas colunas
parse_data_flex <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  out <- suppressWarnings(dmy(x))
  if (all(is.na(out))) out <- suppressWarnings(ymd(x))
  out
}

##ESCOLHER O BAIRRO, COM # É PORTO ALEGRE INTEIRA
bairro_escolhido <- "MOINHOS VENTO  "  # <- ajuste aqui

##Ver as atividades que mais aparecem    
tabela_bairro_2col <- alvaras_filtrado %>%
  filter(bairro == bairro_escolhido) %>%
  mutate(
    data_deferimento_dt = parse_data_flex(data_deferimento),
    ano_deferimento = year(data_deferimento_dt)
  ) %>%
  filter(!is.na(ano_deferimento)) %>%
  summarise(
    ate_2000 = sum(ano_deferimento <= 2000),
    ate_2010 = sum(ano_deferimento <= 2010),
    .by = atividade
  ) %>%
  arrange(desc(ate_2010), desc(ate_2000), atividade)

##Ver a tabela
tabela_bairro_2col

##Upar a tabela
##Escrever nova base de dados com os número por bairros e anos
write_delim(tabela_bairro_2col,
            "C:/Users/PICHAU/Desktop/Coisas Pedro/UFRGS/TCC/atividades_que_mais_aparecem.csv",
            delim = ";")