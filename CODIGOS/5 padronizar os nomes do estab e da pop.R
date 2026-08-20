#Padronizar nome da planilha do estab.
##Carregar a planilha com os dados
dados <- read.csv("C:/Users/PICHAU/Desktop/Coisas Pedro/UFRGS/TCC/alvaras_ativos_2000_2010_por_bairro_sem_bairros_novos.csv", sep = ";", stringsAsFactors = FALSE)

##Substituir os nomes dos Bairros
dados <- dados %>%
  mutate(bairro = ifelse(bairro == "AGRONOMIA", "Agronomia", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "ANCHIETA", "Anchieta", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "ARQUIPELAGO", "Arquipélago", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "AUXILIADORA", "Auxiliadora", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "AZENHA", "Azenha", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "BELA VISTA", "Bela Vista", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "BELEM NOVO", "Belém Novo", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "BELEM VELHO", "Belém Velho", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "BOA VISTA", "Boa Vista", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "BOM FIM", "Bom Fim", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "BOM JESUS", "Bom Jesus", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "CAMAQUA", "Camaquã", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "CAMPO NOVO", "Campo Novo", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "CASCATA", "Cascata", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "CAVALHADA", "Cavalhada", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "CEL AP BORGES", "Cel Aparício Borges", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "CENTRO HISTORIC", "Centro Histórico", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "CHACARA PEDRAS", "Chácara das Pedras", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "CHAPEU DO SOL", "Chapéu do Sol", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "CIDADE BAIXA", "Cidade Baixa", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "CRISTAL", "Cristal", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "CRISTO REDENTOR", "Cristo Redentor", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "ESPIRITO SANTO", "Espírito Santo", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "FARRAPOS", "Farrapos", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "FARROUPILHA", "Farroupilha", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "FLORESTA", "Floresta", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "GLORIA", "Glória", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "GUARUJA", "Guarujá", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "HIGIENOPOLIS", "Higienópolis", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "HIPICA", "Hípica", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "HUMAiTA", "Humaitá", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "INDEPENDENCIA", "Independência", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "IPANEMA", "Ipanema", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "JARDIM BOTANICO", "Jardim Botânico", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "JARDIM CARVALHO", "Jardim Carvalho", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "JARDIM DO SALSO", "Jardim do Salso", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "JARDIM FLORESTA", "Jardim Floresta", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "JARDIM ISABEL", "Jardim Isabel", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "JARDIM ITU", "Jardim Itu", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "JARDIM LINDOIA", "Jardim Lindóia", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "JARDIM SABARA", "Jardim Sabará", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "JAR SAO PEDRO", "Jardim São Pedro", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "LAGEADO", "Lageado", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "LAMI", "Lami", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "LOMBA PINHEIRO", "Lomba do Pinheiro", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "MARIO QUINTANA", "Mário Quintana", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "MEDIANEIRA", "Medianeira", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "MENINO DEUS", "Menino Deus", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "MOINHOS VENTO", "Moinhos de Vento", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "MONT SERRAT", "Mont Serrat", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "MORRO SANTANA", "Morro Santana", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "NAVEGANTES", "Navegantes", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "NONOAI", "Nonoai", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "PARTENON", "Partenon", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "PASSO DA AREIA", "Passo Da Areia", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "PASSO DAS PEDRA", "Passo das Pedras", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "PEDRA REDONDA", "Pedra Redonda", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "PETROPOLIS", "Petrópolis", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "PONTA GROSSA", "Ponta Grossa", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "PRAIA DE BELAS", "Praia de Belas", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "RESTINGA", "Restinga", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "RIO BRANCO", "Rio Branco", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "RUBEM BERTA", "Rubem Berta", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "SANTA CECILIA", "Santa Cecília", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "SANTA M GORETTI", "Santa Maria Goretti", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "SANTA TEREZA", "Santa Tereza", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "SANTANA", "Santana", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "SANTO ANTONIO", "Santo Antônio", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "SAO GERALDO", "São Geraldo", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "SAO JOAO", "São João", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "VILA SAO JOSE", "São José", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "SAO SEBASTIAO", "São Sebastião", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "SARANDI", "Sarandi", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "SERRARIA", "Serraria", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "TERESOPOLIS", "Teresópolis", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "TRES FIGUEIRAS", "Três Figueiras", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "TRISTEZA", "Tristeza", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "VILA ASSUNCAO", "Vila Assunção", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "VILA CONCEICAO", "Vila Conceição", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "VILA IPIRANGA", "Vila Ipiranga", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "VILA JARDIM", "Vila Jardim", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "VL JOAO PESSOA", "Vila João Pessoa", bairro))

dados <- dados %>%
  mutate(bairro = ifelse(bairro == "VILA NOVA", "Vila Nova", bairro))

##Retirar os espaços, e deixar as letras em maiúsculo
dados$bairro <- dados$bairro |>
  str_trim() |>      # remove espaços
  str_to_upper()     # deixa maiúsculo
dados$bairro <- iconv(dados$bairro, from = "UTF-8", to = "ASCII//TRANSLIT") #retirar os acentos e Ç

##Salvar nova base
write_csv2(dados, "C:/Users/PICHAU/Desktop/Coisas Pedro/UFRGS/TCC/alvaras_ativos_2000_2010_por_bairro_sem_bairros_novos_com_nomes_padronizados.csv")




#Padronizar nome da planilha do pop.
##Transformar os números em números inteiros
to_int_br <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "...", "NA", "N/A", "-")] <- NA_character_
  x <- gsub("[^0-9]", "", x)  # 10.180 -> 10180
  as.integer(x)
}

##Limpa os nomes do bairro para realizar a padronização
clean_key <- function(x) {
  x %>%
    str_squish() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%  # remove acento sem iconv "from" problemático
    str_to_upper()
}

##Ler a Base de dados
dados <- read_delim(
  "C:/Users/PICHAU/Desktop/Coisas Pedro/UFRGS/TCC/população_bairro_n_padronizada_sem_isabel.csv",
  delim = ";",
  locale = locale(encoding = "Latin1"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

##Retira os acentos e transforma em letras maiúsculas
dados <- dados %>%
  mutate(
    bairro_raw = bairro,
    bairro_key = clean_key(bairro),
    bairro = case_when(
      bairro_key == "CEL. APARICIO BORGES" ~ "CEL APARICIO BORGES",
      bairro_key == "ESPIRITO SANTO"       ~ "ESPIRITO SANTO",
      bairro_key == "SANTA CECILIA"        ~ "SANTA CECILIA",
      bairro_key == "MONT 'SERRAT"         ~ "MONT SERRAT",
      bairro_key == "PASSO D'AREIA"        ~ "PASSO DA AREIA",
      TRUE ~ bairro_key
    ),
    `2000` = coalesce(to_int_br(`2000`), 0L),
    `2010` = coalesce(to_int_br(`2010`), 0L)
  ) %>%
  select(-bairro_raw, -bairro_key)

##Correções em específico
dados <- dados %>%
  mutate(bairro = ifelse(bairro == "CEL. APARI-CIO BORGES", "CEL APARICIO BORGES", bairro))
dados <- dados %>%
  mutate(bairro = ifelse(bairro == "ESPI-RITO SANTO", "ESPIRITO SANTO", bairro))
dados <- dados %>%
  mutate(bairro = ifelse(bairro == "SANTA CECI-LIA", "SANTA CECILIA", bairro))

##Criar nova base de dados com os nomes padronizados
write_delim(
  dados,
  "C:/Users/PICHAU/Desktop/Coisas Pedro/UFRGS/TCC/população_bairro_s_padronizada.csv",
  delim = ";"
)