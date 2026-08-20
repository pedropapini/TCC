#conseguir o número de estab (ativos). por ano e bairro
##Selecionar os anos de 2000 e 2010
anos <- c(2000, 2010)

##Ler a base de dados
alvaras_filtrado <- read_delim("C:/Users/PICHAU/Desktop/Coisas Pedro/UFRGS/TCC/alvaras_com_atividades_filtradas.csv", delim = ";")

##Formatar o estilo de data utilizado, e selecionar suas respectivas colunas
alvaras_filtrado <- alvaras_filtrado %>%
  mutate(
    data_abertura = as.Date(data_deferimento, format = "%d/%m/%Y"),
    data_fim = as.Date(data_vencimento, format = "%d/%m/%Y")
  )

##Função para verificar se um alvará estava ativo em um ano específico, e filtrar por ano
alvaras_ativos_por_ano <- alvaras_filtrado %>%
  rowwise() %>%
  mutate(
    ativo_2000 = (data_abertura <= as.Date("2000-12-31")) & 
      (is.na(data_fim) | data_fim >= as.Date("2000-01-01")),
    ativo_2010 = (data_abertura <= as.Date("2010-12-31")) & 
      (is.na(data_fim) | data_fim >= as.Date("2010-01-01"))
  ) %>%
  ungroup()

##Função para verificar se um alvará estava ativo em um bairro específico, e filtrar por bairro
alvaras_por_ano_bairro <- alvaras_ativos_por_ano %>%
  group_by(bairro) %>%
  summarise(
    qtd_ativos_2000 = sum(ativo_2000),
    qtd_ativos_2010 = sum(ativo_2010),
    .groups = "drop"
  )

##Escrever nova base de dados com os número por bairros e anos
write_delim(alvaras_por_ano_bairro,
            "C:/Users/PICHAU/Desktop/Coisas Pedro/UFRGS/TCC/alvaras_ativos_2000_2010_por_bairro_com_bairros_novos.csv",
            delim = ";")
