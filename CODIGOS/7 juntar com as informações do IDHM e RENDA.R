#Juntar com as informações da Renda e do IDHM
## Ler arquivos
idhm_renda <- read.csv("C:/Users/pedro.papini/Desktop/Coisas Pedro/UFRGS/TCC/IDHM e RENDA - Ponderada.csv",
                       sep = ";",
                       stringsAsFactors = FALSE)

ativos_pop <- read.csv("C:/Users/pedro.papini/Desktop/Coisas Pedro/UFRGS/TCC/ativos_pop_por_bairro_final.csv",
                       sep = ";",
                       stringsAsFactors = FALSE)

## Remover colunas vazias
idhm_renda <- idhm_renda[, !names(idhm_renda) %in% c("Unnamed: 5", "Unnamed: 6")]

## Juntar mantendo o formato da tabela ativos_pop
base_final <- merge(
  ativos_pop,
  idhm_renda,
  by = "bairro",
  all.x = TRUE
)

## Organizar colunas
base_final <- base_final[, c(
  "bairro",
  "estab_2000",
  "hab_2000",
  "estab_2010",
  "hab_2010",
  "idhm_2000",
  "idhm_2010",
  "idhm_logi_2000",
  "idhm_logi_2010",
  "idhm_educ_2000",
  "idhm_educ_2010",
  "renda_2000",
  "renda_2010"
)]

## Salvar
write.csv2(base_final,
          "base_FINAL_PARA_A_REGRESSAO.csv",
          row.names = FALSE)