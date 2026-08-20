#Juntar as duas planilhas pop. e estab.
##Escolher o diretório das pastas
dir_base <- "C:/Users/PICHAU/Desktop/Coisas Pedro/UFRGS/TCC"

##Ler a base de dados
arq_ativos <- file.path(dir_base, "alvaras_ativos_2000_2010_por_bairro_sem_bairros_novos_com_nomes_padronizados.csv")
arq_pop    <- file.path(dir_base, "população_bairro_s_padronizada.csv")

##Criar colunas estab.
ativos <- read_delim(
  arq_ativos, delim = ";",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE, trim_ws = TRUE
)

##Criar colunas pop.
pop <- read_delim(
  arq_pop, delim = ";",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE, trim_ws = TRUE
)

#Padronizar os espaços das colunas
padroniza_bairro <- function(x) {
  x %>%
    str_trim() %>%
    str_to_upper() %>%
    str_replace_all("//s+", " ") %>%         # espaços duplicados
    str_replace_all("[[:punct:]]", " ") %>%  # remove pontuação (.,-', etc.)
    str_replace_all("//s+", " ") %>%
    str_trim()
}
ativos2 <- ativos %>%
  mutate(bairro_key = padroniza_bairro(bairro)) %>%
  filter(!is.na(bairro_key), bairro_key != "-", bairro_key != "")
pop2 <- pop %>%
  mutate(bairro_key = padroniza_bairro(bairro)) %>%
  filter(!is.na(bairro_key), bairro_key != "")

## Junta e reorganiza as colunas
### bairro | estab_2000 | hab_2000 | estab_2010 | hab_2010
dados_final <- ativos2 %>%
  left_join(
    pop2 %>% select(bairro_key, bairro, `2000`, `2010`),
    by = "bairro_key"
  ) %>%
  transmute(
    # uma coluna de nome de bairro:
    # usa o nome "original" do arquivo de alvarás; se não existir, usa o da população
    bairro = coalesce(bairro.x, bairro.y),
    
    estab_2000 = qtd_ativos_2000,
    hab_2000   = `2000`,
    estab_2010 = qtd_ativos_2010,
    hab_2010   = `2010`
  )

##Checar quais ficaram sem população
nao_casados <- dados_final %>%
  filter(is.na(hab_2000) | is.na(hab_2010)) %>%
  distinct(bairro)
print(nao_casados)

##Criar uma nova planilha com os dados
readr::write_csv2(
  dados_final,
  file.path(dir_base, "ativos_pop_por_bairro_final.csv"),
  na = ""
)