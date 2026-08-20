#Regressão de Efeitos Fixos
##Selecionar o diretório
dir_base <- "C:/Users/pedro.papini/Desktop/Coisas Pedro/UFRGS/TCC"

##Selecionar a base de dados
arq <- file.path(dir_base, "base_FINAL_PARA_A_REGRESSAO.csv")

##Padronizar as colunas
parse_num_misto <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NaN", "NULL", "-")] <- NA_character_
  
  # mantém apenas dígitos, sinal, vírgula e ponto
  x <- gsub("[^0-9,\\.\\-]", "", x)
  
  # caso 1: tem vírgula e ponto -> assume que o último separador é decimal
  both <- grepl(",", x) & grepl("\\.", x)
  if (any(both, na.rm = TRUE)) {
    xb <- x[both]
    dec_comma <- regexpr(",[^,]*$", xb) > regexpr("\\.[^.]*$", xb) # vírgula é o último separador
    # decimal vírgula: 1.234,56 -> 1234.56
    xb[dec_comma] <- gsub("\\.", "", xb[dec_comma])
    xb[dec_comma] <- sub(",", ".", xb[dec_comma], fixed = TRUE)
    # decimal ponto: 1,234.56 -> 1234.56
    xb[!dec_comma] <- gsub(",", "", xb[!dec_comma])
    x[both] <- xb
  }
  
  # caso 2: só vírgula -> vírgula decimal
  only_comma <- grepl(",", x) & !grepl("\\.", x)
  x[only_comma] <- sub(",", ".", x[only_comma], fixed = TRUE)
  
  suppressWarnings(as.numeric(x))
}

## 1) Limpa nomes e remove colunas "..." (lixo do CSV)
df_wide <- read_delim(
  arq,
  delim = ";",
  locale = locale(encoding = "UTF-8", decimal_mark = ","),
  show_col_types = FALSE,
  trim_ws = TRUE
) %>%
  rename_with(~str_trim(.x)) %>%
  select(-matches("^\\.\\.\\.[0-9]+$")) %>%   # remove ...1, ...2, etc.
  select(where(~ !all(is.na(.x))))

## 2) Converte TODAS as colunas numéricas de interesse
num_cols <- c("estab_2000","hab_2000","idhm_2000","renda_2000", "idhm_logi_2000", "idhm_educ_2000",
              "estab_2010","hab_2010","idhm_2010","renda_2010", "idhm_logi_2010", "idhm_educ_2010")
df_wide <- df_wide %>%
  mutate(across(all_of(num_cols), parse_num_misto))


## 3) Long -> Wide com agregação explícita para duplicatas
painel <- df_wide %>%
  pivot_longer(
    cols = all_of(num_cols),
    names_to = c("var", "ano"),
    names_pattern = "(estab|hab|idhm|renda|idhm_educ|idhm_logi)_(2000|2010)",
    values_to = "valor"
  ) %>%
  mutate(
    ano = as.integer(ano),
    valor = as.numeric(valor)
  ) %>%
  group_by(bairro, ano, var) %>%
  summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") %>%  # resolve duplicatas
  pivot_wider(names_from = var, values_from = valor) %>%
  mutate(
    estab = as.numeric(estab),
    hab   = as.numeric(hab),
    idhm  = as.numeric(idhm),
    idhm_educ  = as.numeric(idhm_educ),
    idhm_logi  = as.numeric(idhm_logi),
    renda = as.numeric(renda),
    estab_por_hab = (estab / hab)
  ) %>%
  filter(is.finite(estab_por_hab))

##(OPCIONAL) Defina o corte mínimo de população
pop_min <- 1000 #normal é deixar 0

###(OPCIONAL) Mantém apenas bairros cuja menor população no período é >= pop_min
painel_filtrado <- painel %>%
  group_by(bairro) %>%
  filter(min(hab, na.rm = TRUE) >= pop_min) %>%
  ungroup()

#MODELO 1
##MODELOS COM IDHM COMO VARIÁVEL DEPENDENTE
###Modelo TWFE: idhm ~ estab_noturnos por hab
m_idhm_pc <- feols(idhm ~ estab_por_hab | bairro + ano,
                    data = painel_filtrado, cluster = ~bairro)

###Modelo TWFE: idhm ~ estab_noturnos (absoluto)
m_idhm_abs <- feols(idhm ~ estab + hab | bairro + ano,
                     data = painel_filtrado, cluster = ~bairro)

###(Robustez) absoluto + renda média
m_idhm_abs_pop <- feols(idhm ~ estab_por_hab + renda | bairro + ano,
                         data = painel_filtrado, cluster = ~bairro)

###RESULTADOS 
etable(m_idhm_pc, m_idhm_abs, m_idhm_abs_pop,
       dict = c(
         "estab_por_hab" = "Noturnos por hab.",
         "estab"           = "Nº de noturnos",
         "renda"        = "Renda Média",
         "hab"          = "População"
       ),
       fitstat = ~ n + r2 + wr2,
       se.below = TRUE)



#MODELO 2
##MODELOS COM LOG(IDHM) COMO VARIÁVEL DEPENDENTE
###Modelo TWFE: idhm (log) ~ estab_noturnos por hab
m_logidhm_pc <- feols(log(idhm) ~ estab_por_hab | bairro + ano,
                   data = painel_filtrado, cluster = ~bairro)

###Modelo TWFE: idhm (log) ~ estab_noturnos (absoluto)
m_logidhm_abs <- feols(log(idhm) ~ estab + hab| bairro + ano,
                    data = painel_filtrado, cluster = ~bairro)

###(Robustez) absoluto + renda média
m_logidhm_abs_pop <- feols(log(idhm) ~ estab_por_hab + renda | bairro + ano,
                        data = painel_filtrado, cluster = ~bairro)

###RESULTADOS
etable(m_logidhm_pc, m_logidhm_abs, m_logidhm_abs_pop,
       dict = c(
         "estab_por_hab" = "Noturnos por hab.",
         "estab"           = "Nº de noturnos",
         "renda"        = "Renda Média",
         "hab"          = "População"
       ),
       fitstat = ~ n + r2 + wr2,
       se.below = TRUE)



#MODELO 3
##MODELOS COM RENDA COMO VARIÁVEL DEPENDENTE
###Modelo TWFE: renda ~ estab_noturnos por hab
m_renda_pc <- feols(renda ~ log(estab_por_hab) | bairro + ano,
                   data = painel_filtrado, cluster = ~bairro)

###Modelo TWFE: renda ~ estab_noturnos (absoluto)
m_renda_abs <- feols(renda ~ log(estab) | bairro + ano,
                    data = painel_filtrado, cluster = ~bairro)

###(Robustez) absoluto + controle de tamanho
m_renda_abs_pop <- feols(renda ~ log(estab) + hab | bairro + ano,
                        data = painel_filtrado, cluster = ~bairro)

###RESULTADOS
etable(m_renda_pc, m_renda_abs, m_renda_abs_pop,
       dict = c(
         "estab_por_hab" = "Noturnos por hab.",
         "estab"           = "Nº de noturnos",
         "hab"        = "População",
         "log(estab_por_hab)" = "Log(estab_por_hab)",
         "log(estab)"      = "Log(estab)"
       ),
       fitstat = ~ n + r2 + wr2,
       se.below = TRUE)



#MODELO 4
##MODELOS COM LOG(RENDA) COMO VARIÁVEL DEPENDENTE
####Modelo TWFE: renda ~ estab_noturnos por hab
m_logrenda_pc <- feols(log(renda) ~ log(estab_por_hab) | bairro + ano,
                   data = painel_filtrado, cluster = ~bairro)

###Modelo TWFE: renda ~ estab_noturnos (absoluto)
m_logrenda_abs <- feols(log(renda) ~ log(estab) | bairro + ano,
                    data = painel_filtrado, cluster = ~bairro)

###(Robustez) absoluto + controle de tamanho
m_logrenda_abs_pop <- feols(log(renda) ~ log(estab) + hab | bairro + ano,
                        data = painel_filtrado, cluster = ~bairro)

###RESULTADOS
etable(m_logrenda_pc, m_logrenda_abs, m_logrenda_abs_pop,
       dict = c(
         "estab_por_hab" = "Noturnos por hab.",
         "estab"           = "Nº de noturnos",
         "hab"        = "População",
         "log(estab_por_hab)" = "Log(estab_por_hab)",
         "log(estab)"      = "Log(estab)"
       ),
       fitstat = ~ n + r2 + wr2,
       se.below = TRUE)



#MODELOS DAS OUTRAS VARIÁVEIS DA RENDA
##EDUC
m_idhm_educ_pc <- feols(idhm_educ ~ estab_por_hab | bairro + ano,
                      data = painel_filtrado, cluster = ~bairro)

m_idhm_educ_abs <- feols(idhm_educ ~ estab | bairro + ano,
                        data = painel_filtrado, cluster = ~bairro)


m_idhm_educ_pop <- feols(idhm_educ ~ estab + hab | bairro + ano,
                            data = painel_filtrado, cluster = ~bairro)

###RESULTADOS
etable(m_idhm_educ_pc, m_idhm_educ_abs, m_idhm_educ_pop,
       dict = c(
         "estab_por_hab" = "Noturnos por hab.",
         "estab"           = "Nº de noturnos",
         "renda"        = "Renda Média",
         "hab"          = "População"
       ),
       fitstat = ~ n + r2 + wr2,
       se.below = TRUE)


##Logi
m_idhm_logi_pc <- feols(idhm_logi ~ estab_por_hab | bairro + ano,
                       data = painel_filtrado, cluster = ~bairro)

m_idhm_logi_abs <- feols(idhm_logi ~ estab | bairro + ano,
                         data = painel_filtrado, cluster = ~bairro)

m_idhm_logi_pop <- feols(idhm_logi ~ estab + hab | bairro + ano,
                         data = painel_filtrado, cluster = ~bairro)

###RESULTADOS
etable(m_idhm_logi_pc, m_idhm_logi_abs, m_idhm_logi_pop,
       dict = c(
         "estab_por_hab" = "Noturnos por hab.",
         "estab"           = "Nº de noturnos",
         "renda"        = "Renda Média",
         "hab"          = "População"
       ),
       fitstat = ~ n + r2 + wr2,
       se.below = TRUE)


#MODELO DA MEDIANA
# --- definir grupo por mediana (use idhm_2000 como baseline) ---
med_idhm2000 <- painel_filtrado %>%
  filter(ano == 2000) %>%
  summarise(med = median(idhm, na.rm = TRUE)) %>%
  pull(med)

bairro_grupo <- painel_filtrado %>%
  filter(ano == 2000) %>%
  transmute(bairro, alto_idhm = idhm > med_idhm2000)

painel_g <- painel_filtrado %>%
  left_join(bairro_grupo, by = "bairro") %>%
  mutate(grupo = ifelse(alto_idhm, "Alto IDHM (>= mediana)", "Baixo IDHM (< mediana)"))

# --- estimar separadamente ---
m_low_pc <- feols(idhm ~ estab_por_hab | bairro + ano, data = subset(painel_g, grupo == "Baixo IDHM (< mediana)"),
                cluster = ~bairro)

m_low_abs <- feols(idhm ~ estab | bairro + ano, data = subset(painel_g, grupo == "Baixo IDHM (< mediana)"),
                   cluster = ~bairro)

m_low_pop <- feols(idhm ~ estab + hab | bairro + ano, data = subset(painel_g, grupo == "Baixo IDHM (< mediana)"),
                   cluster = ~bairro)



m_high_pc <- feols(idhm ~ estab_por_hab | bairro + ano, data = subset(painel_g, grupo == "Alto IDHM (>= mediana)"),
                cluster = ~bairro)

m_high_abs <- feols(idhm ~ estab | bairro + ano, data = subset(painel_g, grupo == "Alto IDHM (>= mediana)"),
                    cluster = ~bairro)

m_high_pop <- feols(idhm ~ estab + hab | bairro + ano, data = subset(painel_g, grupo == "Alto IDHM (>= mediana)"),
                    cluster = ~bairro)

etable(m_low_pc, m_low_abs, m_low_pop, m_high_pc, m_high_abs, m_high_pop,
       dict = c("estab_por_hab"="Noturnos por hab."),
       fitstat = ~ n + r2 + wr2,
       se.below = TRUE)