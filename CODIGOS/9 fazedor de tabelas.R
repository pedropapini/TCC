#Fazer TABELAS
##Instalar se necessário
install.packages(c("flextable", "officer"))

library(readr)
library(dplyr)
library(stringr)
library(flextable)
library(officer)

##Exemplo de tabela
dir_base <- "C:/Users/PICHAU/Desktop/UFRGS/Coisas Pedro/"
arq <- file.path(dir_base, "atividades_que_mais_aparecem.csv")

# 1) Ler CSV (separador ; e decimal ,)
df_wide <- read_delim(
  arq,
  delim = ";",
  locale = locale(encoding = "UTF-8", decimal_mark = ","),
  show_col_types = FALSE,
  trim_ws = TRUE
) %>%
  select(where(~ !all(is.na(.x)))) %>%   # remove colunas vazias extras
  mutate(
    #estab_2000 = as.numeric(estab_2000),
    #hab_2000   = as.numeric(hab_2000),
    #idhm_2000  = as.numeric(idhm_2000),
    renda_2000 = as.numeric(renda_2000),
    
    #estab_2010 = as.numeric(estab_2010),
    #hab_2010   = as.numeric(hab_2010),
    #idhm_2010  = as.numeric(idhm_2010),
    renda_2010 = as.numeric(renda_2010)
  )


#ESCOLHER BAIRRO
#bairro_escolhido <- "CIDADE BAIXA"

#tabela <- df_wide %>%
  #filter(str_to_upper(str_trim(bairro)) == str_to_upper(str_trim(bairro_escolhido)))

tabela <- df_wide

##Cria flextable
ft <- flextable(tabela)
ft <- autofit(ft)

##Cria documento Word e adiciona tabela
doc <- read_docx()
doc <- body_add_par(doc, "Tabela exportada do R", style = "heading 1")
doc <- body_add_flextable(doc, ft)

##Salva .docx
print(doc, target = "tabela_r.docx")







# =====================================
# Tabela geral: 12 modelos em uma saída
# =====================================

# (Opcional, mas recomendado) filtrar base positiva para modelos em log:
painel_log <- painel_filtrado %>%
  dplyr::filter(idhm > 0, renda > 0, estab > 0, estab_por_hab > 0)

# Reestimar os de log na base positiva (evita -Inf/NaN)
m_logidhm_pc       <- feols(log(idhm) ~ estab_por_hab | bairro + ano, data = painel_log, cluster = ~bairro)
m_logidhm_abs      <- feols(log(idhm) ~ estab + hab       | bairro + ano, data = painel_log, cluster = ~bairro)
m_logidhm_abs_pop  <- feols(log(idhm) ~ estab_por_hab + renda | bairro + ano, data = painel_log, cluster = ~bairro)

m_logrenda_pc      <- feols(log(renda) ~ log(estab_por_hab) | bairro + ano, data = painel_log, cluster = ~bairro)
m_logrenda_abs     <- feols(log(renda) ~ log(estab)         | bairro + ano, data = painel_log, cluster = ~bairro)
m_logrenda_abs_pop <- feols(log(renda) ~ estab + hab        | bairro + ano, data = painel_log, cluster = ~bairro)

# Tabela única comparando tudo
mods <- list(
  m_idhm_pc, m_idhm_abs, m_idhm_abs_pop,
  m_logidhm_pc, m_logidhm_abs, m_logidhm_abs_pop,
  m_renda_pc, m_renda_abs, m_renda_abs_pop,
  m_logrenda_pc, m_logrenda_abs, m_logrenda_abs_pop
)

fixest::etable(
  . = mods,
  headers = c(
    "IDHM", "IDHM", "IDHM",
    "log(IDHM)", "log(IDHM)", "log(IDHM)",
    "RENDA", "RENDA", "RENDA",
    "log(RENDA)", "log(RENDA)", "log(RENDA)"
  ),
  dict = c(
    "estab_por_hab"      = "Noturnos por hab.",
    "log(estab_por_hab)" = "log(Noturnos por hab.)",
    "estab"              = "Nº de noturnos",
    "log(estab)"         = "log(Nº de noturnos)",
    "hab"                = "População",
    "renda"              = "Renda"
  ),
  fitstat = ~ n + r2 + wr2,
  se.below = TRUE,
  digits = 4
)