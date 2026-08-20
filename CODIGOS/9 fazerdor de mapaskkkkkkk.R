library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)

bairros <- st_read("C:/Users/pedro.papini/Desktop/Coisas Pedro/Untitled map (1).kml")



pop <- read_csv("C:/Users/pedro.papini/Desktop/Coisas Pedro/população_bairro_s_padronizada_teste.csv", locale = locale(encoding = "ISO-8859-1"))
pop <- pop %>%
  separate(`bairro;2000;2010;diferença`,
           into = c("bairro", "2000", "2010", "Diferença"),
           sep = ";")


mapa <- bairros %>%
  left_join(pop, by = c("NOME" = "bairro"))

mapa$`2010` <- as.numeric(mapa$`2010`)
mapa$`2000` <- as.numeric(mapa$`2000`)
mapa$Diferença <- as.numeric(mapa$Diferença)


ggplot(mapa) +
  geom_sf(aes(fill = Diferença), color = "black") +
  scale_fill_gradient(low = "blue", high = "yellow", na.value = "grey80") +
  labs(
    title = "Diferença populacional por bairro - Porto Alegre (2010 e 2000)",
    fill = "Variação da população"
  ) +
  theme_minimal()