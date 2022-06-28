library(tidyverse)
library(glue)

options(scipen = 999)

source("R/arrange.R")
source("R/calc.R")
source('R/results.R')

# Dados de entrada --------------------------------------------------------

lista_municipios <- read_csv2("input/final_table.csv")
lista_outliers <- read_csv("input/outliers_table.csv")
populacao_antigo <- read_csv("input/populacao2019-8-7.csv")
populacao_2020 <- read_csv2("input/pop2020.csv")

frota <- 
  paste0("input/", list.files("input/", pattern = "^frota")) %>%
  map(readxl::read_excel)

# Organizando os dados ----------------------------------------------------

lista_populacao <- arrange_populacao(populacao_antigo, populacao_2020)

lista_frota <- 
  map2(frota, c(2018, 2019, 2020), ~extract_frota(.x, .y)) %>% 
  reduce(bind_rows) %>% 
  calc_media_frota()

lista_municipios <- 
  lista_municipios %>% 
  mutate(ibge_cod = as.character(ibge_cod)) %>% 
  select(ibge_cod:snt, media_mortes, pc1 = a_pc1, prioridade) %>% 
  left_join(lista_populacao, by = "ibge_cod") %>% 
  left_join(lista_frota, by = "ibge_cod")

lista_outliers <- 
  lista_outliers %>% 
  select(ibge_cod:snt, media_mortes)

# Calculo das metas -------------------------------------------------------

lista_clusters <- expand_grid(
  porte = c("Menor porte", "Médio porte", "Maior porte"),
  prioridade = seq(1,27,1)
)

clusters <- map2(
  lista_clusters$porte,
  lista_clusters$prioridade,
  ~split_clusters(lista_municipios, .x, .y)
)

empty_clusters <- map(clusters, check_empty) %>% unlist()
empty_position <- which(empty_clusters == TRUE)
lista_clusters <- lista_clusters %>% slice(-empty_position)
clusters <- clusters[-c(empty_position)]

lista_refs <- 
  pmap(
    list(
      clusters,
      lista_clusters$porte,
      lista_clusters$prioridade
    ),
    calc_referencia
  ) %>% 
  reduce(bind_rows) %>% 
  as.list()

lista_metas <- pmap(
  list(
    clusters,
    lista_clusters$porte,
    lista_clusters$prioridade,
    lista_refs$mortes_ref,
    lista_refs$frota_ref,
    lista_refs$pop_ref
  ),
  calc_metas
) %>% 
  reduce(bind_rows)

lista_metas <- calc_variacao(lista_metas)

lista_metas_neg <- lista_metas %>% filter(var_perc < 0)

lista_metas_positivas <- fix_metas_positivas(lista_metas)

lista_metas_fixed <- apply_metas_fix(lista_metas, lista_metas_positivas)

municipios_metas <- bind_rows(lista_metas_neg, lista_metas_fixed)

metas_uf <- calc_metas_uf(municipios_metas)

lista_outliers <- add_metas_outlier(metas_uf, lista_outliers)

municipios_metas <- bind_rows(
  municipios_metas,
  lista_outliers %>% mutate(ibge_cod = as.character(ibge_cod))
)

# Resultados --------------------------------------------------------------

mortes_br <- sum(municipios_metas$media_mortes)
meta_br <- sum(municipios_metas$meta_round)
var_br <- meta_br - mortes_br
var_perc_br <- var_br / mortes_br

metas_porte <- plot_metas_porte(municipios_metas)
metas_uf <- plot_metas_uf(municipios_metas)

map2(
  c("plot/metas_porte.png", "plot/metas_uf.png"),
  list(metas_porte, metas_uf),
  ~ggsave(.x, .y, device = "png", width = 6, height = 3.5)
)

calc_perc(municipios_metas, porte)
calc_perc(municipios_metas, regiao)
calc_perc(municipios_metas, snt)

# Exportar resultados -----------------------------------------------------

write_csv(municipios_metas, "output/municipios_metas.csv")
