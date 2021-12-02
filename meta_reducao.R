library(tidyverse)
library(readxl)
library(scales)
library(glue)

options(scipen = 999)

# Lista de municipios ----
municipios <- read_csv("input/fulltable_municipios.csv")

# Lista de clusters ----
lista_clusters <- read_csv("input/lista_clusters.csv")

lista_clusters <- lista_clusters %>% 
  mutate(porte = case_when(
    porte == "menor" ~ "Menor porte",
    porte == "medio" ~ "Médio porte",
    porte == "maior" ~ "Maior porte",
    TRUE ~ NA_character_ 
  ))

# Dados de frota ----

nome_frota <- list.files("input", pattern = "^frota")
nome_frota <- glue("input/{nome_frota}")

frota <- map(nome_frota, read_xls)

frota[[1]] <- frota[[1]] %>% 
  select(codigo, TOTAL) %>% 
  rename(total_2017 = TOTAL)

frota[[2]] <- frota[[2]] %>% 
  select(codigo, TOTAL) %>% 
  rename(total_2018 = TOTAL)

frota[[3]] <- frota[[3]] %>% 
  select(codigo, TOTAL) %>% 
  rename(total_2019 = TOTAL)

frota_total <- frota[[1]] %>% 
  left_join(frota[[2]], by = 'codigo') %>% 
  left_join(frota[[3]], by = 'codigo') %>% 
  mutate(frota_media = 
           as.numeric((total_2017 + total_2018 + total_2019)/3)) %>% 
  select(codigo, frota_media) %>% 
  mutate(codigo = as.numeric(codigo))

# Dados de populacao ----

populacao <- read_csv("input/populacao2019-8-7.csv")

populacao <- populacao %>% 
  separate(`Município`, into = c("codigo", NA), sep = 6) %>% 
  mutate(media_pop = as.numeric((`2017` + `2018` + `2019`)/3)) %>% 
  select(codigo, media_pop) %>% 
  mutate(codigo = as.numeric(codigo))

# Unindo tudo em municipios ----

municipios <- municipios %>% 
  left_join(frota_total, by = "codigo") %>% 
  left_join(populacao, by = "codigo")

# Funcao para extrair codigo de referencia ----

mun_check <- function(porte, A, B, C) {
  if (porte == "Menor porte") {
    tabela <- municipios %>% 
      filter(porte == "Menor porte",
             cluster_A == A,
             cluster_B == B,
             cluster_C == C) %>% 
      arrange(-PC1)
  } else if (porte == "Médio porte") {
    tabela <- municipios %>% 
      filter(porte == "Médio porte",
             cluster_A == A,
             cluster_B == B,
             cluster_C == C) %>% 
      arrange(PC1)
  } else {
    tabela <- municipios %>% 
      filter(porte == "Maior porte",
             cluster_A == A,
             cluster_B == B,
             cluster_C == C) %>% 
      arrange(-PC1)
  }
  tabela$codigo[1]
}

## Filtrando lista de clusters ----

criterios <- list(lista_clusters$porte, lista_clusters$cluster_A, lista_clusters$cluster_B,
                  lista_clusters$cluster_C)

lista_clusters$cod_ref <- pmap_dbl(criterios, mun_check)

mun_ref <- lista_clusters$cod_ref

lista_clusters <- lista_clusters %>% 
  drop_na(cod_ref) %>% 
  select(-cod_ref)

# Funcao para selecionar as mortes, populacao e frota de referencia  ----

select_mortes_ref <- function(porte, A, B, C) {
  if (porte == "Menor porte") {
    tabela <- municipios %>% 
      filter(porte == "Menor porte",
             cluster_A == A,
             cluster_B == B,
             cluster_C == C) %>% 
      arrange(-PC1)
  } else if (porte == "Médio porte") {
    tabela <- municipios %>% 
      filter(porte == "Médio porte",
             cluster_A == A,
             cluster_B == B,
             cluster_C == C) %>% 
      arrange(PC1)
  } else {
    tabela <- municipios %>% 
      filter(porte == "Maior porte",
             cluster_A == A,
             cluster_B == B,
             cluster_C == C) %>% 
      arrange(-PC1)
  }
  
  if (nrow(tabela) < 10) {
    tabela2 <- tabela %>% 
      slice_head() %>% 
      rename(mortes_ref = media_mortes,
             frota_ref = frota_media,
             pop_ref = media_pop) %>% 
      select(mortes_ref, frota_ref, pop_ref)
    
  } else {
    tabela2 <- tabela %>% 
      slice_head(prop = 0.1) %>% 
      mutate(mortes_ref = mean(media_mortes),
             frota_ref = mean(frota_media),
             pop_ref = mean(media_pop)) %>% 
      select(mortes_ref, frota_ref, pop_ref) %>% 
      slice_head()
  }
}

# Extraindo refrerencias (mortes, pop, frota) ----
criterios <- list(lista_clusters$porte, lista_clusters$cluster_A, lista_clusters$cluster_B,
                  lista_clusters$cluster_C)

refs <- reduce(pmap(criterios, select_mortes_ref), bind_rows)

refs <- as.list(refs)

# Calculo das metas ----
calc_metas <- function(porte, A, B, C, mortes_ref, frota_ref, pop_ref){
  metas <- municipios %>% 
    filter(cluster_A == A,
           cluster_B == B,
           cluster_C == C,
           porte == porte) %>% 
    mutate(meta_pop = mortes_ref * media_pop / pop_ref,
           meta_veic = mortes_ref * frota_media / frota_ref,
           meta = (meta_pop + meta_veic)/2)
}

input_calc <- c(criterios, refs)

metas_calculadas <- reduce(pmap(input_calc, calc_metas), bind_rows)

## Calculando variacoes e removendo municipios sem mortes em 2017, 2018 e 2019----
metas_calculadas <- metas_calculadas %>% 
  distinct(codigo, .keep_all=TRUE) %>% 
  mutate(meta_round = round(meta),
         var = meta - media_mortes,
         var_perc = var / media_mortes)

## Adicionando prioridades ----
metas_calculadas <- metas_calculadas %>% 
  left_join(lista_clusters, by = c("porte", "cluster_A", "cluster_B", "cluster_C")) %>% 
  select(-A, -B, -C)

## Verificando metas resultantes (apenas negativas) ----
dist_meta <- metas_calculadas %>% 
  filter(var_perc < 0) %>% 
  ggplot(aes(x = var_perc)) +
  geom_histogram(color = "white", binwidth = 0.05) +
  scale_x_continuous(minor_breaks = NULL, breaks = seq(-1,0,0.05)) +
  theme_minimal() +
  labs(
    x = "Metas",
    y = "Quantidade de municípios") +
  theme(axis.text.x = element_text(size = 7))

ggsave("output/dist_meta.png", plot = dist_meta, width = 6, height = 4,
       dpi = 300, device = "png")

metas_neg <- metas_calculadas %>% 
  filter(var_perc < 0)

summary(metas_neg)

min <- min(metas_neg$var_perc)
max <- max(metas_neg$var_perc)

var_perc_prioridade <- seq(min, max, ((max-min)/26))
prioridades_lista <- seq(1,27,1)

metas_prioridade <- tibble(
  prioridade = prioridades_lista,
  var_perc = var_perc_prioridade
)

## Aplicando correcao ----
metas_pos_cor <- metas_calculadas %>% 
  filter(var_perc >= 0) %>% 
  select(-var_perc) %>% 
  left_join(metas_prioridade) %>% 
  mutate(var = var_perc * media_mortes,
         meta = media_mortes + var,
         meta_round = round(meta))

## Unindo os municipios ---- 

municipios_metas <- bind_rows(metas_neg, metas_pos_cor)

# Municipios outlier ----

outlier <- read_csv2("input/outliers.csv")

outlier <- outlier %>% 
  select(codigo, sigla_uf, nome, regiao, porte, media_mortes, municipalizacao) %>% 
  left_join(frota_total, by = "codigo") %>% 
  left_join(populacao, by = "codigo")

## Calculando meta estadual para considerar nos municipios outlier ----

uf_metas <- municipios_metas %>% 
  group_by(sigla_uf) %>% 
  summarise(mortes = sum(media_mortes),
            var = sum(var)) %>% 
  mutate(var_perc = var / mortes) %>% 
  select(sigla_uf, var_perc)

## Aplicando aos outliers ----

outlier_metas <- outlier %>% 
  left_join(uf_metas) %>% 
  mutate(var = var_perc * media_mortes,
         meta = media_mortes + var,
         meta_round = round(meta))

## Juntando todos os municipios ----

municipios_metas <- bind_rows(municipios_metas, outlier_metas)

# Resultados ----

## Meta brasileira ----

mortes_br <- sum(municipios_metas$media_mortes)

meta_br <- sum(municipios_metas$meta_round)

var_br <- meta_br - mortes_br

var_perc_br <- var_br / mortes_br

## Distribuicao das metas ----

metas_porte <- municipios_metas %>% 
  ggplot(aes(x = var_perc, fill=porte)) +
  geom_histogram(binwidth = 0.1, color = "#F1F1F1") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(-1, 0, 0.1), minor_breaks = NULL, labels = seq(-100, 0, 10)) +
  scale_y_continuous(minor_breaks = NULL) +
  facet_wrap(~porte, labeller = labeller(porte = c(
    "Maior porte" = "Maior porte (n = 324)",
    "Médio porte" = "Médio porte (n = 1438)",
    "Menor porte" = "Menor porte (n = 3300)"
  ))) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 8)) +
  labs(
    x = "Meta de redução (%)",
    y = "Quantidade de municípios"
  )

ggsave(filename = "output/metas_porte.png", plot = metas_porte, 
       width = 6, height = 3.5,
       device = "png", dpi = 300)

## Reducao por UF ----

metas_uf <- municipios_metas %>% 
  group_by(sigla_uf) %>% 
  summarise(var = sum(var),
            mortes = sum(media_mortes)) %>% 
  mutate(var_perc = var / mortes,
         sigla_uf = fct_reorder(sigla_uf, desc(var_perc))) %>% 
  ggplot(aes(x = sigla_uf, y = -1 * var_perc)) +
  geom_col(fill = "Grey40") +
  geom_text(aes(label = round((var_perc)*100, digits = 0)), color = "white",
            nudge_y = -0.025, size = 2) +
  annotate(geom = "text", y = 0.65, x = 2.1, 
           label = "Meta de redução\n do Brasil (-56%)",
           size = 2, color = "red") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 8)) +
  geom_hline(yintercept = -1 *var_perc_br, 
             linetype = "dashed", color = "red") +
  scale_y_continuous(minor_breaks = NULL, 
                     breaks = seq(0,1,0.1), 
                     limits = c(0,1),
                     labels = seq(0,-100,-10)) +
  labs(
    y = "Meta de redução (%)",
    x = "Estado"
  )

ggsave(filename = "output/metas_uf.png", plot = metas_uf, width = 4.7, 
       height = 3,
       device = "png", dpi = 300)

