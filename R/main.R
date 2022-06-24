library(tidyverse)

options(scipen = 999)

source("R/arrange.R")

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
