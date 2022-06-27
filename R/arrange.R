arrange_populacao <- function(pop_antigo, pop_2020) {
  lista_populacao <- 
    pop_antigo %>% 
    janitor::clean_names() %>% 
    separate(
      municipio,
      into = c("ibge_cod", "nome_mun"),
      sep = " ",
      extra = "merge"
    ) %>% 
    select(ibge_cod, ano_2018 = x2018, ano_2019 = x2019)
  
  pop2020 <- 
    pop_2020 %>% 
    janitor::clean_names() %>% 
    separate(
      municipio,
      into = c("ibge_cod", "nome_mun"),
      sep = " ",
      extra = "merge"
    ) %>% 
    select(ibge_cod, ano_2020 = populacao_estimada)
  
  lista_populacao %>% 
    left_join(pop2020, by = "ibge_cod") %>% 
    mutate(media_pop = (ano_2018 + ano_2019 + ano_2020) / 3) %>% 
    select(ibge_cod, media_pop)
}

extract_frota <- function(frota, ano) {
  frota %>% 
    select(ibge_cod = codigo, total = TOTAL) %>% 
    mutate(frota_ano = ano)
}

calc_media_frota <- function(frota) {
  frota %>% 
    pivot_wider(
      names_from = frota_ano, 
      values_from = total,
      names_prefix = "ano_"
    ) %>% 
    mutate(media_frota = (ano_2018 + ano_2019 + ano_2020) / 3) %>% 
    select(ibge_cod, media_frota)
}

calc_metas_uf <- function(municipios) {
  municipios %>% 
    group_by(uf) %>% 
    summarise(mortes = sum(media_mortes), var = sum(var)) %>% 
    mutate(var_perc = var / mortes) %>% 
    ungroup() %>% 
    select(uf, var_perc)
}

add_metas_outlier <- function(metas_uf, outliers) {
  outliers %>% 
    left_join(metas_uf, by = "uf") %>% 
    mutate(
      var = var_perc * media_mortes,
      meta = media_mortes + var,
      meta_round = round(meta)
    )
}
