split_clusters <- function(municipios, mun_porte, grupo_prioridade) {
  municipios <- municipios %>% 
    filter(porte == mun_porte, prioridade == grupo_prioridade)
}

check_empty <- function(municipios) {
  if (nrow(municipios) == 0) {
    return(1)
  } else {
    return(0)
  }
}

calc_referencia <- function(municipios, mun_porte, grupo_prioridade) {
  if (mun_porte == "Maior porte") {
    municipios <- municipios %>% arrange(pc1)
  } else {
    municipios <- municipios %>% arrange(-pc1)
  }
  
  if (nrow(municipios) < 10) {
    ref <- 
      municipios %>%
      slice_head() %>%
      select(
        mortes_ref = media_mortes,
        frota_ref = media_frota,
        pop_ref = media_pop
      )
  } else {
    ref <- 
      municipios %>%
      slice_head(prop = 0.1) %>% 
      mutate(
        mortes_ref = mean(media_mortes),
        frota_ref = mean(media_frota),
        pop_ref = mean(media_pop)
      ) %>% 
      select(mortes_ref, frota_ref, pop_ref) %>% 
      slice_head()
  }
  return(ref)
}

calc_metas <- function(
    municipios, mun_porte, grupo_prioridade, mortes_ref, frota_ref, pop_ref
  ) {
  municipios %>% 
    filter(porte == mun_porte, prioridade == grupo_prioridade) %>% 
    mutate(
      meta_pop = mortes_ref * media_pop / pop_ref,
      meta_veic = mortes_ref * media_frota / frota_ref,
      meta = (meta_pop + meta_veic) / 2
    )
}

calc_variacao <- function(municipios) {
  municipios %>% 
    mutate(
      meta_round = round(meta),
      var = meta - media_mortes,
      var_perc = var / media_mortes
    )
}

fix_metas_positivas <- function(lista) {
  metas_neg <- lista %>% filter(var_perc < 0)
  min <- min(metas_neg$var_perc)
  max <- max(metas_neg$var_perc)
  var_perc_prioridade <- seq(min, max, ((max - min) / 26))
  prioridades_lista <- seq(1, 27, 1)
  
  metas_prioridade <- tibble(
    prioridade = prioridades_lista,
    var_perc = var_perc_prioridade
  )
  
  return(metas_prioridade)
}

apply_metas_fix <- function(lista, metas_fix) {
  lista %>% 
    filter(var_perc >= 0) %>% 
    select(-var_perc) %>% 
    left_join(metas_fix) %>% 
    mutate(
      var = var_perc * media_mortes,
      meta = media_mortes + var,
      meta_round = round(meta)
    )
}
