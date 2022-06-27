plot_metas_porte <- function(municipios) {
  n_maior <- nrow(municipios %>% filter(porte == "Maior porte"))
  n_medio <- nrow(municipios %>% filter(porte == "Médio porte"))
  n_menor <- nrow(municipios %>% filter(porte == "Menor porte"))
  
  ggplot(municipios, aes(x = var_perc, fill = porte)) + 
    geom_histogram(binwidth = 0.1, color = "#F1F1F1") +
    scale_fill_brewer(palette = "Set2") +
    scale_x_continuous(
      breaks = seq(-1, 0, 0.1),
      minor_breaks = NULL,
      labels = seq(-100, 0, 10)
    ) +
    scale_y_continuous(minor_breaks = NULL) + 
    facet_wrap(~porte, labeller = labeller(porte = c(
      "Maior porte" = glue("Maior porte (n = {n_maior})"),
      "Médio porte" = glue("Maior porte (n = {n_medio})"),
      "Menor porte" = glue("Menor porte (n = {n_menor})")
    ))) + 
    theme_bw(base_size = 8) +
    theme(legend.position = "none") +
    labs(
      x = "Meta de redução (%)",
      y = "Quantidade de municípios"
    )
}

plot_metas_uf <- function(municipios) {
  label_br <- round(var_perc_br * 100 * -1)
  
  municipios %>% 
    group_by(uf) %>% 
    summarise(var = sum(var), mortes = sum(media_mortes)) %>% 
    mutate(
      var_perc = var / mortes,
      uf = fct_reorder(uf, desc(var_perc))
    ) %>% 
    ggplot(aes(x = uf, y = -1 * var_perc)) +
    geom_col(fill = "grey50") +
    geom_text(
      aes(label = round(var_perc * 100)),
      color = "white",
      nudge_y = -0.025,
      size = 2
    ) +
    geom_hline(
      yintercept = -1 * var_perc_br,
      linetype = "dashed",
      color = "red"
    ) +
    annotate(
      geom = "text",
      y = 0.48,
      x = 4.5,
      label = glue("Meta de redução\n do Brasil: {label_br}%"),
      size = 2,
      color = "red"
    ) +
    coord_flip() +
    theme_minimal(base_size = 8) +
    scale_y_continuous(
      minor_breaks = NULL,
      breaks = seq(0, 1, 0.1),
      limits = c(0,1),
      labels = seq(0, -100, -10)
    ) +
    labs(
      y = "Meta de redução (%)",
      x = "Estado"
    ) +
    theme(plot.background = element_rect(color = NULL, fill = "white"))
}
