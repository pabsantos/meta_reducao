## Estabelecimento de Metas de Redução em Municípíos Brasileiros

### Descrição
O presente projeto tem como objetivo estabelecer a criação de metas de redução de mortes no trânsito em 5062 municípios brasileiros. Através dos clusters estebelecidos no [diagnóstico da segurança viária de municípios brasileiros](https://github.com/pabsantos/diag_municipios) foi possível aplicar _benchmarks_ para calcular essas metas. Esse estudo foi realizado pelo [Observatório Nacional de Segurança Viária](https://onsv.org.br)

### Conteúdo

O script `meta_redução.R` contém todo o processo de cálculo das metas de redução. `input/` inclue os dados de entrada e `output/` contém os principais resultados. `dash_metas.Rmd` contém a criação de um dashboard com os resultados, criado a partir do pacote `flexdashboards`. O html resultante pode ser acessado [neste link](https://rpubs.com/pabsantos/meta_reducao)
