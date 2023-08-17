library(tidyverse)

######## INFORMACOES DE SENSIBILIDADE
dados <- readxl::read_xlsx("dados/dados_finais_19_22.xlsx") %>%
  arrange(Ano, periodo) %>%
  left_join(readxl::read_xlsx("dados/dados_antb.xlsx"), by = c("Antibiótico"))


#INFORMACOES DE PREVALENCIA
dados_prevalencia <- readxl::read_xlsx("dados/dados_prevalencia_22.xlsx") %>%
  arrange(Ano, PERIODO) %>%
  mutate(Painel = paste0(
    '<button  title="Contém informações"><a href="javascript:void(0)" onmousedown="',
    "Shiny.onInputChange('DTClick_prev',[", 1:n(), ",Math.random()]);",
    'event.preventDefault(); event.stopPropagation(); return false;"><font color="#4f749b"><b>Análise</b></font></a></button>'
  )) %>%
  mutate(`QUANTIDADE ISOLADA` = as.numeric(`QUANTIDADE ISOLADA`))