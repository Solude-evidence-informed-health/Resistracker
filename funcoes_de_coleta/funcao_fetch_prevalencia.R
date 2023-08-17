library(googlesheets4)
library(tidyverse)

#LINK DAS PLANILHAS
sem2_2022 <- "https://docs.google.com/spreadsheets/d/1Pz-OHw3Up0kJvKGC9Hlt75dhaSRGXpJ7ZJvZmcuKplg/edit?usp=sharing"
sem1_2022 <- "https://docs.google.com/spreadsheets/d/1xbnSNzYWp0Z42PVl8TXiPFFck6J1dhlsnOc8kDD9rHk/edit?usp=sharing"
sem2_2021 <- "https://docs.google.com/spreadsheets/d/1xKZW8fNu_c-RQghNkGjfjEGqQPTTvF2-yb7dWDqC_Yk/edit?usp=sharing"
sem1_2021 <- "https://docs.google.com/spreadsheets/d/1HMD_NSf01cbaMbN-XS2Exbc2Sic8lGBm04RJTascWKc/edit?usp=sharing"
sem2_2020 <- "https://docs.google.com/spreadsheets/d/1Q3152X-la0xBRQSnTSWVPwstSuP-7qgsRFUn-Wy0TxU/edit?usp=sharing"
sem1_2020 <- "https://docs.google.com/spreadsheets/d/1lH-QnanyfPuGucUaw_Jfxgi6OVHYrZZu0P9SE4an7gI/edit?usp=sharing"
sem2_2019 <- "https://docs.google.com/spreadsheets/d/1OpN5m2sIaw2mBz1S6T_LH4EGYkvPBEVMRygZH3Hq5yQ/edit?usp=sharing"
sem1_2019 <- "https://docs.google.com/spreadsheets/d/1TOPheldj2yV59wXVnVibkSIrwG4-BVl36Ou0Q4uD-w0/edit?usp=sharing"

#CRIAR LISTA DE LINK DAS PLANILHAS
links <- c(
  sem1_2019, 
  sem2_2019, 
  sem1_2020, 
  sem2_2020, 
  sem1_2021, 
  sem2_2021, 
  sem1_2022, 
  sem2_2022)



# Funcao que retorna a planilha conforme o nome da janela (local de cultura)
process_data <- function(link_sheet) {
  gs4_deauth()
  df_list <- list()
  sheet_names <- sheet_names(link_sheet)
  for (i in seq_along(sheet_names)) {
    sheet_name <- sheet_names[i]
    df_list[[sheet_name]] <- read_sheet(link_sheet,
                                        sheet = sheet_name
    ) %>%
    # pivot_longer(cols = -c(Antibiótico, periodo),
    #              names_to = "Microrganismo",
    #              values_to = "Sensibilidade") %>%
    mutate(
      # periodo = as.character(periodo),
      # Sensibilidade = as.numeric(100 * Sensibilidade), # transforma a sensibilidade em numerico
      # n_amostra = ifelse(is.na(Sensibilidade), NA, as.numeric(str_extract(Microrganismo, "(?<=\\().*?(?=\\))"))), # extrai valores entre parenteses
      # Microrganismo = str_remove_all(Microrganismo, "\\(.*?\\)"), # remove informacoes entre os parenteses e os parenteses
      local = as.character(sheet_name),
      # Antibiótico = as.character(Antibiótico),
      Ano = str_sub(PERIODO, start=nchar(PERIODO) - 3, end=nchar(PERIODO))
    )
  }
  return(df_list)
}




# lista para armazenar os resultados
resultado <- list()

# loop para aplicar a função em cada planilha
for (i in seq_along(links)) {
  resultado[[i]] <- process_data(links[i])
}

# combinar todos os resultados em um único dataframe
resultado_final_1sem_2019 <- bind_rows(resultado[[1]])
 
resultado_final_2sem_2019 <- bind_rows(resultado[[2]])
 
resultado_final_1sem_2020 <- bind_rows(resultado[[3]])

resultado_final_2sem_2020 <- bind_rows(resultado[[4]])

resultado_final_1sem_2021 <- bind_rows(resultado[[5]])

resultado_final_2sem_2021 <- bind_rows(resultado[[6]])

resultado_final_1sem_2022 <- bind_rows(resultado[[7]])

resultado_final_2sem_2022 <- bind_rows(resultado[[8]])


dados_finais <- bind_rows(
  resultado_final_1sem_2019, 
  resultado_final_2sem_2019,
  resultado_final_1sem_2020, 
  resultado_final_2sem_2020,
  resultado_final_1sem_2021, 
  resultado_final_2sem_2021,
  resultado_final_1sem_2022, 
  resultado_final_2sem_2022
  ) %>%
  # filter(!is.na(Sensibilidade)) %>%
  mutate(sítio = case_when(
                           grepl("INCID CULT FRAG TECIDO", local) == TRUE ~ "Tecido epitelial e partes moles", #OK
                           # grepl("PERFIL FRAG TECIDO", local) == TRUE ~ "Tecido epitelial e partes moles",
                           grepl("INCID CULT FRAG DIVERSOS", local) == TRUE ~ "Tecido epitelial e partes moles", #OK
                           grepl("INCID CULT FRAG OSSEO", local) == TRUE ~ "Tecido epitelial e partes moles", #OK
                           # grepl("FRAG OSSEO", local) == TRUE ~ "Tecido epitelial e partes moles",
                           grepl("INCID LIQ ASCITICO", local) == TRUE ~ "Cavidade abdominal", #OK
                           grepl("INCID LIQ ASCITCO", local) == TRUE ~ "Cavidade abdominal",
                           grepl("INCID LIQ PERITONEAL", local) == TRUE ~ "Cavidade abdominal",
                           grepl("INCID CULT LIQ PERITONIAL", local) == TRUE ~ "Cavidade abdominal", #OK
                           grepl("INCID LIQ PERITONIAL", local) == TRUE ~ "Cavidade abdominal", #OK
                           grepl("INCID LIQ SINOVIAL", local) == TRUE ~ "Tecido epitelial e partes moles", #OK
                           grepl("INCID LIQ PLEURAL", local) == TRUE ~ "Sistema respiratório", #OK
                           grepl("INCID LIQUOR", local) == TRUE ~ "Tecido epitelial e partes moles", #OK
                           grepl("INCID COPROCULTURA", local) == TRUE ~ "Coprocultura", #OK
                           grepl("INCID CULT ESCARRO", local) == TRUE ~ "Sistema respiratório", #OK
                           grepl("INCID CULT GERAL", local) == TRUE ~ "Geral", #OK
                           grepl("INCID LIQ  PERITONIAL", local) == TRUE ~ "Cavidade abdominal", #Ok
                           grepl("INC HEMOCULTURA", local) == TRUE ~ "Hemocultura", #OK
                           grepl("INCID LAV BRONQUICO", local) == TRUE ~ "Sistema respiratório", #OK
                           grepl("INC PONTA DE CATETER", local) == TRUE ~ "Hemocultura", #OK
                           grepl("SEC DE OROFARINGE", local) == TRUE ~ "Sistema respiratório", #OK
                           grepl("INCID SEC TRAQUEAL", local) == TRUE ~ "Sistema respiratório", #OK
                           grepl("SEC URETRAL", local) == TRUE ~ "Sistema urinário", #OK
                           grepl("SWAB RETAL", local) == TRUE ~ "Swab retal", #OK
                           grepl("INC UROCULTURA", local) == TRUE ~ "Sistema urinário", #OK
                           grepl("INCID CULT SEC DE FERIDA", local) == TRUE ~ "Tecido epitelial e partes moles", #OK
                           grepl("INCID CULT SEC FERIDA", local) == TRUE ~ "Tecido epitelial e partes moles", #OK
                           grepl("SEC VAGINAL", local) == TRUE ~ "Sistema urinário", #ok
                           grepl("INCID CULT FRAG OUTROS MATERIAIS", local) == TRUE ~ "Tecido epitelial e partes moles", #OK
                           grepl("INCID CULT OUTROS MATERIAIS", local) == TRUE ~ "Tecido epitelial e partes moles",
                           grepl("INCID CULT DIVERSOS", local) == TRUE ~ "Tecido epitelial e partes moles",
                           grepl("INCID CULT OUTROS MATERIAS", local) == TRUE ~ "Tecido epitelial e partes moles"
   )) %>%
  select(-c(local, INCIDÊNCIA)) %>%
  mutate(`QUANTIDADE ISOLADA` = as.numeric(`QUANTIDADE ISOLADA`)) %>%
  group_by(MICRORGANISMO, PERIODO, sítio, Ano) %>%
  summarise(`QUANTIDADE ISOLADA` = sum(`QUANTIDADE ISOLADA`))

dados_gerais <- dados_finais %>%
  mutate(`QUANTIDADE ISOLADA` = as.numeric(`QUANTIDADE ISOLADA`)) %>%
  group_by(MICRORGANISMO, sítio) %>%
  summarise(`QUANTIDADE ISOLADA` = sum(`QUANTIDADE ISOLADA`)) %>%
  mutate(PERIODO = "Geral") %>%
  mutate(Ano = "0")

dados_completos <- rbind(dados_finais, dados_gerais)

writexl::write_xlsx(dados_completos, "dados_prevalencia_22.xlsx")
