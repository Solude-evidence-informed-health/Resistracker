library(googlesheets4)
library(tidyverse)

#LINK DAS PLANILHAS
sem2_2022 <- "https://docs.google.com/spreadsheets/d/1Hwcca39NxrmGDbdSEJdtOB6XlXlAkIj5ge0rV0kdv2A/edit?usp=sharing"
sem1_2022 <- "https://docs.google.com/spreadsheets/d/1kb7Wly-b5qc2qMmTb1K1YCJ8OutCNgqDA8RKIR9StRs/edit?usp=share_link"
sem2_2021 <- "https://docs.google.com/spreadsheets/d/1HvMoJM6GfFum0P90GbUsTYS2jyqTAIHxfYX3vh63DLw/edit?usp=sharing"
sem1_2021 <- "https://docs.google.com/spreadsheets/d/19xLYeE-t4CbmQETDvR81YJ_ezcUWA24Frjk7QztN0ck/edit?usp=sharing"
sem2_2020 <- "https://docs.google.com/spreadsheets/d/1CFDe8mQNK8v7yGDFTiLTYUpywd87z1SZtNIsZr9pI8c/edit?usp=sharing"
sem1_2020 <- "https://docs.google.com/spreadsheets/d/1vq3x1OxaIFHzLiXV-V0NqbLizFxQ7i8d3jIBtS7ASak/edit?usp=sharing"
sem2_2019 <- "https://docs.google.com/spreadsheets/d/1jhaiZuOP9X2tJ5kHeS1WaaHbM3q2PNQxGhhCU5-mCNg/edit?usp=sharing"
sem1_2019 <- "https://docs.google.com/spreadsheets/d/1urQ52zob9U7qUxRfFC4pRcZBGKQTjXqjL1lKCIi5O2k/edit?usp=sharing"

#CRIAR LISTA DE LINK DAS PLANILHAS
links <- c(sem1_2019, sem2_2019, sem1_2020, sem2_2020, sem1_2021, sem2_2021, sem1_2022, sem2_2022)



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
      pivot_longer(cols = -c(Antibiótico, periodo), 
                   names_to = "Microrganismo", 
                   values_to = "Sensibilidade") %>%
      mutate(
        periodo = as.character(periodo),
        Sensibilidade = as.numeric(100 * Sensibilidade), # transforma a sensibilidade em numerico
        n_amostra = ifelse(is.na(Sensibilidade), NA, as.numeric(str_extract(Microrganismo, "(?<=\\().*?(?=\\))"))), # extrai valores entre parenteses
        Microrganismo = str_remove_all(Microrganismo, "\\(.*?\\)"), # remove informacoes entre os parenteses e os parenteses
        local = as.character(sheet_name),
        Antibiótico = as.character(Antibiótico),
        Ano = str_sub(periodo, start=nchar(periodo) - 4, end=nchar(periodo))
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


dados_finais <- bind_rows(resultado_final_1sem_2019, resultado_final_2sem_2019,
                     resultado_final_1sem_2020, resultado_final_2sem_2020,
                     resultado_final_1sem_2021, resultado_final_2sem_2021,
                     resultado_final_1sem_2022, resultado_final_2sem_2022) %>%
  filter(!is.na(Sensibilidade)) %>%
  mutate(sítio = case_when(grepl("PERFIL ANTIB FRAG TECIDO", local) == TRUE ~ "Tecido epitelial e partes moles",
                           grepl("PERFIL FRAG TECIDO", local) == TRUE ~ "Tecido epitelial e partes moles",
                           grepl("DIVERSOS", local) == TRUE ~ "Tecido epitelial e partes moles",
                           grepl("FRAGMENTO OSSEO", local) == TRUE ~ "Tecido epitelial e partes moles",
                           grepl("FRAG OSSEO", local) == TRUE ~ "Tecido epitelial e partes moles",
                           grepl("LIQUIDO ASCÍTICO", local) == TRUE ~ "Cavidade abdominal",
                           grepl("LÍQUIDO ASCÍTICO", local) == TRUE ~ "Cavidade abdominal",
                           grepl("LIQUIDO PERITONEAL", local) == TRUE ~ "Cavidade abdominal",
                           grepl("LIQUIDO PERITONIAL", local) == TRUE ~ "Cavidade abdominal",
                           grepl("LÍQUIDO PERITONIAL", local) == TRUE ~ "Cavidade abdominal",
                           grepl("LIQUIDO SINOVIAL", local) == TRUE ~ "Tecido epitelial e partes moles",
                           grepl("LIQUIDO PLEURAL", local) == TRUE ~ "Sistema respiratório",
                           grepl("LIQUOR", local) == TRUE ~ "Tecido epitelial e partes moles",
                           grepl("PERFIL ANTIB COPROCULTURA", local) == TRUE ~ "Coprocultura",
                           grepl("PERFIL ANTIB ESCARRO", local) == TRUE ~ "Sistema respiratório",
                           grepl("PERFIL ANTIB GERAL", local) == TRUE ~ "Geral",
                           grepl("PERFIL ANTIB HEMOCULTURA", local) == TRUE ~ "Hemocultura",
                           grepl("PERFIL ANTIB LAVADO BRONQ", local) == TRUE ~ "Sistema respiratório",
                           grepl("PERFIL ANTIB PONTA DE CATETER", local) == TRUE ~ "Hemocultura",
                           grepl("PERFIL ANTIB SEC OROFARINGE", local) == TRUE ~ "Sistema respiratório",
                           grepl("PERFIL ANTIB SEC TRAQUEAL", local) == TRUE ~ "Sistema respiratório",
                           grepl("PERFIL ANTIB SEC URETRAL", local) == TRUE ~ "Sistema urinário",
                           grepl("PERFIL ANTIB SWAB RETAL", local) == TRUE ~ "Swab retal",
                           grepl("PERFIL ANTIB UROCULTURA", local) == TRUE ~ "Sistema urinário",
                           grepl("SEC DE FERIDA", local) == TRUE ~ "Tecido epitelial e partes moles",
                           grepl("SECREÇÃO VAGINAL", local) == TRUE ~ "Sistema urinário",
                           grepl("OUTROS MATERIAIS", local) == TRUE ~ "Tecido epitelial e partes moles",
                           grepl("OUTROS MATERIAS", local) == TRUE ~ "Tecido epitelial e partes moles"
                           )) %>%
  mutate(n_sensivel = n_amostra*(Sensibilidade/100)) %>%
  group_by(Antibiótico, periodo, Microrganismo, sítio, Ano) %>%
  summarise(n_amostra = sum(n_amostra),
            n_sensivel = sum(n_sensivel)) %>%
  mutate(Sensibilidade = 100*(n_sensivel/n_amostra)) %>%
  mutate(Sensibilidade = round(Sensibilidade, 0)) %>%
  filter(Antibiótico != "BLSE") %>%
  filter(!is.na(Sensibilidade))


dados_gerais <- dados_finais %>%
  group_by(Antibiótico, Microrganismo, sítio) %>%
  summarise(n_amostra = sum(n_amostra),
            n_sensivel = sum(n_sensivel)) %>%
  mutate(Sensibilidade = 100*(n_sensivel/n_amostra)) %>%
  mutate(periodo = "Geral") %>%
  mutate(Sensibilidade = round(Sensibilidade, 0)) %>%
  mutate(Ano = "0")

dados_completos <- rbind(dados_finais, dados_gerais)

writexl::write_xlsx(dados_completos, "dados_finais_19_22.xlsx")
