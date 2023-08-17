library(shiny)
library(apexcharter)
library(DT)
library(tidyverse)
library(htmlwidgets)

source(file = "modulos/footer_s3biotech.R", local = TRUE)

# Nome do microorganismo {CARD}
output$nome_microganismos <- renderText({
  paste(as.character(dados %>%
    filter(
      Microrganismo %in% as.vector(input$filtro_microrganismo),
      !is.na(Sensibilidade)
    ) %>%
    count(Microrganismo) %>%
    select(Microrganismo) %>%
    pull()))
})

# Numero de drogas {CARD}
output$numero_drogas <- renderText({
  paste(as.character(dados %>%
    filter(
      Microrganismo %in% as.vector(input$filtro_microrganismo),
      !is.na(Sensibilidade)
    ) %>%
    count(Antibiótico) %>%
    nrow()))
})



# Periodo de analise {CARD}
output$periodo_analise <- renderText({
  paste(as.character(dados %>%
    filter(periodo %in% as.vector(input$periodo)) %>%
    count(periodo) %>%
    select(periodo) %>%
    pull()))
})



# Nome do microrganismo {FILTRO}
output$filtro_microrganismo <- renderUI({
  pickerInput("filtro_microrganismo", tags$h6(tags$b("Selecione o microrganismo:")),
    choices = sort(unique(dados$Microrganismo)),
    selected = "Acinetobacter baumannii complex"
  )
})



# Nome do microrganismo {FILTRO}
output$tipo_cultura <- renderUI({
  pickerInput("tipo_cultura", tags$h6(tags$b("Selecione o sítio de coleta:")),
    choices = sort(unique(dados$sítio)),
    options = list(`actions-box` = TRUE),
    multiple = T,
    selected = "Geral"
  )
})



# Periodo {FILTRO}
output$periodo <- renderUI({
  pickerInput("periodo", tags$h6(tags$b("Selecione o período:")),
    choices = c(
      "1° sem. de 2019",
      "2° sem. de 2019",
      "1° sem. de 2020",
      "2° sem. de 2020",
      "1° sem. de 2021",
      "2° sem. de 2021",
      "1° sem. de 2022",
      "2° sem. de 2022",
      "Geral"
    ),
    options = list(`actions-box` = TRUE),
    multiple = F,
    selected = "2° sem. de 2022"
  )
})



# Cria dataset que vai ser usado para inserir o botao {TABELA, BOTAO}
dados_gerais <- dados %>%
  mutate(
    Painel = paste0(
      '<button  title="Contém informações da linha detalhadas"><a href="javascript:void(0)" onmousedown="',
      "Shiny.onInputChange('DTClick',[", 1:n(), ",Math.random()]);",
      'event.preventDefault(); event.stopPropagation(); return false;"><font color="#4f749b"><b>Análise</b></font></a></button>'
    ),
    Droga = paste0(
      '<button  title="Contém informações da linha detalhadas"><a href="javascript:void(0)" onmousedown="',
      "Shiny.onInputChange('DTClick_droga',[", 1:n(), ",Math.random()]);",
      'event.preventDefault(); event.stopPropagation(); return false;"><font color="#4f749b"><b>Utilização</b></font></a></button>'
    )
    # ,
    # Antibiotics = case_when(Antibiótico == "Amicacina" ~ "Amikacin",
    #                         Antibiótico == "Ampicilina" ~ "Ampicillin",
    #                         Antibiótico == "Benzilpenicilina" ~ "Benzylpenicillin",
    #                         Antibiótico == "Caspofungina" ~ "Caspofungin",
    #                         Antibiótico == "Cefalotina" ~ "Cephalothin",
    #                         Antibiótico == "Cefepima" ~ "Cefepime",
    #                         Antibiótico == "Cefotaxima" ~ "Cefotaxime",
    #                         Antibiótico == "Ceftarolina" ~ "Ceftaroline",
    #                         Antibiótico == "Ceftazidima" ~ "Ceftazidime",
    #                         Antibiótico == "Ceftriaxona" ~ "Ceftriaxone",
    #                         Antibiótico == "Cefuroxima" ~ "Cefuroxime",
    #                         TRUE ~ "NA")
  )



# Sensibilidade dos microrganismos por sitio, periodo e droga {TABELA}
output$tableUI <- renderDataTable({
  dat <- dados_gerais %>%
    filter(Microrganismo %in% as.vector(input$filtro_microrganismo)) %>%
    filter(sítio %in% as.vector(input$tipo_cultura)) %>%
    filter(periodo %in% as.vector(input$periodo)) %>%
    select(
      -c(
        Microrganismo,
        periodo,
        n_sensivel,
        Ano,
        Antibiotics
      )
    ) %>%
    rename(
      '<a title="Contém número de microrganismos."><font color="#ffffff"><b>Amostras</b></font></a>' = "n_amostra",
      '<a title="Contém o nome do sítio de coleta do microrganismo. Geral é o agrupamento de todos os sítios"><font color="#ffffff"><b>Sítios</b></font></a>' = "sítio",
      '<a title="Contém informações sobre a sensibilidade do microrganismo em %."><font color="#ffffff"><b>Sensibilidade (%)</b></font></a>' = "Sensibilidade",
      '<a title="Contém informações o nome do antibiótico utilizado."><font color="#ffffff"><b>Antibióticos</b></font></a>' = "Antibiótico",
      '<a title="Contém um painel detalhado sobre as informações da linha."><font color="#ffffff"><b>Painéis</b></font></a>' = "Painel",
      '<a title="Contém um painel detalhado com informações sobre o uso dessa droga."><font color="#ffffff"><b>Informações sobre o uso</b></font></a>' = "Droga"
    )

  
  
  # Funcao que vai gerar a informacao com a tabela visual {TABELA}
  datatable(dat,
    rownames = FALSE,
    selection = "none",
    escape = FALSE,
    options = list(
      pageLength = 10,
      dom = "tp",
      order = list(
        list(
          3,
          "desc"
        )
      ),
      columnDefs = list(
        list(className = 'dt-center', 
             targets = "_all"),
        list(
          width = "130px",
          className = "dt-center",
          targets = 3
        )
      ),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#4f749b', 'color': '#ffffff'});",
        "}"
      )
    )
  ) %>%
    formatStyle(
      '<a title="Contém informações sobre a sensibilidade do microrganismo em %."><font color="#ffffff"><b>Sensibilidade (%)</b></font></a>',
      backgroundColor = styleInterval(
        c(0, 10, 15, 25, 35, 45, 60, 75, 80, 85, 95, 100),
        temppal
      )
    )
})



output$modalContent <- renderApexchart({
  dat <- dados_gerais[as.numeric(input$DTClick[1]), ]
  Ant <- dat$Antibiótico
  Mic <- dat$Microrganismo
  Sit <- dat$sítio

  

  data2 <- dados_gerais %>%
    filter(periodo != "Geral") %>%
    filter(
      Microrganismo == Mic,
      sítio == Sit,
      Antibiótico == Ant
    )

  apex(data2,
    aes(periodo,
      Sensibilidade,
      group = sítio,
      fill = sítio
    ),
    type = "line"
  ) %>%
    ax_colors("#4f749b")
})



observeEvent(
  input$DTClick,
  {
    showModal(
      modalDialog(
        title = paste0(
          dados_gerais[as.numeric(input$DTClick[1]), ]$Microrganismo,
          " - ", dados_gerais[as.numeric(input$DTClick[1]), ]$sítio, " - ",
          dados_gerais[as.numeric(input$DTClick[1]), ]$Antibiótico
        ),
        apexchartOutput(
          "modalContent"
        ),
        size = "l",
        easyClose = TRUE,
        footer = footer_s3biotech
      )
    )
  }
)
