library(shiny)
library(htmlwidgets)
library(DT)
library(apexcharter)
library(tidyverse)

# infos - painel
source(file = "modulos/info_filtro_sitio.r", local = TRUE)
# footer - painel
source(file = "modulos/footer_s3biotech.R", local = TRUE)

# Nome do sitio {FILTRO}
output$filtro_sitio_prevalencia <- renderUI({
  pickerInput(
    inputId = "filtro_sitio_prevalencia",
    tags$a(
      class = "cor_prevalencia_filtro",
      title = info_filtro_sitio,
      tags$b("Selecione o sítio:")
    ),
    choices = sort(
      unique(
        dados_prevalencia$sítio
      )
    ),
    selected = "Geral"
  )
})


# Nome do microorganismo {CARD}
output$sitio_selecionado <- renderText({
  paste(as.character(dados_prevalencia %>%
                       filter(
                         sítio %in% as.vector(input$filtro_sitio_prevalencia),
                         PERIODO != "Geral"
                       ) %>%
                       count(sítio) %>%
                       select(sítio) %>%
                       pull()))
})

# Numero de microrganismos {CARD}
output$numero_microrganismos <- renderText({
  paste(as.character(dados_prevalencia %>%
                       filter(
                         sítio %in% as.vector(input$filtro_sitio_prevalencia),
                         PERIODO != "Geral"
                       ) %>%
                       summarise(total = sum(`QUANTIDADE ISOLADA`)) %>%
                       select(total) %>%
                       pull()))
})





# Prevalencia dos microrganismos por sitio {TABELA}
output$tabela_sitio_prevalencia <- renderDataTable({
  # Cria dataset que vai ser usado na criacao da tabela
  dat_prevalencia <- dados_prevalencia %>%
    filter(PERIODO == "Geral") %>%
    filter(sítio %in% as.vector(input$filtro_sitio_prevalencia)) %>%
    # rename(
    #   "Microrganismo" = "MICRORGANISMO",
    #   "Período" = "PERIODO",
    #   "Quantidade isolada" = "QUANTIDADE ISOLADA",
    #   "Painéis" = "Painel"
    # ) %>%
    rename(
      '<a title="Contém o nome do microrganismos."><font color="#ffffff"><b>Microrganismo</b></font></a>' = "MICRORGANISMO",
      '<a title="Contém o período em que o microrganismo foi identificado."><font color="#ffffff"><b>Período</b></font></a>' = "PERIODO",
      '<a title="Contém o número da quantidade de microrganimos isolados."><font color="#ffffff"><b>Quantidade isolada</b></font></a>' = "QUANTIDADE ISOLADA",
      '<a title="Contém um painel detalhado sobre as informações da linha."><font color="#ffffff"><b>Painéis</b></font></a>' = "Painel"
    ) %>%
    select(
      -c(
        sítio,
        Ano
      )
    )


  # Funcao que vai gerar a informacao com a tabela visual {TABELA}
  datatable(dat_prevalencia,
    rownames = FALSE,
    selection = "none",
    escape = FALSE,
    options = list(
      pageLength = 10,
      dom = "tp",
      order = list(
        list(
          2,
          "desc"
        )
      ),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#4f749b', 'color': '#ffffff'});",
        "}"
      )
    )
  )
})



output$modal_figure <- renderApexchart({
  dat_prevalencia <- dados_prevalencia[as.numeric(input$DTClick_prev[1]), ]
  Mic <- dat_prevalencia$MICRORGANISMO
  Sit <- dat_prevalencia$sítio


  data2 <- dados_prevalencia %>%
    filter(PERIODO != "Geral") %>%
    filter(
      MICRORGANISMO == Mic,
      sítio == Sit
    )

  apex(data2,
    aes(PERIODO,
      `QUANTIDADE ISOLADA`,
      group = sítio,
      fill = sítio
    ),
    type = "line"
  )
})

observeEvent(
  input$DTClick_prev,
  {
    showModal(
      modalDialog(
        title = paste0(
          "Sítio de coleta: ",
          dados_prevalencia[as.numeric(input$DTClick_prev[1]), ]$sítio,
          " - ",
          "Microrganismo: ",
          dados_prevalencia[as.numeric(input$DTClick_prev[1]), ]$MICRORGANISMO
        ),
        apexchartOutput(
          "modal_figure"
        ),
        size = "l",
        easyClose = TRUE,
        footer = footer_s3biotech
      )
    )
  }
)