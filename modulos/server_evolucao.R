library(shiny)
library(htmlwidgets)
library(DT)
library(apexcharter)
library(tidyverse)



# Nome do microrganismo {FILTRO}
output$filtro_microrganismo_evolucao <- renderUI({
  pickerInput(
    inputId = "filtro_microrganismo_evolucao",
    tags$h6(
      tags$b(
        "Selecione o microrganismo:"
      )
    ),
    choices = sort(
      unique(
        dados$Microrganismo
      )
    ),
    selected = "Acinetobacter baumannii complex",
    multiple = F
  )
})



# Nome do sitio {FILTRO}
output$tipo_cultura_evolucao <- renderUI({
  pickerInput(
    inputId = "tipo_cultura_evolucao",
    tags$h6(
      tags$b(
        "Selecione o sítio de coleta:"
      )
    ),
    choices = sort(
      unique(
        dados$sítio
      )
    ),
    options = list(
      `actions-box` = TRUE
    ),
    multiple = T,
    selected = "Geral"
  )
})



# Nome do antibiotico {FILTRO}
output$tipo_antibiotico_evolucao <- renderUI({
  pickerInput(
    inputId = "tipo_antibiotico_evolucao",
    tags$h6(
      tags$b(
        "Selecione a droga:"
      )
    ),
    choices = sort(
      unique(
        dados$Antibiótico
      )
    ),
    options = list(
      `actions-box` = TRUE
    ),
    multiple = F,
    selected = "Amicacina"
  )
})



# Evolucao da sensibilidade ao longo do tempo {GRAFICO}
output$fig_evolucao <- renderApexchart({
  apex(
    dados %>%
      filter(periodo != "Geral") %>%
      filter(Microrganismo %in% as.vector(input$filtro_microrganismo_evolucao)) %>%
      filter(sítio %in% as.vector(input$tipo_cultura_evolucao)) %>%
      filter(Antibiótico %in% as.vector(input$tipo_antibiotico_evolucao)),
    aes(periodo,
      Sensibilidade,
      group = sítio,
      fill = sítio
    ),
    type = "line"
  ) %>%
    ax_colors(
      "#6495ed",
      "#5d8aa8",
      "#5a4fcf",
      "#483d8b",
      "#019196",
      "#019d6e",
      "#50647f",
      "#b0c6df"
    ) %>%
    ax_yaxis(
      title = list(
        text = "Sensibilidade (%)",
        style = list(
          color = "#4f749b"
        )
      )
    ) %>%
    ax_xaxis(
      title = list(
        text = "Períodos (semestres)",
        style = list(
          color = "#4f749b"
        )
      )
    )
})



# Sensibilidade geral {GRAFICO}
output$fig_evolucao_sens_geral <- renderApexchart({
  apex(
    dados %>%
      filter(periodo == "Geral") %>%
      filter(Microrganismo %in% as.vector(input$filtro_microrganismo_evolucao)) %>%
      filter(sítio == "Geral") %>%
      filter(Antibiótico %in% as.vector(input$tipo_antibiotico_evolucao)),
    type = "radialBar",
    mapping = aes(
      x = "Sensibilidade geral",
      y = Sensibilidade
    )
  ) %>%
    ax_colors("#4f749b")
})

