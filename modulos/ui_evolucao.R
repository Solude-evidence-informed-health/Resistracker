library(shiny)
library(DT)

source(file = "modulos/footer_s3biotech.R", local = TRUE)

ui_evolucao <- tabPanel(
  title = "Evolução",
  fluidRow(
    div(
      class = "titulo_pagina",
      strong(
        h2(
          "Evolução"
        )
      )
    )
  ),
  br(),
  fluidRow(
    column(
      4,
      uiOutput(
        "filtro_microrganismo_evolucao"
      )
    ),
    column(
      4,
      uiOutput(
        "tipo_cultura_evolucao"
      )
    ),
    column(
      4,
      uiOutput(
        "tipo_antibiotico_evolucao"
      )
    ),
    br(),
    br(),
    br()
  ),
    fluidRow(
      column(
        7,
        apexchartOutput(
          "fig_evolucao"
        )
      ),
      column(
        5,
        apexchartOutput(
          "fig_evolucao_sens_geral"
        )
      )
    ),
  fluidRow(
    tags$br()
    ),
  footer_s3biotech
)
