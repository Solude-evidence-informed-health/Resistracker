library(shiny)
library(DT)

source(file = "modulos/footer_s3biotech.R", local = TRUE)

ui_sensibilidade <- tabPanel(
  "Perfil de sensibilidade",
  fluidRow(
    div(
      class = "titulo_pagina",
      strong(
        h2(
          "Perfil de sensibilidade"
        )
      )
    )
  ),
  br(),
  fluidRow(
    column(
      4,
      div(
        class = "card",
        div(
          class = "info_dados",
          h5(
            htmltools::span(
              "Nome do microrganismo:"
            )
          ),
          hr(),
          h4(
            class = "saida_texto",
            textOutput(
              "nome_microganismos"
            )
          )
        )
      )
    ),
    column(
      2,
      div(
        class = "card",
        div(
          class = "info_dados",
          h5(
            htmltools::span(
              "Drogas utilizadas:"
            )
          ),
          hr(),
          h4(
            class = "saida_texto",
            textOutput(
              "numero_drogas"
            )
          )
        )
      )
    ),
    column(
      2,
      div(
        class = "card_maior",
        div(
          class = "info_dados",
          h5(
            htmltools::span(
              "Período de análise:"
            )
          ),
          hr(),
          h4(
            class = "saida_texto",
            textOutput(
              "periodo_analise"
            )
          )
        )
      )
    )
  ),
  br(),
  fluidRow(
    column(
      3,
      uiOutput(
        "filtro_microrganismo"
      )
    ),
    column(
      3,
      uiOutput(
        "tipo_cultura"
      )
    ),
    column(
      3,
      uiOutput(
        "periodo"
      )
    ),
    br(),
    br(),
    br()
  ),
  fluidRow(
    column(
      12, 
      dataTableOutput(
        "tableUI"
      )
    )
  ),
  fluidRow(
    tags$br()
    ),
  footer_s3biotech
)
