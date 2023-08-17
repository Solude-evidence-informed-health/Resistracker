library(shiny)
library(DT)
library(tidyverse)
source(file = "modulos/footer_s3biotech.R", local = TRUE)

ui_prevalencia <- tabPanel(
  title = "Prevalência",
  fluidRow(
    div(
      class = "titulo_pagina",
      strong(
        h2(
          "Prevalência"
        )
      )
    )
  ),
  br(),
  fluidRow(
    column(
      3,
      div(
        class = "card",
        div(
          class = "info_dados",
          h5(
            htmltools::span(
              "Sítio selecionado::"
            )
          ),
          hr(),
          h4(
            class = "saida_texto",
            textOutput(
              "sitio_selecionado"
            )
          )
        )
      )
    ),
    column(
      3,
      div(
        class = "card",
        div(
          class = "info_dados",
          h5(
            htmltools::span(
              "Número de microrganismos:"
            )
          ),
          hr(),
          h4(
            class = "saida_texto",
            textOutput(
              "numero_microrganismos"
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
        "filtro_sitio_prevalencia"
      )
    )
  ),
  fluidRow(
    column(
      12,
      dataTableOutput(
        "tabela_sitio_prevalencia"
      )
    )
  ),
  fluidRow(
    tags$br()
    ),
  footer_s3biotech
)