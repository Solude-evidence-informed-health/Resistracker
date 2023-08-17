library(shiny)
library(tidyverse)
library(htmltools)
library(shinyWidgets)
library(googlesheets4)
library(apexcharter)
library(DT)
library(shinybusy)

# icones e cores - painel
source("modulos/icones_e_cores_painel.R")

# ui - modulos
source("modulos/ui_sensibilidade.r", local = TRUE)
source("modulos/ui_evolucao.R", local = TRUE)
source("modulos/ui_prevalencia.R", local = TRUE)


# app
ui <- fluidPage(
  includeCSS(
    "www/style.css"
  ),
  add_busy_spinner(
    spin = "pixel",
    position = "full-page",
    onstart = TRUE,
    color = "#4f749b"
  ),
  tags$head(
    tags$link(
      rel = "shortcut icon",
      href = icon_page
    ),
    tags$script(
      HTML("
        $(document).ready(function() {
          $('#minimize-btn').click(function() {
            $('.minimize-content').toggleClass('minimized');
          });
        });
      ")
    )
  ),
  titlePanel(
    title = div(
      class = "mesma_linha",
      div(
        class = "esquerda",
        tags$img(
          src = ebserh_icon,
          height = "50px"
        )
      ),
      div(
        class = "resistracker_position",
        img(
          src = icon_page,
          height = "50px",
          style = "margin-right: 10px;"
        ),
        "RESISTRACKER"
      )
    ),
    windowTitle = "RESISTRACKER"
  ),
  navbarPage(
    title = div(
      img(
        src = icon_page
      )
    ),
    ui_prevalencia,
    ui_evolucao,
    ui_sensibilidade
  )
)


server <- function(input, output, session) {

  
  #server - modulos
  source("modulos/dados_painel.R", local = TRUE)

  source("modulos/server_sensibilidade.r", local = TRUE)

  source("modulos/server_evolucao.R", local = TRUE)

  source("modulos/server_prevalencia.r", local = TRUE)
  
  source("modulos/server_api_fda.R", local = TRUE)
}

shinyApp(ui, server, options = list(host = "0.0.0.0", port = 3838))
