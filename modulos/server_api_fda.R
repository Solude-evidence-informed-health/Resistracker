library(httr)
library(jsonlite)


# BOX COSTUMIZADO QUE SERÁ USADO PARA INSERIR AS INFORMAÇÕES SOBRE OS MEDICAMENTOS
# 
# customBox <- function(title = "Meu Box Personalizado", outputId = outputId) {
#   tags$div(
#     class = "custom-box",
#     tags$div(
#       class = "title-bar",
#       h4(title),
#       tags$button(id = "minimize-btn", "-")
#     ),
#     tags$div(
#       class = "minimize-content minimized",
#       htmlOutput(outputId)
#     )
#   )
# }

customBox <- function(title, contentID, outputId = outputId) {
  tags$div(class = "custom-box",
           tags$div(class = "title-bar",
                    h4(title),
                    tags$button(class = "minimize-btn", "data-target" = paste0("#", contentID), "-")
           ),
           tags$div(id = contentID, class = "minimize-content minimized",
                    htmlOutput(outputId)
           )
  )
}


drug_data <- eventReactive(input$DTClick_droga[1], {
  nome_droga <- dados_gerais[as.numeric(input$DTClick_droga[1]), ]$Antibiotics
  # Corrigindo a consulta para pesquisar pelo nome do medicamento
  res <- GET(paste0("https://api.fda.gov/drug/label.json?search=openfda.brand_name:", nome_droga))

  content(res)$results[[1]]
})



# OUTPUT DA DESCRICAO DA DROGA
output$descricao <- renderUI({
  if (is.na(dados_gerais[as.numeric(input$DTClick_droga[1]), ]$Antibiotics) | is.null(drug_data()$description[[1]])) {
    
    data_text <- "Sem informação disponível"
    
    HTML(data_text)
  } else {
    
    data <- drug_data()
        # Acessando todos os dados como texto
    data_text <- paste(data$description[[1]])
    
    HTML(data_text)
  }

})

# OUTPUT DA FARMACOLOGIA CLINICA
output$farmacologia_clinica <- renderUI({
  if (is.na(dados_gerais[as.numeric(input$DTClick_droga[1]), ]$Antibiotics) | is.null(drug_data()$clinical_pharmacology[[1]])) {
    
    data_text <- "Sem informação disponível"
    
    HTML(data_text)
  } else {
    
    data <- drug_data()
    # Acessando todos os dados como texto
    data_text <- paste(data$clinical_pharmacology[[1]])
    
    HTML(data_text)
  }
  
})

# OUTPUT DE CONTRAINDICACOES
output$contraindicacoes <- renderUI({
  if (is.na(dados_gerais[as.numeric(input$DTClick_droga[1]), ]$Antibiotics) | is.null(drug_data()$contraindications[[1]])) {
    
    data_text <- "Sem informação disponível"
    
    HTML(data_text)
  } else {
    
    data <- drug_data()
    # Acessando todos os dados como texto
    data_text <- paste(data$contraindications[[1]])
    
    HTML(data_text)
  }
  
})

# OUTPUT DE REACOES ADVERSAS
output$reacoes_adversas <- renderUI({
  if (is.na(dados_gerais[as.numeric(input$DTClick_droga[1]), ]$Antibiotics) | is.null(drug_data()$adverse_reactions[[1]])) {
    
    data_text <- "Sem informação disponível"
    
    HTML(data_text)
  } else {
    
    data <- drug_data()
    # Acessando todos os dados como texto
    data_text <- paste(data$adverse_reactions[[1]])
    
    HTML(data_text)
  }
  
})


# OUTPUT DE INDICACOES DE USO
output$indicacoes_de_uso <- renderUI({
  if (is.na(dados_gerais[as.numeric(input$DTClick_droga[1]), ]$Antibiotics) | is.null(drug_data()$indications_and_usage[[1]])) {
    
    data_text <- "Sem informação disponível"
    
    HTML(data_text)
  } else {
    
    data <- drug_data()
    # Acessando todos os dados como texto
    data_text <- paste(data$indications_and_usage[[1]])
    
    HTML(data_text)
  }
  
})



# OUTPUT DE INTERACOES MEDICAMENTOSAS
output$interacoes_drogas <- renderUI({
  if (is.na(dados_gerais[as.numeric(input$DTClick_droga[1]), ]$Antibiotics) | is.null(drug_data()$drug_interactions[[1]])) {
    
    data_text <- "Sem informação disponível"
    
    HTML(data_text)
  } else {
    
    data <- drug_data()
    # Acessando todos os dados como texto
    data_text <- paste(data$drug_interactions[[1]])
    
    HTML(data_text)
  }
  
})



observeEvent(
  input$DTClick_droga,
  {
    showModal(
      modalDialog(
        title = paste0(dados_gerais[as.numeric(input$DTClick_droga[1]), ]$Antibiotics),
        fluidPage(
          tags$head(
            tags$script(
              HTML(
                "$(document).ready(function() {
          $('.minimize-btn').click(function() {
            var target = $(this).data('target');
            $(target).toggleClass('minimized');
          });
        });
      "
                )
              )
            ),
          #BOX DE DESCRICAO
          customBox(title = "Descrição",
                    contentID = "box_descricao",
                    outputId = "descricao"),
          
          #BOX DE FARMACOLOGIA CLINICA
          customBox(title = "Farmacologia clínica",
                    contentID = "box_farmacologia_clinica",
                    outputId = "farmacologia_clinica"),
          
          #BOX DE FARMACOLOGIA CLINICA
          customBox(title = "Contraindicações",
                    contentID = "box_contraindicacoes",
                    outputId = "contraindicacoes"),
          
          #BOX DE REACOES ADVERSAS
          customBox(title = "Reações adversas",
                    contentID = "box_reacoes_adversas",
                    outputId = "reacoes_adversas"),     
          
          #BOX DE INDICACOES DE USO
          customBox(title = "Indicações de uso",
                    contentID = "box_indicacoes_de_uso",
                    outputId = "indicacoes_de_uso"),
          
          
          #BOX DE INDICACOES DE USO
          customBox(title = "Interações medicamentosas",
                    contentID = "box_interacoes_drogas",
                    outputId = "interacoes_drogas")
        ),
        size = "l",
        easyClose = TRUE,
        footer = footer_s3biotech
      )
    )
  }
)
