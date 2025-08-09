library(shiny)
library(shinyWidgets)
library(tidyverse)
library(rsconnect)

#shinyWidgetsGallery()


#### UI ####
ui <- fluidPage(
  
  #Encabezado
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('Encabezado.png');
        background-size: cover;
        background-repeat: no-repeat;
        background-attachment: fixed;
      }
    "))
  ),
  

#tags$style(HTML("
#    h1, h2, h3 { color: white; }
#    p, span, .shiny-text-output { color: #f0f0f0; }
#  ")),  

#  tags$style(HTML("
#    .mainPanel label,
#    .mainPanel span,
#    .mainPanel .shiny-text-output {
#      color: black;
#    }
#  ")),

tags$style(HTML("
    /* Estilo para títulos generales */
    h1, h2, h3 { color: white; }

    /* Texto SOLO en el main panel */
    .col-sm-8 p,
    .col-sm-8 span,
    .col-sm-8 .shiny-text-output,
    .col-sm-8 label {
      color: #f0f0f0;
    }

    /* Texto SOLO en el sidebar panel */
    .col-sm-4 p,
    .col-sm-4 span,
    .col-sm-4 .shiny-text-output,
    .col-sm-4 label {
      color: black;
    }
")),

  
  titlePanel("Optimizador de manos iniciales"),
  
  sidebarLayout(
    sidebarPanel(
      
      #### Inputs primer motor
      numericInput("motor1", "Cartas del motor principal", value = 20, min = 0),
      numericInput("starters1", "Starters motor principal", value = 9, min = 0),
      numericInput("exitos1", "Starters que querés ver en mano", value = 2, min = 0),
      
      #### Inputs segundo motor
      numericInput("motor2", "Cartas del motor secundario", value = 0, min = 0),
      numericInput("starters2", "Starters motor secundario", value = 0, min = 0),
      numericInput("exitos2", "Starters secundarios que querés ver mano", value = 0, min = 0),
      
      #### Input de handtraps
      numericInput("handtraps", "Cantidad de handtraps que querés ver en mano", value = 3, min = 0),
      
      #### Tamaño de mano
      checkboxInput("Turno1", "¿Mano yendo primero?", T),
      actionButton("calcular", "Calcular")
    ),
    
    mainPanel(
        tags$div(
          textOutput("introText"),
          style = "margin-bottom: 20px;"  # Espacio debajo del introText
        ),
        textOutput("resultado")
    )
      
  )
)


server <- function(input, output, session) {
  # Instrucciones de uso
  output$introText <- renderText({
    paste("Ingresa el tamaño de tus motores y deja que el maximizador de consistencia",
          "te indique la cantidad óptima de handtraps que deberías llevar en tu mazo.",
          "Dada la cantidad de Handtraps y starters de cada motor que quieras robar ",
          "en tu mano inicial, el optimizador te indicará cuantas handtraps deberías llevar",
          "para maximizar tus chances de robar la cantidad deseada de cartas de cada tipo.")
  })
  
  # Función para calcular la cantidad óptima de handtraps y tamaño de mazo
  
  calculo <- eventReactive(
    input$calcular, # Asegura que el cálculo solo se realice cuando se presiona el botón
    {
      # Validación de inputs
      if (input$motor1 < 0 || input$motor2 < 0 || 
          input$starters1 < 0 || input$starters2 < 0 || 
          input$exitos1 < 0 || input$exitos2 < 0 || 
          input$handtraps < 0) { # Verifica si hay valores negativos
        return(list(diagnostico = "⚠️ Error: Todos los valores deben ser mayores o iguales a cero."))
      }
      
      if (input$motor1 + input$motor2 == 0) { # Verifica que haya motores
        return(list(diagnostico = "⚠️ Error: Debes tener al menos un motor en tu mazo."))
      }
      
      if (input$starters1 > input$motor1 || 
          input$starters2 > input$motor2) { # Verifica que no haya más starters que cartas en el motor
        return(list(diagnostico = "⚠️ Error: No puedes tener más starters que cartas en el motor correspondiente."))
      }
      
      if (input$exitos1 > input$starters1 || 
          input$exitos2 > input$starters2) { # Verifica que no se roben + starters de los q hay
        return(list(diagnostico = "⚠️ Error: No puedes querer ver más starters en mano de los que tienes disponibles."))
      }
      
      D_test <- ifelse(input$motor1 + input$motor2 < 40, 
                       40, 
                       input$motor1 + input$motor2) # Tamaño del mazo
      
      
      motor1    <- input$motor1
      motor2    <- input$motor2
      starters1 <- input$starters1
      starters2 <- input$starters2
      exitos1   <- input$exitos1
      exitos2   <- input$exitos2
      handreq   <- input$handtraps
      robo      <- ifelse(input$Turno1 == T, 5, 6)
      
      p <- 0 
      D_optimo <- D_test 
      
      if ((exitos1 + exitos2 + handreq) != robo) {
        return(list(diagnostico = paste(
          "⚠️ Error: La suma de starters y handtraps que deseas robar debe ser igual a", robo
        )))
      }
      
      for (deck_size in 40:60) {
        
        non_engine = deck_size - motor1 - motor2
        
        if (non_engine == 0) {
          
          next # Si no hay cartas fuera de los motores, salta a la siguiente iteración
        } else {
          p <- max(p, 
                   (choose(motor1, starters1) * choose(non_engine, handreq) * choose(motor2, starters2)) / 
                     (choose(deck_size, robo)))
          
          D_optimo <- ifelse(p > (choose(motor1, starters1) * choose(non_engine, handreq) * choose(motor2, starters2)) / 
                                 (choose(deck_size, robo)),
                             deck_size,
                             D_optimo)
          
          next
        }
      }
      
      list(optimo = D_optimo, robo = robo)
      
    }) 
  
  # Mostrar los resultados en la salida
  
  output$resultado <- renderText({
    # Mostrar placeholder hasta que se presione "Calcular"
    if (is.null(input$calcular) || input$calcular == 0) {
      return("Aquí aparecerán los resultados.")
    }
    
    # A partir de acá, 'calcular' ya fue presionado al menos una vez
    r <- calculo()   # eventReactive que se dispara con input$calcular
    
    if (!is.null(r$diagnostico)) {
      return(r$diagnostico)
    }
    
    if (is.na(r$optimo)) {
      return("No se encontró un tamaño óptimo de mazo. Verifica que los tamaños de los motores sean razonables.")
    }
    

    paste0(
      "✅ Tamaño óptimo del mazo es: ", r$optimo,
      " para robar ", input$handtraps, " handtraps en una mano de ", r$robo, " cartas.\n",
      "👉 Deberías llevar un total de ",
      round(r$optimo - input$motor1 - input$motor2, 0),
      " cartas en tu mazo, además de los motores."
          )

    })
  
  
}

shinyApp(ui, server)
