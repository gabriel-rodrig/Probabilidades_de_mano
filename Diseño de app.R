library(shiny)
library(shinyWidgets)
library(tidyverse)

#shinyWidgetsGallery()

ui <- fluidPage(
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
      numericInput("handtraps", "Cantidad de handtraps que querés ver en mano", value = 1, min = 0),
      
      #### Tamaño de mano
      checkboxInput("Turno1", "¿Mano yendo primero?", T),
      actionButton("calcular", "Calcular")
    ),
    
    mainPanel(
      textOutput("introText"),
      verbatimTextOutput("outputText"),
      textOutput("resultado"),
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
  
  calculo <- eventReactive( # Esta es una función reactiva
    input$calcular, # Asegura que el cálculo solo se realice cuando se presiona el botón
    {
      # Validación de inputs
      if (input$motor1 < 0 || input$motor2 < 0 || 
          input$starters1 < 0 || input$starters2 < 0 || 
          input$exitos1 < 0 || input$exitos2 < 0 || 
          input$handtraps < 0) {
        return(list(diagnostico = "⚠️ Error: Todos los valores deben ser mayores o iguales a cero."))
      }
      
      if (input$motor1 + input$motor2 == 0) {
        return(list(diagnostico = "⚠️ Error: Debes tener al menos un motor en tu mazo."))
      }
      
      if (input$starters1 > input$motor1 || 
          input$starters2 > input$motor2) {
        return(list(diagnostico = "⚠️ Error: No puedes tener más starters que cartas en el motor correspondiente."))
      }
      
      if (input$exitos1 > input$starters1 || 
          input$exitos2 > input$starters2) {
        return(list(diagnostico = "⚠️ Error: No puedes querer ver más starters en mano de los que tienes disponibles."))
      }
      
      D_test <- ifelse(input$motor1 + input$motor2 < 40, 
                       40, 
                       input$Turno1 + input$motor2) # Tamaño del mazo
      
      
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
    req(input$calcular) # Asegura que el cálculo solo se realice cuando se presiona el botón
    
    
    
    r <- calculo()
    
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
  
  # Lo que sale si no se ha calculado nada todavía
  output$outputText <- renderText({
    "Aquí aparecerán los resultados."
  })
}

shinyApp(ui, server)
