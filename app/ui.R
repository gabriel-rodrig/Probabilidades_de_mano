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

