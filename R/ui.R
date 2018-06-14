
ui <- fluidPage(
  
  # App title ----
  titlePanel("UNET"),
  fluidRow(
    navbarPage(img(src = "logo_unet.png", height = 30, width = 30),
               tabPanel("Carga de Datos",
                        column(4,
                               fileInput("file", label = h3("Carga de Archivo")),
                               tags$hr(),
                               h5(helpText("Debe subir un archivo con extension .csv")),
                               h5(helpText("Los datos deben estar separados con coma (,)")),
                               h5(helpText("Datos obligatorios:")),
                               h5(helpText("PAC -> Id del paciente, valor numerico")),
                               h5(helpText("CVP -> Carga viral plasmatica")),
                               h5(helpText("CD4 -> Celulas TCD4")),
                               h5(helpText("CD8 -> Celulas TCD8")),
                               h5(helpText("Genero -> 0 es masculino, 1 es femenino")),
                               h5(helpText("Sex -> f o m")),
                               h5(helpText("Edad")),
                               h5(helpText("Peri -> Periodo por semestres. Ejem: 20071"))
                               
                        ),
                        column(8,
                               uiOutput("tb")
                        )
               ),
               
               #   -----Fin de Carga de datos
               
               tabPanel("Visualizacion en el mapa",
                        
                        uiOutput("mapa")
               ),
               
               #------ separador
               
               tabPanel("Exploracion de datos",
                        
                        uiOutput("grafica")
                        
               ),
               #----- Fin de analisis de resultados
               tabPanel("Ajuste del modelo",
                        
                        uiOutput("analisis")
                        
               ),
               tabPanel("Validacion del Modelo",
                        
                        uiOutput("validacion")
                        
               ),
               tabPanel("Generar Reporte",
                        
                        uiOutput("reporte")
               )
               
    )
    
  )
)