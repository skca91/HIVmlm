library(shinythemes)


ui <- fluidPage(# App title ----
                
                titlePanel("HIVmlm"),
                fluidRow(
                 # shinythemes::themeSelector(),
                  navbarPage(
                    theme = shinytheme("flatly"),
                    img(
                      src = "lacito_rojo.png",
                      alt = "HIV",
                      height = 20,
                      width = 20
                    ),
                    tabPanel(
                      "Carga de Datos",
                      column(
                        4,
                        fileInput(
                          "file",
                          label = h3("Carga de Archivo"),
                          placeholder = "Ningun archivo seleccionado"
                        ),
                        tags$hr(),
                        h4(
                          "Antes de cargar la informacion, los nombres de las variables a cargar en el archivo deben coincidir con las mostradas en la tabla"
                        ),
                        tags$hr(),
                        h5(helpText("Debe subir un archivo con extension .csv"))
                      ),
                      column(8,
                             uiOutput("tb"))
                    ),
                    
                    #   -----Fin de Carga de datos
                    tabPanel("Visualizacion en el mapa",
                             
                             uiOutput("mapa")),
                    
                    #------ separador
                    tabPanel("Exploracion de datos",
                             
                             uiOutput("grafica")),
                    #----- Fin de analisis de resultados
                    tabPanel("Ajuste del modelo",
                             
                             uiOutput("analisis")),
                    tabPanel("Validacion del Modelo",
                             
                             uiOutput("validacion")),
                    tabPanel("Generar Reporte",
                             
                             uiOutput("reporte"))
                    
                  )
                  
                ))