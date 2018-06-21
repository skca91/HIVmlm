#' Archivo Interfaz Grafica
#' 
#' @description 
#' Archivo de la interfaz grafica donde permite definir la estructura grafica de la aplicacion
#' @author Stephanie Correa

#'  Es la interfaz de la aplicacion
#'  
#'  @description 
#'  El \code{fluidPage} permite cargar todo el diseño grafico de la aplicacion
ui <- fluidPage(
  
  # App title ----
  #' Titulo de la Aplicacion
  titlePanel("HIVmlm"),
  fluidRow(
    navbarPage(img(src="lacito_rojo.png", alt="HIV", height = 20, width = 20),
               #' Carga del Archivo
               #' permite cargar un archivo externo de la aplicacion para poderlo procesar en la aplicacion
               tabPanel("Carga de Datos",
                        column(4,
                               fileInput("file", label = h3("Carga de Archivo"),placeholder = "Ningun archivo seleccionado"),
                               tags$hr(),
                               h5(helpText("Debe subir un archivo con extension .csv"))
                               
                        ),
                        column(8,
                               uiOutput("tb")
                        )
               ),
               
               #   -----Fin de Carga de datos
               #' Visualizacion del mapa
               #' Permite visualizar en el mapa, la propagacion de la enfermedad con respecto a la ubicacion geografica
               tabPanel("Visualizacion en el mapa",
                        
                        uiOutput("mapa")
               ),
               
               #------ separador
               #' Exploracion de datos
               #' Permite visualizar mediante graficos los datos cargados por medio del archivo
               #' graficos de torta, histogramas y graficos de puntos
               tabPanel("Exploracion de datos",
                        
                        uiOutput("grafica")
                        
               ),
               #----- Fin de analisis de resultados
               #' Ajuste del modelo
               #' 
               #' Permite observar los valores del modelo lineal mixto con los datos cargados en la aplicacion
               tabPanel("Ajuste del modelo",
                        
                        uiOutput("analisis")
                        
               ),
               #' Validacion del Modelo
               #'  
               #' Permite observar mediante graficos los residuos del modelo lineal mixto, para comprobarsi los datos se ajustan bien
               tabPanel("Validacion del Modelo",
                        
                        uiOutput("validacion")
                        
               ),
               #' Generar Reporte
               #'  
               #' Permite descargar un archivo pdf con la informacion que el usuario desee visualizar y guardar
               tabPanel("Generar Reporte",
                        
                        uiOutput("reporte")
               )
               
    )
    
  )
)