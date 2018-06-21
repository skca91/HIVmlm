#' Archivo Servidor
#' 
#' @description 
#' Archivo donde se realiza el calculo, graficos y mapa de la aplicacion
#' @param input todas las entradas de la aplicacion
#' @param output todas las salidas de la aplicacion
#' @author Stephanie Correa
#' 
server <- function(input, output) {
  
  #---------------------Reactivos
  
  #' Dato reactivo \code{entradasInput} 
  #' Permite crear un evento reactivo con el periodo que el usuario desee visualizar en la categoria por genero
  #' Permite una visualizacion dinamica
  #' @param eventReactive es la funcion que establece que el evento del selectInput sea reactivo
  #' @param input$entradas es la informacion del selectInput
  #'  
  entradasInput <- eventReactive(input$entradas,{
    switch(input$entradas,
           "20071" = "20071",
           "20072" = "20072",
           "20081" = "20081",
           "20082" = "20082",
           "20091" = "20091",
           "20092" = "20092",
           "20101" = "20101",
           "20102" = "20102",
           "20111" = "20111",
           "20112" = "20112",
           "20121" = "20121",
           "20122" = "20122",
           "20131" = "20131",
           "20132" = "20132")
  }, ignoreNULL = FALSE)
  
  #' Dato reactivo \code{datosVInput}
  #'  
  #' Permite crear un evento reactivo con la carga viral, celulas CD4 y CD8 que el usuario desee visualizar
  #' Permite una visualizacion dinamica
  #' @param eventReactive es la funcion que establece que el evento del selectInput sea reactivo
  #' @param input$DatosV es la informacion del selectInput
  #'  
  datosVInput <- eventReactive(input$DatosV,{
    switch(input$DatosV,
           "Carga viral plasmatica" = dato()$CVP,
           "Celulas CD4" = dato()$CD4,
           "Celulas CD8" = dato()$CD8)
  }, ignoreNULL = FALSE)
  
  #' Dato reactivo selectajusteInput
  #' Permite crear un evento reactivo con la carga viral, celulas CD4 y CD8 que el usuario desee visualizar
  #' Permite una visualizacion dinamica
  #' @param eventReactive es la funcion que establece que el evento del selectInput sea reactivo
  #' @param input$selectajuste es la informacion del selectInput
  #'
  selectajusteInput <- eventReactive(input$selectajuste,{
    switch(input$selectajuste,
           "Carga viral plasmatica" = dato()$CVP,
           "Celulas CD4" = dato()$CD4,
           "Celulas CD8" = dato()$CD8)
  }, ignoreNULL = FALSE)
  
  #' Dato reactivo \code{selectcargaInput}
  #' Permite crear un evento reactivo con la carga viral, celulas CD4 y CD8 que el usuario desee visualizar
  #' Permite una visualizacion dinamica
  #' @param eventReactive es la funcion que establece que el evento del selectInput sea reactivo
  #' @param input$selectcarga es la informacion del selectInput
  #'
  selectcargaInput <- eventReactive(input$selectcarga,{
    switch(input$selectcarga,
           "Carga viral plasmatica" = dato()$CVP,
           "Celulas CD4" = dato()$CD4,
           "Celulas CD8" = dato()$CD8)
  }, ignoreNULL = FALSE)
  
  #' Dato reactivo \code{dato}
  #' Permite crear un evento reactivo con la informacion cargada del archivo
  #' Permite una visualizacion dinamica
  #' @param reactive es la funcion que establece que la informacion sea reactiva
  #' El \code{file1} Carga el archivo
  #' El \code{input$file} es la informacion del archivo que se cargo
  #' \code{is.null(file1)} se realiza una condicion para chequear que realmente se carga el archivo
  #' si no cargo nada solo devuelve un \code{return()}
  #' en caso contrario, me guarda la informacion
  #' en \code{read.table}
  #' @param file1$datapath es la ubicacion del archivo
  #' @param sep es el metodo de separacion de todo el archivo
  #' @param header verifica si tiene cabecera el archivo
  #' @examples 
  #' \code{read.table(/data/proyecto/file.csv",sep=",",header=FALSE)}
  #' \code{read.table(/data/proyecto/file.csv",sep=" ",header=FALSE)}
  #' \code{read.table(/data/proyecto/file.csv",sep=";",header=FALSE)}
  #' 
  dato <-reactive({
    file1 <- input$file 
    
    if(is.null(file1)){return()}
    
    read.table(file1$datapath, sep = ",", header = TRUE)
    
    
  })
  #--------------------------Carga de archivos
  #' Muestra la informacion referente al nombre del archivo y la extension
  #' @description 
  #' Crea un \code{renderTable} con la informacion del nombre, la extension y la ubicacion del archivo
  #' Verifica si esta cargado el archivo, sino se regresa
  #' si carga muestra la informacion
  output$filedf <- renderTable({
    if(is.null(dato())){return()}
    input$file
    
  })
  #' Muestra la informacion del archivo
  #' @description 
  #' Crea un \code{renderDataTable} con la informacion cargada del archivo
  #' Primero verifica si se carga el archivo
  output$info <- renderDataTable({
    if(is.null(dato())){return()}
    dato()
  })
  #' Muestra el resumen de la informacion con las variables cargadas del sistema
  
  output$summary <- renderTable({
    dataset <- dato()
    summary(dataset)
  })
  #' Me carga el panel de la informacion del archivo
  #' @description 
  #' Cuando la informacion es cargada al sistema, aparece un pequeño panel con las opciones
  #' de acerca del archivo, informacion y resumen de la informacion
  output$tb <- renderUI({
    if(is.null(dato())){
      h5("No hay ninguna informacion cargada")
    }
    else{
      
      tabsetPanel(tabPanel("Acerca del archivo",tableOutput("filedf")),tabPanel("Informacion",dataTableOutput("info")),tabPanel("Resumen",tableOutput("summary")))
    }
  })
  
  #---------------------------------Visualizacion del mapa
  #' Visualizacion del mapa
  #' @description 
  #' primero realiza la validacion de si carga o no la informacion de los datos
  #' para cargar el mapa en el sistema
  #' si se cargo el archivo, envia un output con la variable \code{mapa1}
  #' donde va a renderizar el mapa con el paquete leaflet
  output$mapa <- renderUI({
    if(is.null(dato())){
      h5("No hay ninguna informacion cargada")
    }
    else{
      
      column(12,
             leafletOutput("mapa1")
      )
      
    }
    
  })
  #' Crea el renderLeaflet para visualizar el mapa
  #' @description 
  #' Primero realiza una condicion para evitar los datos que no tengan la longitud para visualizarla en el mapa
  #' con \code{idx} omite los datos NA y con \code{tre} carga la tabla completa con los datos omitiendo los na
  #' el \code{leaflet(map)} carga el mapa
  output$mapa1 <- renderLeaflet({
    idx <- with(info, is.na(info$LNG)==FALSE)
    tre <-info[idx,]
    leaflet(map) %>% addTiles() %>% #addProviderTiles(providers$OpenStreetMap) %>%
      # addPolygons(fill = FALSE, stroke = TRUE, color = "#03F") %>% addLegend("bottomright", colors = "#03F", labels = "Estado Merida")%>%
      addCircleMarkers(lng = tre$LNG, lat = tre$LAT, radius = 10,color = c("blue"),
                       fillOpacity = 0.3,stroke = FALSE,
                       popup = tre$NAME_2)
  })
  
  #--------------------------------Visualizacion de los graficos
  
  #' Muestra la visualizacion grafica de la exploracion de datos
  #' @description 
  #' Muestra 3 paneles con 3 categorias distintas: Genero, Edad y Carga
  #'   
  output$grafica <- renderUI({
    if(is.null(dato())){
      h5("No hay ninguna informacion cargada")
    }
    else{
      tabsetPanel(tabPanel("Genero",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("entradas","Seleccione El periodo",choices = c("20071","20072","20081","20082","20091","20092","20101","20102","20111","20112","20121","20122","20131","20132"))
                             ),
                             mainPanel(
                               plotlyOutput("piechart")
                             ))),
                  
                  tabPanel("Edad",
                           column(12,
                                  plotlyOutput("histogram")
                           )
                           
                  ),
                  tabPanel("Carga",
                           sidebarLayout(
                             sidebarPanel(
                               
                               selectInput("selectcarga","Seleccione las Variables",choices = c("Carga viral plasmatica","Celulas CD4","Celulas CD8"))
                               
                             ),
                             mainPanel(
                               plotlyOutput("line")
                             ))
                  )
      )
    }
    
  })
  #' Muestra un grafico de tortas con el genero
  #' @description 
  #' Visualiza graficos de torta de genero por periodos
  #' Primero realiza una validacion para que no muestre los datos que na
  #' Visualiza el grafico de torta por medio del paquete \code{plotly} 
  output$piechart <- renderPlotly({
    idx <- with(dato(), Peri == entradasInput() & is.na(CVP)==FALSE)
    tre <-dato()[idx,]
    sexo <- c(tre[,5])
    tiposex <- rep(NA,length(sexo))
    tiposex[sexo==0] <-'Masculino'
    tiposex[sexo==1] <-'Femenino'
    frec.tipo.sex <- table(tiposex)
    plot_ly(tre, values = ~frec.tipo.sex, type = 'pie',
            textposition = 'inside',
            textinfo = 'percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(frec.tipo.sex, ' personas'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            showlegend = FALSE) %>%
      layout(title = 'Grafico del genero por periodo',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
    
  })
  #' Muestra un histograma de los edades de los datos cargados
  output$histogram <- renderPlotly({
    idx <- with(dato(), is.na(CVP)==FALSE)
    tre <-dato()[idx,]
    plot_ly(alpha = 0.6) %>%
      add_histogram(x = ~tre$Edad)%>%
      layout(barmode = "overlay")%>%
      layout(title = 'Frecuencia de edades',
             xaxis = list(title = 'Edad',
                          zeroline = TRUE),
             yaxis = list(title = 'Frecuencia'))
  })
  
  #' Muestra un grafico de lineas con el comportamiento de la carga viral plasmatica, celulas CD4 y CD8
  output$line <- renderPlotly({
    
    variable <- selectcargaInput()
    plot_ly(dato(), x = ~as.character.Date(Peri), y = ~variable, type="scatter", text = paste("Paciente: ", dato()$Pac,"</br>Genero: ",dato()$Sex),
            mode = "markers", colors = "Set1")%>%
      layout(title = 'Comportamiento del conteo por semestre',
             yaxis = list(zeroline = FALSE,title="Variable"),
             xaxis = list(zeroline = FALSE),title="Periodo")
    
  })
  
  #----------------------------Ajuste del modelo
  
  #' Muestra el resultado de la aplicacion del modelo lineal mixto
  #' @description 
  #' permite escoger si sera por la carga viral plasmatica, celulas CD4 y CD8
  #' y si se aplicara el REML
  output$analisis <- renderUI({
    if(is.null(dato())){
      h5("No hay ninguna informacion cargada")
    }
    else{
      
      sidebarLayout(
        sidebarPanel(
          "Modelo de Intercepto Aleatorio con Media Fija",
          tags$hr(),
          selectInput("selectajuste","Seleccione las Variables",choices = c("Carga viral plasmatica","Celulas CD4","Celulas CD8")),
          checkboxInput("REML", "Restringida", TRUE)
        ),
        mainPanel(
          h4("Ajuste de Modelos por Maxima Verosimilitud"),
          verbatimTextOutput("sum")
          #tableOutput("ajuste")
        )
      )
    }
  })  
  
  #' Muestra la salida del formula del modelo lineal mixto
  output$sum <- renderPrint({
    
    variable <-  selectajusteInput() 
    M0 <- lmer(LogCVP ~ 1 + Fecha +  variable + (1|Pac), data = dato(), REML = input$REML)
    summary(M0)
  })
  
  #-------------------------------Validacion de los datos
  #' Muestra los residuos encontrados en la formaula del modelo lineal mixto
  #' @description 
  #' Primero escoger entre la carga viral plasmatica, celulas CD4 y CD8
  #' para visualiza mediante una grafica, la visualizacion de los datos
  output$validacion <- renderUI({
    if(is.null(dato())){
      h5("No hay ninguna informacion cargada")
    }
    else{
      sidebarLayout(
        sidebarPanel(
          selectInput("DatosV","Seleccione las variables:",choices = c("Carga viral plasmatica","Celulas CD4","Celulas CD8"))
        ),
        mainPanel(
          plotOutput("valido")
        )
      )
    }
  })
  #' Graficacion de los residuos del modelo lineal mixto
  output$valido <- renderPlot({
    variable <- datosVInput()
    M1RML <- lmer(variable ~ 1 + Fecha  + Edad + EI + (Fecha|Pac), data = dato(), REML = FALSE) 
    qqmath (M1RML,id =0.05)
  })
  
  
  #-----------------------------------------------Reportes
  #' Genera el reporte con los datos
  #' @description 
  #' Genera el reporte con el mapa, los graficos y los resultados estadisticos expuestos en la aplicacion 
  output$reporte <- renderUI({
    if(is.null(dato())){
      h5("No hay ninguna informacion cargada")
    }
    else{
      
      sidebarLayout(
        sidebarPanel(
          downloadButton('downloadReport', label = "Descargar Archivo")
          
        ),
        mainPanel(
          # plotOutput("Reporte")
        )
      )
    }
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('reporte', sep = '.','docx')
    },
    
    content = function(file) {
      src <- normalizePath('reporte.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'reporte.Rmd', overwrite = TRUE)
      param <- list(reml = input$REML, ajuste = selectajusteInput(), carga = selectcargaInput(), validacion = datosVInput(), periodo = entradasInput())
      
      library(rmarkdown)
      out <- render('reporte.Rmd', params = param,word_document(), envir = new.env())
      file.rename(out, file)
      
    }
  )
  # output$reporte <- renderPlot({
  #  
  # })
  
  
}