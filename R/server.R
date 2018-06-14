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
  #' 
  output$filedf <- renderTable({
    if(is.null(dato())){return()}
    input$file
    
  })
  output$info <- renderDataTable({
    if(is.null(dato())){return()}
    dato()
  })
  
  output$summary <- renderTable({
    dataset <- dato()
    summary(dataset)
  })
  
  output$tb <- renderUI({
    if(is.null(dato())){
      h5("No hay ninguna informacion cargada")
      h5(helpText("Los datos deben estar separados con coma (,)"))
      h5(helpText("Datos obligatorios:"))
      h5(helpText("PAC -> Id del paciente, valor numerico"))
      h5(helpText("CVP -> Carga viral plasmatica"))
      h5(helpText("CD4 -> Celulas TCD4"))
      h5(helpText("CD8 -> Celulas TCD8"))
      h5(helpText("Genero -> 0 es masculino, 1 es femenino"))
      h5(helpText("Sex -> f o m"))
      h5(helpText("Edad"))
      h5(helpText("Peri -> Periodo por semestres. Ejem: 20071"))
    }
    else{
      
      tabsetPanel(tabPanel("Acerca del archivo",tableOutput("filedf")),tabPanel("Informacion",dataTableOutput("info")),tabPanel("Resumen",tableOutput("summary")))
    }
  })
  
  #---------------------------------Visualizacion del mapa
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
  
  output$line <- renderPlotly({
    
    variable <- selectcargaInput()
    plot_ly(dato(), x = ~as.character.Date(Peri), y = ~variable, type="scatter", text = paste("Paciente: ", dato()$Pac,"</br>Genero: ",dato()$Sex),
            mode = "markers", colors = "Set1")%>%
      layout(title = 'Comportamiento del conteo por semestre',
             yaxis = list(zeroline = FALSE,title="Variable"),
             xaxis = list(zeroline = FALSE),title="Periodo")
    
  })
  
  #----------------------------Ajuste del modelo
  
  
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
  
  output$sum <- renderPrint({
     
     variable <-  selectajusteInput() 
     M0 <- lmer(LogCVP ~ 1 + Fecha +  variable + (1|Pac), data = dato(), REML = input$REML)
     summary(M0)
  })
  
  #-------------------------------Validacion de los datos
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
  output$valido <- renderPlot({
    variable <- datosVInput()
    M1RML <- lmer(variable ~ 1 + Fecha  + Edad + EI + (Fecha|Pac), data = dato(), REML = FALSE) 
    qqmath (M1RML,id =0.05)
    
    
    
  })
  
  
  #-----------------------------------------------Reportes
  
  output$reporte <- renderUI({
    if(is.null(dato())){
      h5("No hay ninguna informacion cargada")
    }
    else{
      
      sidebarLayout(
        sidebarPanel(
          checkboxInput("mapainput",label = "Incluir mapa")
          
        ),
        mainPanel(
          h5("Holaa")
        )
      )
    }
  })
  
}



