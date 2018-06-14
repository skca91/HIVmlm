
server <- function(input, output) {
  
  #---------------------Reactivos
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
  datosVInput <- eventReactive(input$DatosV,{
    switch(input$DatosV,
           "CVP" = CVP,
           "CD4" = CD4,
           "CD8" = CD8)
  }, ignoreNULL = FALSE)
  
  selectajusteInput <- eventReactive(input$selectajuste,{
    switch(input$selectajuste,
           "CVP" = CVP,
           "CD4" = CD4,
           "CD8" = CD8)
  }, ignoreNULL = FALSE)
  
  selectcargaInput <- eventReactive(input$selectcarga,{
    switch(input$selectcarga,
           "CVP" = CVP,
           "CD4" = CD4,
           "CD8" = CD8)
  }, ignoreNULL = FALSE)
  
  dato <-reactive({
    file1 <- input$file 
    
    if(is.null(file1)){return()}
    
    dat <- read.table(file = file1$datapath, sep = ",", header = TRUE)
    
  })
  #--------------------------Carga de archivos
  output$filedf <- renderTable({
    if(is.null(dato())){return()}
    file1<-input$file
    x <- paste(file1$name, collapse = ".")
    x
    
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
                               
                               selectInput("selectcarga","Seleccione las Variables",choices = c("CVP","CD4","CD8"),selected = CVP)
                               
                             ),
                             mainPanel(
                               plotlyOutput("line")
                             ))
                  )
      )
    }
    
  })
  output$piechart <- renderPlotly({
    idx <- with(dat, Peri == entradasInput() & is.na(dat$CVP)==FALSE)
    tre <-dat[idx,]
    sexo <- c(tre[,5])
    tiposex <- rep(NA,length(sexo))
    tiposex[sexo==0] <-'Masculino'
    tiposex[sexo==1] <-'Femenino'
    frec.tipo.sex <- table(tiposex)
    plot_ly(dat, values = ~frec.tipo.sex, type = 'pie',
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
    idx <- with(dat, is.na(dat$CVP)==FALSE)
    tre <-dat[idx,]
    plot_ly(alpha = 0.6) %>%
      add_histogram(x = ~tre$Edad)%>%
      layout(barmode = "overlay")%>%
      layout(title = 'Frecuencia de edades',
             xaxis = list(title = 'Edad',
                          zeroline = TRUE),
             yaxis = list(title = 'Frecuencia'))
  })
  
  output$line <- renderPlotly({
    
    #plot(as.character.Date(Peri),selectcargaInput() ,type="o",group=Pac,xlab="Fecha",ylab="Variable",main="Comportamiento del conteo por periodo")
    idx <- with(dat, is.na(dat$CVP)==FALSE)
    tre <-dat[idx,]
    plot_ly(tre, x = ~as.character.Date(tre$Peri), y = tre$CVP, type="scatter", text = paste("Paciente: ", tre$Pac,"</br>Genero: ",tre$Sex),
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
          selectInput("selectajuste","Seleccione las Variables",choices = c("CVP","CD4","CD8"),selected = CVP),
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
    idx <- with(dat, is.na(dat$CVP)==FALSE)
    tre <-dat[idx,]
    variable <- selectajusteInput() 
    
    M0 <- lmer(LogCVP ~ 1 + Fecha + variable + (1|Pac), tre, REML = input$REML)
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
          selectInput("DatosV","Seleccione las variables:",choices = c("CVP","CD4","CD8"),selected = CVP)
        ),
        mainPanel(
          plotOutput("valido")
        )
      )
    }
  })
  output$valido <- renderPlot({
    variable <- datosVInput()
    M1RML <- lmer(variable ~ 1 + Fecha  + Edad + EI + (Fecha|Pac), dato(), REML = FALSE) 
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
          checkboxInput("mapainput",label = "Incluir mapa"),
          selectInput("valor1",label = "Region",selected = "east",choices = c("east","west","north","south")),
          fileInput("valor2",label = "Input",placeholder = "No file selected")
          
        ),
        mainPanel(
          h5("Holaa")
        )
      )
    }
  })
  
}



