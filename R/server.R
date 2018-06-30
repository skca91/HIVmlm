#' Funcion comprobar
#' @description
#' Permite revisar la extension del archivo cargado
#'
comprobar <- function(extension, nombreArch) {
  if (grepl(extension, nombreArch, fixed = TRUE)) {
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
#' Funcion filename
#' @description
#' Permite guardar el nombre del reporte a descargar con su extension
filename = function() {
  paste('reporte', sep = '.', 'docx')
}



listaVariable <-
  c("Id",
    "Edad",
    "Sex",
    "Genero",
    "Periodo",
    "CD4",
    "CD8",
    "CVP",
    "Municipio")

descripcion <- c(
  "Es el id del paciente",
  "Edad en ese periodo de tiempo",
  "Es el sexo del paciente",
  "Es el genero del paciente",
  "Periodo en el que se trata el paciente",
  "Conteo de las celulas CD4",
  "Conteo de las celulas CD8",
  "Conteo de la carga viral plasmatica",
  "Es el id del municipio"
)
ejemplos <- c(
  "Es numerico: 1,2,3,4...100",
  "Es un caracter: f=femenino, m=masculino",
  "Es numerico: 0 es masculino, 1 es femenino",
  "Es una cadena de caracteres: 20071, 20132",
  "Es numerico: 45,29",
  "Es numerico: 3000,100,375",
  "Es numerico: 4000,156,543",
  "Es numerico: 1000,432,20",
  "157 ->Municipio Alberto Adriani Estado Merida"
)
cargaEjemplo <- data.frame(listaVariable, descripcion, ejemplos)

#' Funcion buscarCabeceras
#' @description
#' Permite verificar que los nombres de las variables esten correctos
buscarCabeceras <- function(archivo) {
  y <- listaVariable[match(colnames(archivo), listaVariable)]
  y.sin.na <- y[!is.na(y)]
  if (length(y.sin.na) < 1) {
    return(FALSE)
  } else{
    if (isTRUE(all.equal(y.sin.na, listaVariable))) {
      return(TRUE)
    }
    else{
      return(FALSE)
    }
    
  }
  
}

server <- function(input, output)
{
  #---------------------Reactivos
  
  #' Dato reactivo \code{entradasInput}
  #' Permite crear un evento reactivo con el periodo que el usuario desee visualizar en la categoria por genero
  
  entradasInput <- eventReactive(input$entradas, {
    switch(
      input$entradas,
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
      "20132" = "20132"
    )
  }, ignoreNULL = FALSE)
  
  #' Dato reactivo \code{datosVInput}
  #'
  #' Permite crear un evento reactivo con la carga viral, celulas CD4 y CD8 que el usuario desee visualizar
  
  datosVInput <- eventReactive(input$DatosV, {
    switch(
      input$DatosV,
      "Carga viral plasmatica" = dato()$CVP,
      "Celulas CD4" = dato()$CD4,
      "Celulas CD8" = dato()$CD8
    )
  }, ignoreNULL = FALSE)
  
  #' Dato reactivo selectajusteInput
  #' Permite crear un evento reactivo con la carga viral, celulas CD4 y CD8 que el usuario desee visualizar
  
  selectajusteInput <- eventReactive(input$selectajuste, {
    switch(
      input$selectajuste,
      "Carga viral plasmatica" = dato()$CVP,
      "Celulas CD4" = dato()$CD4,
      "Celulas CD8" = dato()$CD8
    )
  }, ignoreNULL = FALSE)
  
  #' Dato reactivo \code{selectcargaInput}
  #' Permite crear un evento reactivo con la carga viral, celulas CD4 y CD8 que el usuario desee visualizar
  
  selectcargaInput <- eventReactive(input$selectcarga, {
    switch(
      input$selectcarga,
      "Carga viral plasmatica" = dato()$CVP,
      "Celulas CD4" = dato()$CD4,
      "Celulas CD8" = dato()$CD8
    )
  }, ignoreNULL = FALSE)
  
  #' Dato reactivo \code{dato}
  #' Permite crear un evento reactivo con la informacion cargada del archivo
  
  dato <- reactive({
    file1 <- input$file
    if (is.null(file1)) {
      return()
    }
    read.table(file1$datapath, sep = ",", header = TRUE)
    
  })
  
  
  #' Dato Reactivo extension
  extension <- reactive({
    file1 <- input$file
    nombre <- file1$name
    ext <- ".csv"
    if (comprobar(ext, nombre) == FALSE) {
      return(FALSE)
    }
    
  })
  
  #--------------------------Carga de archivos
  #' Muestra la informacion referente al nombre del archivo y la extension
  #' @description
  #' Crea un \code{renderTable} con la informacion del nombre, la extension y la ubicacion del archivo
  #' Verifica si esta cargado el archivo, sino se regresa
  #' si carga muestra la informacion
  output$filedf <- renderTable({
    if (is.null(dato())) {
      return()
    }
    input$file
    
  })
  #' Muestra la informacion del archivo
  #' @description
  #' Crea un \code{renderDataTable} con la informacion cargada del archivo
  #' Primero verifica si se carga el archivo
  output$info <- renderDataTable({
    if (is.null(dato())) {
      return()
    }
    dato()
  })
  #' Muestra el resumen de la informacion con las variables cargadas del sistema
  
  output$summary <- renderTable({
    dataset <- dato()
    summary(dataset)
  })
  
  output$ejemplo <- renderTable({
    cargaEjemplo
  })
  output$municipios <- renderDataTable({
    idx <-  with(info, info$ID_1 == 15)
    tre <- info[idx, ]
    IdEstado <- tre$ID_1
    NombreEstado <- tre$NAME_1
    IdMunicipio <- tre$ID_2
    NombreMunicipio <- tre$NAME_2
    mun <-
      data.frame(IdEstado, NombreEstado , IdMunicipio, NombreMunicipio)
    mun
  })
  #' Me carga el panel de la informacion del archivo
  #' @description
  #' Cuando la informacion es cargada al sistema, aparece un pequeño panel con las opciones
  #' de acerca del archivo, informacion y resumen de la informacion
  output$tb <- renderUI({
    if (is.null(dato())) {
      tabsetPanel(
        tabPanel("Acerca de la carga del archivo", tableOutput("ejemplo")),
        tabPanel(
          "Acerca de los municipios del Estado Merida",
          dataTableOutput("municipios")
        )
      )
    }
    else{
      if (isFALSE(extension())) {
        h4("La extension no es la recomendada, debe ser una extension .csv")
      } else{
        if (buscarCabeceras(dato()) == TRUE) {
          tabsetPanel(
            tabPanel("Acerca del archivo", tableOutput("filedf")),
            tabPanel("Informacion", dataTableOutput("info")),
            tabPanel("Resumen", tableOutput("summary"))
          )
        } else{
          h5("Los nombres de las variables no coinciden")
        }
        
      }
      
    }
    
    
  })
  
  #---------------------------------Visualizacion del mapa
  #' Visualizacion del mapa
  
  output$mapa <- renderUI({
    if (is.null(dato())) {
      h5("No hay ninguna informacion cargada")
    } else{
      if (isFALSE(extension())) {
        h5("La extension no es la recomendada")
      } else{
        if (buscarCabeceras(dato()) == TRUE) {
          column(12,
                 leafletOutput("mapa1"))
        } else{
          h5("Los nombres de las variables no coinciden")
        }
        
      }
    }
    
  })
  #' Crea el renderLeaflet para visualizar el mapa
  output$mapa1 <- renderLeaflet({
    idx <-  which(info$ID_2 %in%  dato()$Municipio)
    tre <- info[idx, ]
    
    ggg <-  group_by(dato(), Id)
    mun <- group_by(ggg, Municipio)
    cd4 <- summarise(mun, mean(CD4, na.rm = TRUE))
    cvp <- summarise(mun, mean(CVP, na.rm = TRUE))
    
    genero <-
      summarise(mun, mean(Genero == 0) * 100, mean(Genero == 1) * 100) #Genero
    desviacion <-
      summarise(mun, sd(CD4, na.rm = TRUE), sd(CVP, na.rm = TRUE)) #Desviacion estandar
    Num <- table(mun$Municipio) / 14
    Rango <- 20 - 6
    max <- max(Num)
    
    tre <- data.frame(tre, cd4$`mean(CD4, na.rm = TRUE)`)
    tre <- data.frame(tre, cvp$`mean(CVP, na.rm = TRUE)`)
    tre <-
      data.frame(tre,
                 genero$`mean(Genero == 0) * 100`,
                 genero$`mean(Genero == 1) * 100`)
    tre <-
      data.frame(tre,
                 desviacion$`sd(CD4, na.rm = TRUE)`,
                 desviacion$`sd(CVP, na.rm = TRUE)`)
    leaflet(map) %>% addTiles() %>% #addProviderTiles(providers$OpenStreetMap) %>%
      # addPolygons(fill = FALSE, stroke = TRUE, color = "#03F") %>% addLegend("bottomright", colors = "#03F", labels = "Estado Merida")%>%
      addCircleMarkers(
        lng = tre$LNG,
        lat = tre$LAT,
        radius = ((Num / max) * Rango) + 6,
        color = c("purple"),
        fillOpacity = 0.6,
        stroke = FALSE,
        popup = paste(
          "<Strong>Municipio </Strong> ",
          tre$NAME_2,
          "</br>",
          "</br>Numero de Personas: ",
          Num,
          "</br>Porcentaje de Hombres: ",
          tre$genero..mean.Genero....0....100.,
          "</br>Porcentaje de Mujeres: ",
          tre$genero..mean.Genero....1....100.,
          "</br>Media de Celulas CD4: ",
          tre$cd4..mean.CD4..na.rm...TRUE..,
          "</br>Media de Carga viral plasmatica: ",
          tre$cvp..mean.CVP..na.rm...TRUE..,
          "</br>Desviacion estandar de Celulas CD4: ",
          tre$desviacion..sd.CD4..na.rm...TRUE..,
          "</br>Desviacion estandar de Carga viral plasmatica: ",
          tre$desviacion..sd.CVP..na.rm...TRUE..
        )
      )
  })
  
  #--------------------------------Visualizacion de los graficos
  
  #' Muestra la visualizacion grafica de la exploracion de datos
  
  output$grafica <- renderUI({
    if (is.null(dato())) {
      h5("No hay ninguna informacion cargada")
    }
    else{
      if (isFALSE(extension())) {
        h5("La extension no es la recomendada")
      } else{
        if (buscarCabeceras(dato()) == TRUE) {
          tabsetPanel(
            tabPanel("Genero",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           "entradas",
                           "Seleccione El periodo",
                           choices = c(
                             "20071",
                             "20072",
                             "20081",
                             "20082",
                             "20091",
                             "20092",
                             "20101",
                             "20102",
                             "20111",
                             "20112",
                             "20121",
                             "20122",
                             "20131",
                             "20132"
                           )
                         )
                       ),
                       mainPanel(plotlyOutput("piechart"))
                     )),
            
            
            
            tabPanel("Edad",
                     column(12,
                            plotlyOutput(
                              "histogram"
                            ))),
            tabPanel("Marcadores",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           "selectcarga",
                           "Seleccione las Variables",
                           choices = c("Carga viral plasmatica", "Celulas CD4", "Celulas CD8")
                         )
                         
                       ),
                       mainPanel(plotlyOutput("line"))
                     ))
          )
        } else{
          h5("Los nombres de las variables no coinciden")
        }
        
      }
    }
  })
  
  #' Muestra un grafico de tortas con el genero
  
  output$piechart <- renderPlotly({
    idx <-
      with(dato(), Periodo == entradasInput() & is.na(CVP) == FALSE)
    tre <- dato()[idx,]
    sexo <- c(tre[, 5])
    tiposex <- rep(NA, length(sexo))
    tiposex[sexo == 0] <- 'Masculino'
    tiposex[sexo == 1] <- 'Femenino'
    frec.tipo.sex <- table(tiposex)
    plot_ly(
      tre,
      values = ~ frec.tipo.sex,
      type = 'pie',
      textposition = 'inside',
      textinfo = 'percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      text = ~ paste(frec.tipo.sex, ' personas'),
      marker = list(
        colors = colors,
        line = list(color = '#FFFFFF', width = 1)
      ),
      showlegend = FALSE
    ) %>%
      layout(
        title = 'Grafico del genero por periodo',
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      )
    
    
  })
  #' Muestra un histograma de los edades de los datos cargados
  output$histogram <- renderPlotly({
    plot_ly(alpha = 0.6) %>%
      add_histogram(x = ~ dato()$Edad) %>%
      layout(barmode = "overlay") %>%
      layout(
        title = 'Frecuencia de edades',
        xaxis = list(title = 'Edad',
                     zeroline = TRUE),
        yaxis = list(title = 'Frecuencia')
      )
  })
  
  #' Muestra un grafico de lineas con el comportamiento de la carga viral plasmatica, celulas CD4 y CD8
  output$line <- renderPlotly({
    variable <- selectcargaInput()
    plot_ly(
      dato(),
      x = ~ as.character.Date(Periodo),
      y = ~ variable,
      type = "scatter",
      text = paste("Paciente: ", dato()$Id, "</br>Genero: ", dato()$Sex),
      mode = "markers",
      colors = "Set1"
    ) %>%
      layout(
        title = 'Comportamiento del conteo por semestre',
        yaxis = list(zeroline = FALSE, title = 'Variable'),
        xaxis = list(zeroline = FALSE),
        title = 'Periodo'
      )
    
  })
  
  #----------------------------Ajuste del modelo
  
  #' Muestra el resultado de la aplicacion del modelo lineal mixto
  
  output$analisis <- renderUI({
    if (is.null(dato())) {
      h5("No hay ninguna informacion cargada")
    }
    else{
      if (isFALSE(extension())) {
        h5("La extension no es la recomendada")
      } else{
        if (buscarCabeceras(dato()) == TRUE) {
          sidebarLayout(
            sidebarPanel(
              "Modelo de Intercepto Aleatorio con Media Fija",
              tags$hr(),
              selectInput(
                "selectajuste",
                "Seleccione las Variables",
                choices = c("Carga viral plasmatica", "Celulas CD4", "Celulas CD8")
              ),
              checkboxInput("REML", "Restringida", TRUE)
            ),
            mainPanel(
              h4("Ajuste de Modelos por Maxima Verosimilitud"),
              verbatimTextOutput("sum")
              #tableOutput("ajuste")
            )
          )
        } else{
          h5("Los nombres de las variables no coinciden")
        }
        
      }
    }
    
  })
  
  #' Muestra la salida del formula del modelo lineal mixto
  output$sum <- renderPrint({
    LOGCVP <- log(dato()$CVP)
    variable <-  selectajusteInput()
    M0 <-
      lmer(LOGCVP ~ 1 + Fecha +  variable + (1 |
                                               Id),
           data = dato(),
           REML = input$REML)
    summary(M0)
  })
  
  #-------------------------------Validacion de los datos
  #' Muestra los residuos encontrados en la formaula del modelo lineal mixto
  
  output$validacion <- renderUI({
    if (is.null(dato())) {
      h5("No hay ninguna informacion cargada")
    }
    else{
      if (isFALSE(extension())) {
        h5("La extension no es la recomendada")
      } else{
        if (buscarCabeceras(dato()) == TRUE) {
          sidebarLayout(sidebarPanel(
            selectInput(
              "DatosV",
              "Seleccione las variables:",
              choices = c("Carga viral plasmatica", "Celulas CD4", "Celulas CD8")
            )
          ),
          mainPanel(plotOutput("valido")))
        } else{
          h5("Los nombres de las variables no coinciden")
        }
        
      }
      
    }
  })
  #' Graficacion de los residuos del modelo lineal mixto
  output$valido <- renderPlot({
    variable <- datosVInput()
    M1RML <-
      lmer(variable ~ 1 + Fecha  + Edad  + (Fecha |
                                              Id),
           data = dato(),
           REML = FALSE)
    qqmath (M1RML, id = 0.05)
  })
  
  
  #-----------------------------------------------Reportes
  #' Genera el reporte con los datos
  output$reporte <- renderUI({
    if (is.null(dato())) {
      h5("No hay ninguna informacion cargada")
    } else{
      if (isFALSE(extension())) {
        h5("La extension no es la recomendada")
      } else{
        if (buscarCabeceras(dato()) == TRUE) {
          sidebarLayout(sidebarPanel(
            downloadButton('downloadReport', label = "Descargar Archivo")
          ),
          mainPanel())
        } else{
          h5("Los nombres de las variables no coinciden")
        }
        
      }
      
    }
  })
  
  output$downloadReport <- downloadHandler(
    filename(),
    content = function(file) {
      src <- normalizePath('reporte.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'reporte.Rmd', overwrite = TRUE)
      param <-
        list(
          reml = input$REML,
          ajuste = selectajusteInput(),
          carga = selectcargaInput(),
          validacion = datosVInput(),
          periodo = entradasInput()
        )
      
      library(rmarkdown)
      out <-
        render('reporte.Rmd',
               params = param,
               word_document(),
               envir = new.env())
      file.rename(out, file)
      
    }
    
  )
  
}
