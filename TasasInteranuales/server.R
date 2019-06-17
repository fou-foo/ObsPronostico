##########################################
# Proyecto: Dashboard de pronosticos del laboratorio econometrico
# J. Antonio Garcia: jose.ramirez@cimat.mx
# CODIGO QUE DEFINE LA LOGICA DE LA APP para descarga de tasas interanuales
##########################################
# Dependencias
{
    library(shiny) # probee la infraestructura basica para apps web
    library(shinyjs) # incorpora elementos de javascript a R
    library(shinyWidgets) # se requiere para hacer mas monos los widgets del package 'shiny'
    library(shinydashboard) # anade los tabs de la derecha y el header
    library(shinydashboardPlus) # permiter la construccion de los cuadro que se cierran y los botones de colores
    library(plotly) # despliega las graficas con metainformacion
    library(ggplot2) # construccion de graficas planas que se converten a 'plotly'
    library(markdown) # Por si alguna vez la nota metodologica se hace publica en otra app
    # dependencias del mapa
    library(lubridate)
    library(leaflet) # permite usar el mapa
    library(geoR)  # necesaria para manejar mapas
    library(sp)  # clase principal de R para mapas
    library(rgdal) # otra clase principal de R para mapas
    library(DT) # permite presentar los data.frame's en tablas bonitas
}
#############################################
# Codificacion de la logia de la aplicacion #
#############################################

# Todo el codigo se compone de un solo CLOSURE
server = function(input, output) {
    # Leemos los archivos que siempre se utilizan UNA SOLA VEZ para mejorar los tiempos de respuesta
    # Construccion del mapa para el promedio de la tasa interanual para el INPC por regiones
    mex <- readOGR(dsn="MapaSHP/") # se recomienda la leer la carpeta no solo el archivo .shp los demas archivos contiene (en ocaciones) informacion necesaria
    mex@data$Region <- ''
    mex@data$INPC <- -1.
    mex@data$ESTADO <- as.character(mex@data$ESTADO)
    # cambio de acentos que se pierden por el enconding
    mex@data$ESTADO[7] <- 'Queretaro'
    mex@data$ESTADO[9] <- 'Michoacan'
    mex@data$ESTADO[10] <- 'CDMX'
    mex@data$ESTADO[14] <- 'Yucatan'
    mex@data$ESTADO[29] <- 'San Luis Potosi'
    mex@data$ESTADO[30] <- 'Nuevo Leon'
    # cambio de id mal en el mapa y nombres
    mex@data$ESTADO[mex@data$ID==28] <- 'Tamaulipas norte'
    mex@data$ID[mex@data$ID==28] <- 37
    mex@data$ESTADO[as.character(mex@data$CODIGO)=='MX37'] <- 'Tamaulipas'
    mex@data$ID[as.character(mex@data$CODIGO)=='MX37'] <- 28
    mex@data$ESTADO[ mex@data$ID == 9   ] <-  'CDMX'
    mex@data$Region [ mex@data$ID == 9   ] <-  'CDMX'

    # Closure que regresa el mapa pintado
    output$mapaReg <- renderLeaflet({
        # pequenio casteo de 'int' a 'character' para tener el mes que se requiere visualizar
        if(nchar(as.character(input$inpcReginitmes)) == 1)
        {
            mes.casteo <- paste0('0',input$inpcReginitmes)
        } else{
            mes.casteo <- input$inpcReginitmes
        }
        # variable que guarda el nomnre del archivo que determinada los colores del mapa
        string <-paste0('Mapa/PronosticoINPC_Tasa_Interanual_anio',
                        input$inpcReginitanio, 'mes_',input$inpcReginitanio, '-',
                        mes.casteo,"horizonte_",
                        input$inpcRegmes, '.csv')
        pronosticos_regional <- read.csv(string, row.names = 1)
        valores <- sapply(pronosticos_regional, mean) # promediamos la tasa interanual
        # vamos por los estados que forman la region economica centro norte
        # los id's vienen por defecto en el mapa que consiguio Andres en http://tapiquen-sig.jimdo.com.
        id.centro.norte <- c(1, 8, 11, 14, 16, 22, 24 ) #1-aguascalientes, 8-Colima, 11-Guanajuato, 14-Jalisco, 22-Qro, 24-SLP
        mex@data$INPC[match(id.centro.norte, mex@data$ID)  ] <<- valores['Centro.norte']
        mex@data$Region [match(id.centro.norte, mex@data$ID)  ] <<- 'Centro norte'
        # estados de la frontra norte
        id.Frontera.norte <- c(2, 3, 33, 34, 35, 36, 37) #2-Baja-norte, 3-BC sur, 33-Sonora-norte, 34-Chihuahuanorte, 35-Coahuila-norte, 36-NL-norte, 37-Tamaulipas-norte
        mex@data$INPC[match(id.Frontera.norte, mex@data$ID)  ] <<- valores['Frontera.norte']
        mex@data$Region[match(id.Frontera.norte, mex@data$ID)  ] <<- 'Frontera norte'
        # area noroeste
        id.Noroeste <- c(18, 25, 26) # 18-Nayarit, 25-Sinaloa, 26-Sonora
        mex@data$INPC[match(id.Noroeste, mex@data$ID)  ] <<- valores['Noroeste']
        mex@data$Region [match(id.Noroeste, mex@data$ID)  ] <<- 'Noroeste'
        #noreste
        id.Noreste <- c(6, 7, 10, 19, 28, 32) # 6-Chihuahua,7-Coahuila, 10-Durango, 19-NL, 28-Tamaulipas, 32-Zacatecas
        mex@data$INPC[match(id.Noreste, mex@data$ID)  ] <<- valores['Noreste']
        mex@data$Region[match(id.Noreste, mex@data$ID)  ] <<- 'Noreste'
        # sur
        id.Sur <- c(4, 5, 20, 23, 27, 31) #4-Campeche, 5-Chiapas, 20-Oaxaca, 23-Quintana Roo, 27-Tabasco, 31-Yucatan
        mex@data$INPC[match(id.Sur, mex@data$ID)  ] <<- valores['Sur']
        mex@data$Region[match(id.Sur, mex@data$ID)  ] <<- 'Sur'
        mex@data$INPC[ mex@data$ID == 9  ] <<- valores['Mexico']
        # centro sur
        id.Centro.sur <- c(12, 13, 15, 17, 21, 29, 30 ) #12-guerrero, 13-Hidalgo, 15-Edo.Mexico, 17-morelos, 21-Puebla, 29-Tlaxcala, 30-Veracruz
        mex@data$INPC[match(id.Centro.sur, mex@data$ID)  ] <<- valores['Centro.sur']
        mex@data$Region[match(id.Centro.sur, mex@data$ID)  ] <<- 'Centro sur'
        # construccion del mapa
        leaflet(mex) %>%
            addPolygons(color = "#444444", weight = 1, smoothFactor = 0.8,
                        opacity = 1.0, fillOpacity = 0.5,
                        fillColor = ~colorQuantile("YlOrRd", INPC)(INPC),
                        highlightOptions = highlightOptions(color = "black", weight = 4,
                                                            bringToFront = FALSE),
                        label = ~paste0(ESTADO, ' INPC: ', round(INPC, 2), ' Region: ', Region)) %>%
            addProviderTiles("Esri.WorldTerrain")
})

####################################################################
# EVENTOS ##########################################################
#eventos (informacion extra para funcionalidad de los controles)


        # Closure que lee de disco la grafica con o sin historico para el INPC en cualquier region
   
  output$RegHTML <- renderText({
      paste0('<h style="color:#702039;"><strong> Pronóstico de INPC a ', input$inpcRegmes, " meses a partir del mes ", input$inpcReginitmes, " del año ", input$inpRecinitanio , ' en la región: ', input$Reg, " </strong></h>")
    })
    output$CambioHTML <- renderText({
      paste0('<h style="color:#702039;"><strong> Pronóstico de tipo de cambio a ', input$Cambiomes, " meses a partir del mes ", input$Cambioinitmes, " del año ", input$Cambioinitanio ," </strong></h>")
    })


    # funcion para visualizar la tabla del INPC por region
    output$tablaReg <-  DT::renderDataTable({
      if(nchar(as.character(input$inpcReginitmes)) == 1)
      {
        mes.casteo <- paste0('0',input$inpcReginitmes)
      } else{
        mes.casteo <- input$inpcReginitmes
      }
      # variable que guarda el nomnre del archivo que determinada los colores del mapa
      string <-paste0('Mapa/PronosticoINPC_Tasa_Interanual_anio',
                      input$inpcReginitanio, 'mes_',input$inpcReginitanio, '-',
                      mes.casteo,"horizonte_",
                      input$inpcRegmes, '.csv')
      pronosticos_regional <- read.csv(string, row.names = 1)
      
      DT::datatable(pronosticos_regional)
    })

    # Lectura y despliege de las graficas de los tres tabs
    # La primer grafica del ultimo reglon del tab 1

      # titulo del pronostico actual a un mes

    output$PronosticosinpcReg <- renderText({
        paste0('<h1 style="color:#702039;"><strong> Pronósticos de INPC regional (corte marzo de 2019) </strong></h1>')
    })


 


    
    
#FUNCIONES PARA DESCARGAR LAS tablas que se visualizan a formato,csv
    output$downloadReg <- downloadHandler(
          filename = 'Tasas', 
          content = function(file) {
            if(nchar(as.character(input$inpcReginitmes)) == 1)
            {
              mes.casteo <- paste0('0',input$inpcReginitmes)
            } else{
              mes.casteo <- input$inpcReginitmes
            }
            # variable que guarda el nomnre del archivo que determinada los colores del mapa
            string <-paste0('Mapa/PronosticoINPC_Tasa_Interanual_anio',
                            input$inpcReginitanio, 'mes_',input$inpcReginitanio, '-',
                            mes.casteo,"horizonte_",
                            input$inpcRegmes, '.csv')
            string
            file.copy(string, file)
          }
          
    )

    



}
