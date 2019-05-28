library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(ggplot2)
library(markdown)
library(DT)
library(lubridate)
#mapa#
library(leaflet)
library(geoR)
library(sp)
library(rgdal)


server = function(input, output) {
    # mapa regional
    mex <- readOGR(dsn="MapaSHP")
    mex@data$Nombre <- c("Frontera.norte", "Noroeste", "Noreste", "Centro.norte",
                         "Centro.sur", "Sur", "Mexico")


    output$mapaReg <- renderLeaflet({
        epsg2163 <- leafletCRS(
            crsClass = "L.Proj.CRS.TMS",
            code = "WGS84",
            proj4def = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
            resolutions=0:10)
        # pequenio casteo
      # input <- list(2017, 12, 6)
      # names(input) <- c('inpcReginitanio', 'inpcReginitmes', 'inpcRegmes4')
        if(nchar(as.character(input$inpcReginitmes)) == 1)
        {
          mes.casteo <- paste0('0',input$inpcReginitmes)
        } else{
          mes.casteo <- input$inpcReginitmes
        }


        string <-paste0('Mapa/PronosticoINPC_Tasa_Interanual_anio',
                        input$inpcReginitanio, 'mes_',input$inpcReginitanio, '-',
                        mes.casteo,"horizonte_",
                        input$inpcRegmes, '.csv')
        pronosticos_regional <- read.csv(string)
        mex@data$INPC <<- sapply(pronosticos_regional[, mex@data$Nombre], mean)
        leaflet(mex,options = leafletOptions(crs = epsg2163)) %>%
            setView(-97.9190868+750, 17.3610943-450, 3, options = leafletOptions(crs = epsg2163)) %>%
            addPolygons(color = "#444444", weight = 1, smoothFactor = 0.8,
                        opacity = 1.0, fillOpacity = 0.5,
                        fillColor = ~colorQuantile("YlOrRd", INPC)(INPC),
                        highlightOptions = highlightOptions(color = "black", weight = 4,
                                                            bringToFront = FALSE),
                        label = ~paste0(Nombre, ' INPC: ', round(INPC, 2)))%>%
            removeControl("zonesLegend")



    })
    #eventos (informacion extra para funcionalidad de los controles)
    output$Nacional <- renderPlotly({
        if(input$inpcnacHist == 'No')
        {
            load(file=paste0('BinariosMax/GGplotpronostico_fecha_corteMes_ ',input$inpcnacinitmes," anio_", input$inpcnacinitanio,
                             "numero_de_meses_pronostico_",input$inpcnacmes," region Nacional.Rdata"))
            p <- p.hist + xlim(c(ymd('2016-01-01'), ymd('2019-06-01')))+ ylim(c(85,110))
            p <- ggplotly(p, tooltip = c('x','y'), dynamicTicks = TRUE )
            p <- p %>% config(collaborate=FALSE , displaylogo = FALSE) %>%
                layout(legend = list(orientation = 'h'))
            return(p)
        } else{
            load(file=paste0('BinariosMax/pronostico_fecha_corteMes_ ',input$inpcnacinitmes," anio_", input$inpcnacinitanio,
                             "numero_de_meses_pronostico_",input$inpcnacmes," region Nacional.Rdata"))

            p.hist
        }

       })

    output$Regional <- renderPlotly({
        if(input$inpcRegHist=='No')
        {
            load(file=paste0('BinariosMax/GGplotpronostico_fecha_corteMes_ ',input$inpcReginitmes," anio_", input$inpcReginitanio,
                             "numero_de_meses_pronostico_",input$inpcRegmes," region ", input$Reg, ".Rdata"))
            p <- p.hist + xlim(c(ymd('2016-01-01'), ymd('2019-06-01')))+ ylim(c(85,110))
            p <- ggplotly(p, tooltip = c('x','y'), dynamicTicks = TRUE )
            p <- p %>% config(collaborate=FALSE , displaylogo = FALSE) %>%
                layout(legend = list(orientation = 'h'))
            return(p)
        } else{
            load(file=paste0('BinariosMax/pronostico_fecha_corteMes_ ',input$inpcReginitmes," anio_", input$inpcReginitanio,
                       "numero_de_meses_pronostico_",input$inpcRegmes," region ", input$Reg, ".Rdata"))
            p.hist
        }
    })

    output$Cambioplot <- renderPlotly({
        if(input$CambioHist == 'No')
        {
            load(file=paste0('BinariosMax/GGplotTipoDeCambiopronostico_fecha_corteMes_ ',input$Cambioinitmes," anio_", input$Cambioinitanio,
                             "numero_de_meses_pronostico_", input$Cambiomes,".Rdata"))
            p <- p.hist + xlim(c(ymd('2016-01-01'), ymd('2019-06-01')))+ ylim(c(15,25))
            p <- ggplotly(p, tooltip = c('x','y'), dynamicTicks = TRUE )
            p <- p %>% config(collaborate=FALSE , displaylogo = FALSE) %>%
                layout(legend = list(orientation = 'h'))
            return(p)
        } else {
            load(file=paste0('BinariosMax/TipoDeCambiopronostico_fecha_corteMes_ ',input$Cambioinitmes," anio_", input$Cambioinitanio,
                       "numero_de_meses_pronostico_", input$Cambiomes,".Rdata"))
            p.hist
        }
    })

    # output$testfoo <- renderPrint({
    #        if(nchar(as.character(input$inpcReginitmes)) == 1)
    #            mes.casteo <- paste0('0',input$inpcReginitmes)
    #
    # string <-paste0('Mapa/PronosticoINPC_Tasa_Interanual_anio',
    #                                      input$inpcReginitanio, 'mes_',input$inpcReginitanio, '-',
    #                                      mes.casteo,"horizonte_",
    #                                      input$inpcRegmes, '.csv')#
    #   string})

    output$NacHTML <- renderText({
      paste0('<h style="color:#702039;"><strong> Pronóstico de INPC a ', input$inpcnacmes, " meses a partir del mes ", input$inpcnacinitmes, " del año ", input$inpcnacinitanio , "</strong></h>")
    })
    output$RegHTML <- renderText({
      paste0('<h style="color:#702039;"><strong> Pronóstico de INPC a ', input$inpcRegmes, " meses a partir del mes ", input$inpcReginitmes, " del año ", input$inpRecinitanio , ' en la región: ', input$Reg, " </strong></h>")
    })
    output$CambioHTML <- renderText({
      paste0('<h style="color:#702039;"><strong> Pronóstico de tipo de cambio a ', input$Cambiomes, " meses a partir del mes ", input$Cambioinitmes, " del año ", input$Cambioinitanio ," </strong></h>")
    })


    output$tablaNac <-  DT::renderDataTable({
      load(file=paste0("BinariosMax/pronostico_intervalosConfi_mes_", input$inpcnacinitmes," anio_",input$inpcnacinitanio,"numero_de_meses_pronostico_", input$inpcnacmes," region Nacional.Rdata"))
      #drop = FALSE
      DT::datatable(forecast.mean)
    })
    output$tablaReg <-  DT::renderDataTable({
      load(file=paste0("BinariosMax/pronostico_intervalosConfi_mes_", input$inpcReginitmes," anio_",input$inpcReginitanio,"numero_de_meses_pronostico_", input$inpcRegmes," region ", input$Reg,  ".Rdata"))
      #drop = FALSE
      DT::datatable(forecast.mean)
    })
    output$tablaCambio <-  DT::renderDataTable({
      load(file=paste0("BinariosMax/TipoDeCambiopronostico_intervalosConfi_mes_", input$Cambioinitmes," anio_", input$Cambioinitanio, "numero_de_meses_pronostico_", input$Cambiomes,".Rdata"))
      #drop = FALSE
      DT::datatable(forecast.mean)
    })

    # falta actualizar el mapita


    # cuadros inferiores del primer tab INPC nacional
        #para un mes


    output$Nacional1mes <- renderPlotly({
      load(file='BinariosMax/GGplotpronostico_fecha_corteMes_ 6 anio_2018numero_de_meses_pronostico_1 region Nacional.Rdata')
                       p <- p.hist + xlim(c(ymd('2016-01-01'), ymd('2019-06-01'))) + ylim(c(85,110))
      p <- ggplotly(p, tooltip = c('x','y'), dynamicTicks = TRUE )
      p <- p %>% config(collaborate=FALSE , displaylogo = FALSE) %>%
        layout(legend = list(orientation = 'h'))
      p
      })
    output$Regional1mes <- renderPlotly({
      load(paste0(file='BinariosMax/GGplotpronostico_fecha_corteMes_ 6 anio_2018numero_de_meses_pronostico_1 region ',input$Reg,'.Rdata'))
      p <- p.hist + xlim(c(ymd('2016-01-01'), ymd('2019-06-01')))  + ylim(c(85,110))
      p <- ggplotly(p, tooltip = c('x','y'), dynamicTicks = TRUE )
      p <- p %>% config(collaborate=FALSE , displaylogo = FALSE) %>%
        layout(legend = list(orientation = 'h'))
      p
    })
    output$Cambio1mes <- renderPlotly({
      load(paste0(file='BinariosMax/GGplotTipoDeCambiopronostico_fecha_corteMes_ 6 anio_2018numero_de_meses_pronostico_1.Rdata'))
      p <- p.hist + xlim(c(ymd('2016-01-01'), ymd('2019-06-01'))) + ylim(c(17,28))
      p <- ggplotly(p, tooltip = c('x','y'), dynamicTicks = TRUE )
      p <- p %>% config(collaborate=FALSE , displaylogo = FALSE) %>%
        layout(legend = list(orientation = 'h'))
      p
      
    })

    # titulo del pronostico actual a un mes
    output$PronosticosinpcNac <- renderText({
        paste0('<h1 style="color:#702039;"><strong> Pronósticos de INPC nacional (corte diciembre de 2018) </strong></h1>')
    })
    output$PronosticosinpcReg <- renderText({
        paste0('<h1 style="color:#702039;"><strong> Pronósticos de INPC regional (corte diciembre de 2018) </strong></h1>')
    })
    output$PronosticosCambio <- renderText({
        paste0('<h1 style="color:#702039;"><strong> Pronósticos de Tipo de cambio (corte diciembre de 2018) </strong></h1>')
    })




    output$nacHTML1mes <- renderText({
      paste0('<h style="color:#702039;"><strong> A 1 mes </strong></h>')
    })
    output$RegHTML1mes <- renderText({
      paste0('<h style="color:#702039;"><strong> A 1 mes para la región:  ',input$Reg,' </strong></h>')
    })
    output$CambioHTML1mes <- renderText({
      paste0('<h style="color:#702039;"><strong> A 1 mes </strong></h>')
    })

    #para tres meses
    output$Nacional3mes <- renderPlotly({
      load(file='BinariosMax/GGplotpronostico_fecha_corteMes_ 6 anio_2018numero_de_meses_pronostico_3 region Nacional.Rdata')
      p <- p.hist + xlim(c(ymd('2016-01-01'), ymd('2019-06-01')))  + ylim(c(85,110))
      p <- ggplotly(p, tooltip = c('x','y'), dynamicTicks = TRUE )
      p <- p %>% config(collaborate=FALSE , displaylogo = FALSE) %>%
        layout(legend = list(orientation = 'h'))
      p
      
    })
    output$Regional3mes <- renderPlotly({
      load(file=paste0('BinariosMax/GGplotpronostico_fecha_corteMes_ 6 anio_2018numero_de_meses_pronostico_6 region ',input$Reg,'.Rdata'))
      p <- p.hist + xlim(c(ymd('2016-01-01'), ymd('2019-06-01')))  + ylim(c(85,110))
      p <- ggplotly(p, tooltip = c('x','y'), dynamicTicks = TRUE )
      p <- p %>% config(collaborate=FALSE , displaylogo = FALSE) %>%
        layout(legend = list(orientation = 'h'))
      p
      
    })
    output$Cambio3mes <- renderPlotly({
      load(file='BinariosMax/GGplotTipoDeCambiopronostico_fecha_corteMes_ 6 anio_2018numero_de_meses_pronostico_3.Rdata')
      p <- p.hist + xlim(c(ymd('2016-01-01'), ymd('2019-06-01'))) + ylim(c(17,28))
      p <- ggplotly(p, tooltip = c('x','y'), dynamicTicks = TRUE )
      p <- p %>% config(collaborate=FALSE , displaylogo = FALSE) %>%
        layout(legend = list(orientation = 'h'))
      p     
    })

    # titulo del pronostico actual a tres meses
    output$nacHTML3mes <- renderText({
      paste0('<h style="color:#702039;"><strong> A 3 meses  </strong></h>')
    })
    output$RegHTML3mes <- renderText({
      paste0('<h style="color:#702039;"><strong> A 3 meses región ',input$Reg,'  </strong></h>')
    })
    output$CambioHTML3mes <- renderText({
      paste0('<h style="color:#702039;"><strong> A 3 mes  </strong></h>')
    })
    #para seis meses
    output$Nacional6mes <- renderPlotly({
      load(file='BinariosMax/GGplotpronostico_fecha_corteMes_ 6 anio_2018numero_de_meses_pronostico_6 region Nacional.Rdata')
      p <- p.hist + xlim(c(ymd('2016-01-01'), ymd('2019-06-01')))  + ylim(c(85,110))
      p <- ggplotly(p, tooltip = c('x','y'), dynamicTicks = TRUE )
      p <- p %>% config(collaborate=FALSE , displaylogo = FALSE) %>%
        layout(legend = list(orientation = 'h'))
      p
      
    })
    output$Regional6mes <- renderPlotly({
      load(file=paste0('BinariosMax/GGplotpronostico_fecha_corteMes_ 6 anio_2018numero_de_meses_pronostico_6 region ',input$Reg,'.Rdata'))
      p <- p.hist + xlim(c(ymd('2016-01-01'), ymd('2019-06-01')))  + ylim(c(85,110))
      p <- ggplotly(p, tooltip = c('x','y'), dynamicTicks = TRUE )
      p <- p %>% config(collaborate=FALSE , displaylogo = FALSE) %>%
        layout(legend = list(orientation = 'h'))
      p
      
    })
    output$Cambio6mes <- renderPlotly({
      load(file='BinariosMax/GGplotTipoDeCambiopronostico_fecha_corteMes_ 6 anio_2018numero_de_meses_pronostico_6.Rdata')
      p <- p.hist + xlim(c(ymd('2016-01-01'), ymd('2019-06-01'))) + ylim(c(17,28))
      p <- ggplotly(p, tooltip = c('x','y'), dynamicTicks = TRUE )
      p <- p %>% config(collaborate=FALSE , displaylogo = FALSE) %>%
        layout(legend = list(orientation = 'h'))
      p     
    })
    # titulo del pronostico actual a seis meses
    output$nacHTML6mes <- renderText({
      paste0('<h style="color:#702039;"><strong> A 6 meses </strong></h>')
    })
    output$RegHTML6mes <- renderText({
      paste0('<h style="color:#702039;"><strong> A 6 meses región ',input$Reg, ' </strong></h>')
    })
    output$CambioHTML6mes <- renderText({
      paste0('<h style="color:#702039;"><strong> A 6 mes </strong></h>')
    })


    #### opiniones condicionales
    output$OpinionOk <- renderText({
      paste0('<h style="color:#702039;"><strong> Opinión OK del experto </strong></h>')
    })
    output$OpinionNoOk <- renderText({
      paste0('<h style="color:#702039;"><strong> Opinión No OK del experto </strong></h>')
    })
    output$OpinionSemi <- renderText({
        paste0('<h style="color:#702039;"><strong> Opinión media (ni bien, ni mal) del experto </strong></h>')
    })

    output$opinionOkReg <- renderText({
      paste0('<h style="color:#702039;"><strong> Opinión OK del experto </strong></h>')
    })
    output$OpinionSemiReg <- renderText({
        paste0('<h style="color:#702039;"><strong> Opinión media (ni bien ni mal) del experto </strong></h>')
    })
    output$opinionNoOkReg <- renderText({
      paste0('<h style="color:#702039;"><strong> Opinión No OK del experto </strong></h>')
    })
    output$OpinionSemiCambio <- renderText({
      paste0('<h style="color:#702039;"><strong> Opinión media (ni bien ni mal) del experto </strong></h>')
    })
    output$opinionOkCambio <- renderText({
        paste0('<h style="color:#702039;"><strong> Opinión OK del experto </strong></h>')
    })
    output$opinionNoOkCambio <- renderText({
      paste0('<h style="color:#702039;"><strong> Opinión No OK del experto </strong></h>')
    })


#descarga de archivos
    output$downloadNac <- downloadHandler(
        filename = function() {
            paste0("pronostico_mes_", input$inpcnacinitmes," anio_",input$inpcnacinitanio,"numero_de_meses_pronostico_", input$inpcnacmes," region Nacional.csv")
        },
        content = function(file) {
            load(file=paste0("BinariosMax/pronostico_intervalosConfi_mes_", input$inpcnacinitmes," anio_",input$inpcnacinitanio,"numero_de_meses_pronostico_", input$inpcnacmes," region Nacional.Rdata"))

            write.csv( forecast.mean, file, row.names = TRUE)
        }
    )

    output$downloadReg <- downloadHandler(
        filename = function() {
            paste0("pronostico_mes_", input$inpcReginitmes," anio_",input$inpcReginitanio,"numero_de_meses_pronostico_", input$inpcRegmes," region ", input$Reg,  ".csv")
        },
        content = function(file) {
            load(file=paste0("BinariosMax/pronostico_intervalosConfi_mes_", input$inpcReginitmes," anio_",input$inpcReginitanio,"numero_de_meses_pronostico_", input$inpcRegmes," region ", input$Reg,  ".Rdata"))
            write.csv( forecast.mean, file, row.names = TRUE)
        }
    )
    output$downloadCambio <- downloadHandler(
        filename = function() {
           paste0("TipoDeCambiopronostico_intervalosConfi_mes_", input$Cambioinitmes," anio_", input$Cambioinitanio, "numero_de_meses_pronostico_", input$Cambiomes,".csv")
        },
        content = function(file) {
            load(file=paste0("BinariosMax/TipoDeCambiopronostico_intervalosConfi_mes_", input$Cambioinitmes," anio_", input$Cambioinitanio, "numero_de_meses_pronostico_", input$Cambiomes,".Rdata"))
            write.csv( forecast.mean, file, row.names = TRUE)
        }
    )




}
