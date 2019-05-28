library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(ggplot2)
library(markdown)
#mapa
library(leaflet)
library(geoR)
library(sp)
library(rgdal)


theme_set(theme_bw())

#########################################
# Construccion de la UI                 #
#########################################
sidebar <- dashboardSidebar(
    #comenzamos con el menu
    sidebarMenu(
        menuItem("INPC Nacional", icon = icon("chart-line"), tabName = "INPCNac"),
        menuItem("INPC Regional", icon = icon("chart-line"), tabName = "INPCReg"),
        menuItem("Tipo de cambio", icon = icon("comments-dollar"), tabName = "Cambio")
    )
)

#cramos varias tabs
body <- dashboardBody(
  tabItems(
    # primer tab del dashboard
    tabItem(tabName = "INPCNac",
    # primer cuadro info del experto
    fluidRow(column( width=2), column( width=8,
        boxPlus(title = htmlOutput("OpinionOk"),  closable = TRUE, width = NULL, status = "success",  solidHeader = TRUE,
            collapsible = TRUE, enable_dropdown = TRUE, dropdown_icon = "external-link-square-alt",
            dropdown_menu = dropdownItemList(
                            dropdownItem(url = "https://www.cimat.mx/es/Monterrey", name = "Link al mensaje") ),
            p("Consulte el mensaje del experto") ) ), column( width = 2)) ,
#    fluidRow(column( width=2),column( width=8,
 #       boxPlus(title = htmlOutput("OpinionSemi"), closable = TRUE, width = NULL, status = "warning",  solidHeader = TRUE,
  #              collapsible = TRUE, enable_dropdown = TRUE, dropdown_icon = "external-link-square-alt",
   #             dropdown_menu = dropdownItemList(
    #                dropdownItem(url = "https://www.cimat.mx/es/Monterrey", name = "Link al mensaje") ),
     #           p("Consulte el mensaje del experto") ) ), column( width=2) ),
#    fluidRow( column( width=2),column( width=8,
 #       boxPlus(title = htmlOutput("OpinionNoOk"),  closable = TRUE, width = NULL, status = "danger",  solidHeader = TRUE,
  #              collapsible = TRUE, enable_dropdown = TRUE, dropdown_icon = "external-link-square-alt",
   #             dropdown_menu = dropdownItemList(
    #                dropdownItem(url = "https://www.cimat.mx/es/Monterrey", name = "Link al mensaje") ),
     #           p("Consulte el mensaje del experto") ) ), column( width=2) ) ,
    #segundo cuadro el pronostico principal
    fluidRow(
        tabBox(side = "left",  title = "INPC Nacional", id = "tabNac", height = "450px", width = 12,
        # primer subtab del primer cuadro
        tabPanel("Pronóstico", p(), htmlOutput("NacHTML"), p(), plotlyOutput("Nacional")),
        # segunda subtab del primer cuadro
        tabPanel("Tabla",  DT::dataTableOutput("tablaNac"), downloadButton("downloadNac", "Descargar pronosticos")))) ,
        # control de los graficos
    fluidRow(
        box(side='right', title='Opciones del pronóstico', id='controlnac', height='150px', width = 12,
             p('Con los botones puede verificar la calidad del pronóstico mes a mes desde enero de 2015. Elige el mes y año de inicio del pronóstico, asi como el horizonte a pronosticar'),
        column(width=2,
                    pickerInput(inputId = "inpcnacinitmes", label = "Mes inicio",
                                 choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                                 options = list(  style = "btn-warning"))),
       column(width=2,
                    pickerInput( inputId = "inpcnacinitanio", label = "Año inicio", choices = c("2017", "2018"),
                                 options = list(style = "btn-warning"))) ,
       column(width=2,
        pickerInput(inputId = "inpcnacmes", label = "Meses a pronosticar",
                                choices = c("1", "3","6"),
                                options = list(  style = "btn-warning"))),
       column(width=2,
                    pickerInput(inputId = "inpcnacHist", label = "Histórico de la serie",
                                choices = c('No', 'Sí'),
                                options = list(  style = "btn-warning"))      )))   ,
        # tercer cuadro (pronosticos con toda la info)
        fluidRow(column(width=12, htmlOutput('PronosticosinpcNac'))),
        # pronostico a un mes
        fluidRow(column(width = 12,
        boxPlus(title='', closable = TRUE, status = "info", width=4,
                htmlOutput("nacHTML1mes"), plotlyOutput("Nacional1mes")),
        # pronostico tres meses
        boxPlus(title='', closable = TRUE, status = "info", width=4,
                htmlOutput("nacHTML3mes"), plotlyOutput("Nacional3mes")),
        # pronostico seis meses
        boxPlus(title='', closable = TRUE, status = "info", width=4,
                htmlOutput("nacHTML6mes") ,     plotlyOutput("Nacional6mes"))) ) ) ,

    # segundo tab  seccion REGIONAL
    tabItem(tabName = "INPCReg",
    # primer cuadro info del experto
    fluidRow(column( width=2), column( width=8,
            boxPlus(title = htmlOutput("opinionOkReg"),  closable = TRUE, width = NULL, status = "success",  solidHeader = TRUE,
                    collapsible = TRUE, enable_dropdown = TRUE, dropdown_icon = "external-link-square-alt",
                    dropdown_menu = dropdownItemList(
                        dropdownItem(url = "https://www.cimat.mx/es/Monterrey", name = "Link al mensaje") ),
                     p("Consulte el mensaje del experto") ) ), column( width = 2)) ,
  #  fluidRow(column( width=2),column( width=8,
   #         boxPlus(title = htmlOutput("OpinionSemiReg"), closable = TRUE, width = NULL, status = "warning",  solidHeader = TRUE,
    #                collapsible = TRUE, enable_dropdown = TRUE, dropdown_icon = "external-link-square-alt",
     #               dropdown_menu = dropdownItemList(
      #                   dropdownItem(url = "https://www.cimat.mx/es/Monterrey", name = "Link al mensaje") ),
       #                  p("Consulte el mensaje del experto") ) ), column( width=2) ),
#    fluidRow( column( width=2),column( width=8,
 #           boxPlus(title = htmlOutput("opinionNoOkReg"),  closable = TRUE, width = NULL, status = "danger",  solidHeader = TRUE,
  #                  collapsible = TRUE, enable_dropdown = TRUE, dropdown_icon = "external-link-square-alt",
   #                dropdown_menu = dropdownItemList(
    #                    dropdownItem(url = "https://www.cimat.mx/es/Monterrey", name = "Link al mensaje") ),
     #                   p("Consulte el mensaje del experto") ) ), column( width=2) ) ,
    fluidRow(
       tabBox(side = "left",  title = "INPC Regional", id = "tabReg", height = "450px", width = 12,
        # primer subtab del primer cuadro
        tabPanel("Prónostico", p(),
         htmlOutput("RegHTML"), p(),  plotlyOutput("Regional")),
        # segunda subtab del primer cuadro
        tabPanel("Tabla",  DT::dataTableOutput("tablaReg"), downloadButton("downloadReg", "Descargar pronosticos")),
        # tercera subtab del primer cuadro
        tabPanel('Mapa Regional',leafletOutput("mapaReg") )  ) ) , # leafletOutput("mapaReg")
     fluidRow(
         # control de los graficos
         box(side='right', title='Opciones del pronóstico', id='controlreg', height='175px', width = 12,
         p('Con los botones puede verificar la calidad del pronóstico mes a mes desde enero de 2015. Elige el mes y año de inicio del pronóstico, así como el horizonte a pronosticar y la región'),
         column(width=2,
                pickerInput(inputId = "Reg", label = "Región",
                            choices = c("Frontera.norte", "Noroeste", "Noreste", "Centro.norte", "Centro.sur", "Sur", "Mexico"),
                            options = list(  style = "btn-primary"))),
         column(width=2,
                pickerInput(inputId = "inpcReginitmes", label = "Mes inicio",
                    choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                    options = list(  style = "btn-warning"))),
               column(width=2,
                    pickerInput( inputId = "inpcReginitanio", label = "Año inicio", choices = c("2017", "2018"),
                    options = list(style = "btn-warning")) ),
         column(width=2,
                    pickerInput(inputId = "inpcRegmes", label = "Meses a pronosticar",
                    choices = c("1", "3","6"),
                    options = list(  style = "btn-warning"))),
         column(width=2,
                    pickerInput(inputId = "inpcRegHist", label = "Histórico de la serie",
                                choices = c('No', 'Sí'),
                                options = list(  style = "btn-warning"))))  )  ,
    # tercer cuadro (pronosticos con toda la info)
    fluidRow( column(width=12, htmlOutput('PronosticosinpcReg'))),
    # pronostico a un mes
    fluidRow(column(width = 12,
      boxPlus(title='', closable = TRUE, status = "info", width=4,
      # tercer cuadro (pronostico regional un mes con toda la info)
            htmlOutput("RegHTML1mes"), plotlyOutput("Regional1mes")),
      boxPlus(title='', closable = TRUE, status = "info", width=4,
      # cuarto cuadro (pronostico tres meses con toda la info)
            htmlOutput("RegHTML3mes"), plotlyOutput("Regional3mes")),
      boxPlus(title='', closable = TRUE, status = "info", width=4,
      # quinto cuadro (pronostico seis meses con toda la info)
            htmlOutput("RegHTML6mes") ,     plotlyOutput("Regional6mes"))) ) ),


    # tercer tab de tipo de cambio
    tabItem(tabName = "Cambio",
       # primer cuadro info del experto
       fluidRow(column( width=2), column( width=8,
         boxPlus(title = htmlOutput("opinionOkCambio"),  closable = TRUE, width = NULL, status = "success",  solidHeader = TRUE,
                 collapsible = TRUE, enable_dropdown = TRUE, dropdown_icon = "external-link-square-alt",
                dropdown_menu = dropdownItemList(dropdownItem(url = "https://www.cimat.mx/es/Monterrey", name = "Link al mensaje") ),
                                 p("Consulte el mensaje del experto") ) ), column( width = 2)) ,
    #    fluidRow(column( width=2),column( width=8,
     #     boxPlus(title = htmlOutput("OpinionSemiCambio"), closable = TRUE, width = NULL, status = "warning",  solidHeader = TRUE,
      #            collapsible = TRUE, enable_dropdown = TRUE, dropdown_icon = "external-link-square-alt",
       #           dropdown_menu = dropdownItemList( dropdownItem(url = "https://www.cimat.mx/es/Monterrey", name = "Link al mensaje") ),
#                p("Consulte el mensaje del experto") ) ), column( width=2) ),
 #       fluidRow( column( width=2),column( width=8,
  #        boxPlus(title = htmlOutput("opinionNoOkCambio"),  closable = TRUE, width = NULL, status = "danger",  solidHeader = TRUE,
   #               collapsible = TRUE, enable_dropdown = TRUE, dropdown_icon = "external-link-square-alt",
    #              dropdown_menu = dropdownItemList(
     #               dropdownItem(url = "https://www.cimat.mx/es/Monterrey", name = "Link al mensaje") ),
      #              p("Consulte el mensaje del experto") ) ), column( width=2) ) ,
       #segundo cuadro el pronostico principal
        fluidRow(
          tabBox(side = "left",  title = "INPC Regional", id = "tabReg", height = "450px", width = 12,
         # primer subtab del primer cuadro
          tabPanel("Pronóstico", p(),  htmlOutput("CambioHTML"), p(), plotlyOutput("Cambioplot")),
         # segunda subtab del primer cuadro
          tabPanel("Tabla",  DT::dataTableOutput("tablaCambio"),  downloadButton("downloadCambio", "Descargar pronosticos")))),
       fluidRow(
           # control de los graficos
           box(side='right', title='Opciones del pronóstico', id='controlCambio', height='150px', width = 12,
               p('Con los botones puede verificar la calidad del pronóstico mes a mes desde enero de 2015. Elige el mes y año de inicio del pronóstico, así como el horizonte a pronosticar'),
           column(width=2,
                     pickerInput(inputId = "Cambioinitmes", label = "Mes inicio",
                               choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                               options = list(  style = "btn-warning"))),
           column(width=2,
                     pickerInput( inputId = "Cambioinitanio", label = "Año inicio", choices = c("2017", "2018"),
                               options = list(style = "btn-warning"))),
            column(width=2,
                     pickerInput(inputId = "Cambiomes", label = "Meses a pronosticar",
                               choices = c("1", "3","6"), options = list(  style = "btn-warning"))),
            column(width=2,
                     pickerInput(inputId = "CambioHist", label = "Histórico de la serie",
                                 choices = c("No", "Sí"), options = list(  style = "btn-warning"))) )),
            # tercer cuadro (pronosticos con toda la info)
            fluidRow(column(width=11, htmlOutput('PronosticosCambio'))),
            fluidRow(column(width=12,
             boxPlus(title='', closable = TRUE, status = "info", width=4,
                     htmlOutput("CambioHTML1mes"), plotlyOutput("Cambio1mes")),
            # cuarto cuadro (pronostico tres meses con toda la info)
            boxPlus(title='', closable = TRUE, status = "info", width=4,
                     htmlOutput("CambioHTML3mes"), plotlyOutput("Cambio3mes")),
            # quinto cuadro (pronostico seis meses con toda la info)
            boxPlus(title='', closable = TRUE, status = "info", width=4,
                htmlOutput("CambioHTML6mes") ,     plotlyOutput("Cambio6mes"))) ) )

    )
)












ui = dashboardPage(skin = "red",
                   dashboardHeader(title = "CIMAT  Observatorio Econometrico"),
                   sidebar, body)


