##########################################
# Proyecto: Dashboard de pronosticos del laboratorio econometrico
# J. Antonio Garcia: jose.ramirez@cimat.mx
# CODIGO PARA CONSTRUIR LA INTERFAZ GRAFICA
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
    library(leaflet) # permite usar el mapa
    library(geoR)  # necesaria para manejar mapas
    library(sp)  # clase principal de R para mapas
    library(rgdal) # otra clase principal de R para mapas
}
#########################################
# Construccion de la UI                 #
#########################################

# Esta seccion se encarga de contruir la parte derecha de la vista del dashboard
# donde aparecen los nombres en la seccion negra
sidebar <- dashboardSidebar(
    #comenzamos con el menu
    sidebarMenu(
        menuItem("INPC Nacional", icon = icon("chart-line"), tabName = "INPCNac"),
        menuItem("INPC Regional", icon = icon("chart-line"), tabName = "INPCReg"),
        menuItem("Tipo de cambio", icon = icon("comments-dollar"), tabName = "Cambio")
    )
)

# Construccion de cuerpo del dashbord, debe de hacerse
# tab por tab
body <- dashboardBody( #comienza la construccion del cuerpo
tabItems( #esta linea que contiene todos los taps
    tabItem(tabName = "INPCNac", # primer tab del dashboard IMPORTANTE: el nombre debe de coincidir con los de la linea 31
    # primer cuadro info del experto
    fluidRow( # se define un renglon
        column( width=2), # se deja un espacio entre el borde y la siguiente columna DENTRO DEL REGLON
        column( width=8, # dentro de esta columna se incluye el cuadro para redireccionar al boletin impreso
        boxPlus(title = htmlOutput("OpinionOk"),  closable = TRUE, width = NULL, status = "success",  solidHeader = TRUE,
            collapsible = TRUE, enable_dropdown = TRUE, dropdown_icon = "external-link-square-alt",
            dropdown_menu = dropdownItemList(
                            dropdownItem(url = "https://www.cimat.mx/es/Monterrey", name = "Ir al boletin") ),
            p("Descargue de boletin") ) ), column( width = 2)) ,
              #termina primer reglon del primer tab
    # comienza segundo reglon del primer tab, el que contiene el pronostico interactivo y la tabla
    fluidRow(
        tabBox(side = "left",  title = "INPC Nacional", id = "tabNac", height = "450px", width = 12, # lo objetos tab box permiten definir pestallas dentro de un cuadro
        # primer subtab del primer cuadro
        tabPanel("Pronóstico", p(), htmlOutput("NacHTML"), p(), plotlyOutput("Nacional")), #grafica del pronostico INPC nacional
        # segunda subtab del primer cuadro
        tabPanel("Tabla",  DT::dataTableOutput("tablaNac"), downloadButton("downloadNac", "Descargar pronosticos")))) , # tabla del INPC a nivel nacional
        # control de los graficos
    # termina segundo reglon del primer tab
    # inicia tercer reglon del primer tab contiene los controles de la grafica del reglon anterior
    fluidRow(
        # caja para acomodar los reglones
        box(side='right', title='Opciones del pronóstico', id='controlnac', height='150px', width = 12,
             p('Con los botones puede verificar la calidad del pronóstico mes a mes desde enero de 2015. Elige el mes y año de inicio del pronóstico, asi como el horizonte a pronosticar'),
        #comienzan los controles del primer tab
        column(width=2,
                    pickerInput(inputId = "inpcnacinitmes", label = "Mes inicio",
                                 choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                                 options = list(  style = "btn-warning"))),
       column(width=2,
                    pickerInput( inputId = "inpcnacinitanio", label = "Año inicio", choices = c("2017", "2018"),
                                 options = list(style = "btn-warning"))) ,
       column(width=2,
        pickerInput(inputId = "inpcnacmes", label = "Meses a pronosticar",
                                choices = c("1", "3" ),  #"6"), por recomendacion de la Dra. Graciela se omite mostar el calculo a 6 mese pero SI SE CALCULA Y SE ENCUENTRA EN LA CARPETA DE 'BinariosMAx'
                                options = list(  style = "btn-warning"))),
       column(width=2,
                    pickerInput(inputId = "inpcnacHist", label = "Histórico de la serie",
                                choices = c('No', 'Si'),
                                options = list(  style = "btn-warning"))      )))   ,
        #terminan controles y tercer reglon
        # inica ultimo reglon con los pronosticos mas recientes
        fluidRow(column(width=12, htmlOutput('PronosticosinpcNac'))),
        # pronostico mas reciente a un mes
        fluidRow(column(width = 12,
        boxPlus(title='', closable = TRUE, status = "info", width=6,
                htmlOutput("nacHTML1mes"), plotlyOutput("Nacional1mes")),
        # pronostico mas reciente tres meses
        boxPlus(title='', closable = TRUE, status = "info", width=6,
                htmlOutput("nacHTML3mes"), plotlyOutput("Nacional3mes")) #,
        # pronostico mas reciente seis meses
        #boxPlus(title='', closable = TRUE, status = "info", width=4,
         #       htmlOutput("nacHTML6mes") ,     plotlyOutput("Nacional6mes")))
        ) ) ) ,
    # termina primer tab
    # segundo tab  con los pronosticos de la inflacion a nivel regional y CDMX
    tabItem(tabName = "INPCReg", #IMPORTANTE QUE SE LLAME IGUAL QUE EN LA LINEA 32
    # primer reglon del segundo tab
    # contiene cuadro info del experto
    fluidRow(column( width=2), column( width=8,
            boxPlus(title = htmlOutput("opinionOkReg"),  closable = TRUE, width = NULL, status = "success",  solidHeader = TRUE,
                    collapsible = TRUE, enable_dropdown = TRUE, dropdown_icon = "external-link-square-alt",
                    dropdown_menu = dropdownItemList(
                        dropdownItem(url = "https://www.cimat.mx/es/Monterrey", name = "Ir al boletin") ),
                     p("Descargue el boletin") ) ), column( width = 2)) ,
    # comienza segundo reglon del segundo tab
    fluidRow(
        # caja que permite introducir varias pestanas en ella
       tabBox(side = "left",  title = "INPC Regional", id = "tabReg", height = "450px", width = 12,
        # primer subtab de la caja contiene el grafico del pronostico que se pide con los controles
        tabPanel("Prónostico", p(), htmlOutput("RegHTML"), p(),  plotlyOutput("Regional")),
        # segunda subtab contieje la tabla que se puede descargar (regional)
        tabPanel("Tabla",  DT::dataTableOutput("tablaReg"), downloadButton("downloadReg", "Descargar pronosticos")),
        # tercera subtab contiene el mapa
        tabPanel('Mapa Regional', p(), htmlOutput("TasaDeCambio"), p(), leafletOutput("mapaReg") )  ) ) ,
    # fin de segunda fila del segundo tab
    # comienza tercer fila del segundo tab contiene los controles de la visualizacion
     fluidRow(
         # control de los graficos
         box(side='right', title='Opciones del pronóstico', id='controlreg', height='175px', width = 12,
         p('Con los botones puede verificar la calidad del pronóstico mes a mes desde enero de 2015. Elige el mes y año de inicio del pronóstico, asi_ como el horizonte a pronosticar y la región'),
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
                    choices = c("1", "3"), #"6"), por recomendacion de la doctora se quita el pronostico a 6 meses pero si se calculo y se encuentra en la carpeta 'BinariosMax'
                    options = list(  style = "btn-warning"))),
         column(width=2,
                    pickerInput(inputId = "inpcRegHist", label = "Histórico de la serie",
                                choices = c('No', 'Si_'),
                                options = list(  style = "btn-warning"))))  )  ,
    # termina tercer fila
    # inicia ultima fila del segundo tab con los pronosticos mas recientes
    # tercer cuadro (pronosticos con toda la info)
    fluidRow( column(width=12, htmlOutput('PronosticosinpcReg'))),
    # pronostico a un mes
    fluidRow(column(width = 12,
      boxPlus(title='', closable = TRUE, status = "info", width=6,
      # tercer cuadro (pronostico regional un mes con toda la info)
            htmlOutput("RegHTML1mes"), plotlyOutput("Regional1mes")),
      boxPlus(title='', closable = TRUE, status = "info", width=6,
      # cuarto cuadro (pronostico tres meses con toda la info)
            htmlOutput("RegHTML3mes"), plotlyOutput("Regional3mes"))   )) ),
#      boxPlus(title='', closable = TRUE, status = "info", width=4,
      # quinto cuadro (pronostico seis meses con toda la info)
#            htmlOutput("RegHTML6mes") ,     plotlyOutput("Regional6mes"))) ) ),
    # termina tercer fila del segundo tab

    # comienza tercer tab TIPO DE CAMBIO
    tabItem(tabName = "Cambio", #MANTENER EL MISMO NOMBRE DEL TAB COMO EN LA LINEA 33
       # comienza primer fila del tercer tab
       fluidRow(column( width=2),        # primer cuadro info del experto se deba un espacio
                column( width=8,
         boxPlus(title = htmlOutput("opinionOkCambio"),  closable = TRUE, width = NULL, status = "success",  solidHeader = TRUE,
                 collapsible = TRUE, enable_dropdown = TRUE, dropdown_icon = "external-link-square-alt",
                dropdown_menu = dropdownItemList(dropdownItem(url = "https://www.cimat.mx/es/Monterrey", name = "Ir al boletin") ),
                                 p("Descargue el boletin") ) ), column( width = 2)) ,
       # se termina primer fila del tercer tab que contiene solo el mensaje del experto
       # inicia segunda fila del tercer tab, con las graficas y tablas de pronosticos para el tipo de cambio
        fluidRow(
          # inicia box con sus propias tabs
          tabBox(side = "left",  title = "INPC Regional", id = "tabReg", height = "450px", width = 12,
         # primer subtab del box de la tercer tab
          tabPanel("Pronóstico", p(),  htmlOutput("CambioHTML"), p(), plotlyOutput("Cambioplot")),
         # segunda subtab del primer cuadro
          tabPanel("Tabla",  DT::dataTableOutput("tablaCambio"),  downloadButton("downloadCambio", "Descargar pronosticos")))),
       # termina segunda fila del tercer tab
       # inicia tercera fila del tercer tab con los controles de la visualizacion
       fluidRow(
           # control de los graficos
           box(side='right', title='Opciones del pronóstico', id='controlCambio', height='150px', width = 12,
               p('Con los botones puede verificar la calidad del pronóstico mes a mes desde enero de 2015. Elige el mes y año de inicio del pronóstico, asi_ como el horizonte a pronosticar'),
           column(width=2,
                     pickerInput(inputId = "Cambioinitmes", label = "Mes inicio",
                               choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                               options = list(  style = "btn-warning"))),
           column(width=2,
                     pickerInput( inputId = "Cambioinitanio", label = "Año inicio", choices = c("2017", "2018"),
                               options = list(style = "btn-warning"))),
            column(width=2,
                     pickerInput(inputId = "Cambiomes", label = "Meses a pronosticar",
                               choices = c("1", "3"), options = list(  style = "btn-warning"))),
            column(width=2,
                     pickerInput(inputId = "CambioHist", label = "Histórico de la serie",
                                 choices = c("No", "Si_"), options = list(  style = "btn-warning"))) )),
            # termina tercer fila
            # comienza ultima fila del tercer tab pronosticos mas recientes para tipo de cambio
            fluidRow(column(width=11, htmlOutput('PronosticosCambio'))),
            fluidRow(column(width=12,
             boxPlus(title='', closable = TRUE, status = "info", width=6,
                     htmlOutput("CambioHTML1mes"), plotlyOutput("Cambio1mes")),
            # cuarto cuadro (pronostico tres meses con toda la info)
            boxPlus(title='', closable = TRUE, status = "info", width=6,
                     htmlOutput("CambioHTML3mes"), plotlyOutput("Cambio3mes"))
            # quinto cuadro (pronostico seis meses con toda la info)
    #    boxPlus(title='', closable = TRUE, status = "info", width=4,
    #    htmlOutput("CambioHTML6mes") ,     plotlyOutput("Cambio6mes"))
            ) ) )
    # termina ultima fila del tercer tab
      )
    )

# termina body












ui = dashboardPage(skin = "red",
                   dashboardHeader(title = "CIMAT  Observatorio Econometrico"),
                   sidebar, body)


