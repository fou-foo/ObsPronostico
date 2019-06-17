##########################################
# Proyecto: Dashboard de pronosticos del laboratorio econometrico
# J. Antonio Garcia: jose.ramirez@cimat.mx
# CODIGO PARA CONSTRUIR LA INTERFAZ GRAFICA para descarga de tasas interanuales
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
}
#########################################
# Construccion de la UI                 #
#########################################

# Esta seccion se encarga de contruir la parte derecha de la vista del dashboard
# donde aparecen los nombres en la seccion negra
sidebar <- dashboardSidebar(
    #comenzamos con el menu
    sidebarMenu(
        menuItem("INPC Regional", icon = icon("chart-line"), tabName = "INPCReg")
    )
)

# Construccion de cuerpo del dashbord, debe de hacerse
# tab por tab
body <- dashboardBody( #comienza la construccion del cuerpo
tabItems( #esta linea que contiene todos los taps
    # segundo tab  con los pronosticos de la inflacion a nivel regional y CDMX
    tabItem(tabName = "INPCReg", #IMPORTANTE QUE SE LLAME IGUAL QUE EN LA LINEA 32
     fluidRow(
        # caja que permite introducir varias pestanas en ella
       tabBox(side = "left",  title = "INPC Regional", id = "tabReg", height = "450px", width = 12,
        # primer subtab de la caja contiene el grafico del pronostico que se pide con los controles
        # segunda subtab contieje la tabla que se puede descargar (regional)
        tabPanel("Tabla tasa interanual",  DT::dataTableOutput("tablaReg"), downloadButton("downloadReg", "Descargar tasas")),
        # tercera subtab contiene el mapa
        tabPanel('Mapa Regional', p(), htmlOutput("TasaDeCambio"), p(), leafletOutput("mapaReg") )  ) ) ,
    # fin de segunda fila del segundo tab
    # comienza tercer fila del segundo tab contiene los controles de la visualizacion
     fluidRow(
         # control de los graficos
         box(side='right', title='Opciones del pronóstico', id='controlreg', height='175px', width = 12,
         p('Con los botones puede verificar la calidad del pronóstico mes a mes desde enero de 2017. Elige el mes y año de inicio del pronóstico, asi_ como el horizonte a pronosticar y la región'),
         column(width=2,
                pickerInput(inputId = "Reg", label = "Región",
                            choices = c("Frontera.norte", "Noroeste", "Noreste", "Centro.norte", "Centro.sur", "Sur", "Mexico"),
                            options = list(  style = "btn-primary"))),
         column(width=2,
                pickerInput(inputId = "inpcReginitmes", label = "Mes inicio",
                    choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                    options = list(  style = "btn-warning"))),
               column(width=2,
                    pickerInput( inputId = "inpcReginitanio", label = "Año inicio", choices = c("2017", "2018", "2019"),
                    options = list(style = "btn-warning")) ),
         column(width=2,
                    pickerInput(inputId = "inpcRegmes", label = "Meses a pronosticar",
                    choices = c("1", "3"), #"6"), por recomendacion de la doctora se quita el pronostico a 6 meses pero si se calculo y se encuentra en la carpeta 'BinariosMax'
                    options = list(  style = "btn-warning"))),
         column(width=2,
                    pickerInput(inputId = "inpcRegHist", label = "Histórico de la serie",
                                choices = c('No', 'Sí'),
                                options = list(  style = "btn-warning"))))  
     ) 
    )
)
)


# termina body












ui = dashboardPage(skin = "red",
                   dashboardHeader(title = "CIMAT  Observatorio Econometrico"),
                   sidebar, body)


