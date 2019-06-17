#####################################################################
# Actualizacion en el servidor de CIMAT 17 de junio de 2019        # 
# J. Antonio García Ramirez  jose.antonio@cimat.mx               ####
#####################################################################
# Programa para ejecutar todos los pronosticos y graficas HASTA DICIEMBRE DE 2018
# que se emplean en la app disponible en http://10.14.10.84:3838/obsPronostico/
# de la red interna de CIMAT para el INPC
#####################################################################
# Carga de librerias
{
  remove(list=ls())
  t.inicio <- Sys.time() # medimos el tiempo de ejecucion
  library(pls)    # estimacion de minimos cuadrados parciales
  library(lubridate) # manejo sencillo de fechas
  library(vars) # funciones para estimar modelos var
  library(forecast) # unknown
  dt.file <-"/home/fou/Desktop/ObsPronostico/obsPronostico/" # direccion en dond se encuentra la carpeta 'obsPronostico' en el servidor (es el ambiente de desarollo NO LA DE PRODUCCION)
  source(paste0(dt.file, "model_functions.r", sep ="")) # define funciones para chechar cointegracion, seleccion y validacion de modelos VAR
  source(paste(dt.file, "inflacionMax.r", sep ="")) # define una unica funcion que selecciona el mejor modelo y escribe en disco archivos 
}
#####################################################################
# lectura de DATOS de las 24 series de tiempo
#####################################################################
# el contenido de cada archivo de lectura se encuentra en la documentacion 
# IMPORTANTE: mantener la estructura y nombre de archivos que se menciona en la documentacion
{ 
  monetario <- read.csv(paste(dt.file, "/Econ.DataMax/Monetario.csv", sep = ""), row.names = 1)
  row.names(monetario) <- as.character(dmy(row.names(monetario)))
  costos <- read.csv(paste(dt.file, "/Econ.DataMax/Costos.csv", sep = ""), row.names = 1)
  row.names(costos) <- as.character(dmy(row.names(costos)))
  demanda <- read.csv(paste(dt.file, "/Econ.DataMax/Demanda.csv", sep = ""), row.names = 1)
  row.names(demanda) <- as.character(dmy(row.names(demanda)))
  precios <- read.csv(paste(dt.file, "/Econ.DataMax/Precios.csv", sep = ""), row.names = 1)
  row.names(precios) <- as.character(dmy(row.names(precios)))
  precios.row <- precios
}
####################################################################
#  PARAMETROS
####################################################################
# Se fijan los parametros a valores ya probados.
# para valores de parametros que produzcan errores, la documentación indica que valores usar
{
  args <- commandArgs(TRUE) # recepcion de los valores de la consola de linux
  print(c(args))
  length.fore <- strtoi(args[1])  # Num. de meses a pronosticar
  mes.shiny <- strtoi(args[2]) # ano a pronosticar
  print(paste0('Número de meses a pronosticar ',length.fore ))
  print(paste0('Mes a pronosticar ', mes.shiny))
  regiones <- c("Frontera.norte", "Noroeste", "Noreste", "Centro.norte",
              "Centro.sur", "Sur", "Mexico", "Nacional") # regiones economicas del pais (ver documentacion)
  print(paste0('Número de meses a pronosticar ',length.fore ))
  print(paste0('Mes a pronosticar ', mes.shiny))
  c.sig <- 0.10      # Nivel de significancia
  show.data <- 48    # ventana de tiempo
  length.test <- 6    # Meses a probar  intramuestra
  n.try <- 5         # Rezagos a probar
  restrict <- FALSE  # TRUE Si pronostico no puede superar (min,max)
  objective <- 3     # Lo usa en el bias - Ahora el objetivo de BM es 3
  lag.max <- 2    # Para el numero de modelos
  seas.max <- 2    # Para el numero de modelos
  anual <- 12             # Para tasa interanual >> lo añadio Andres
  ec.det <- c("none", "const", "trend")
  anio.parser <- c(paste0('0',1:9), 10:12)
  names(anio.parser) <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio',
                'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')
  anio.loop <- as.character(2017:2018) # anos en los que el usuario puede consultar en el dashboard
}
for( anio.shiny in anio.loop)
{
  # se itera sobre todos los anos PERO DENTRO DE UN MISMO JOB de la consola
  print(paste0('anio: ', anio.shiny))
  for(mes.shiny in anio.parser )
  {
    # se itera sobre todos los meses del ano
    mes.first <- dmy("01/01/2005")
    mes.last <- dmy(paste0('01/', mes.shiny, '/', anio.shiny))
    mes.format <- as.Date(mes.last, format = "%d/%m/%Y")
    dir.create(paste0(dt.file,'resultados_boletinMax/', mes.format, '/')) # se crea la carpeta en donde se depositan los archivos necesarios para visualizar
                                                                          # el promedio de la tasa interanual para el INPC 
    setwd(paste0(dt.file,'resultados_boletinMax/', mes.format))
    # LOOP sobre las regiones
      pronostico_regional <- as.data.frame(matrix(NA, nrow=length.fore+1))
      pronostico_tasa_mensual <- as.data.frame(matrix(NA, nrow=length.fore+1))
      pronostico_tasa_interanual <- as.data.frame(matrix(NA, nrow=length.fore+1))
      for(index in 1:length(regiones))
      { 
        region <- regiones[index]
        pronostico <- as.data.frame(matrix(NA, nrow=length.fore+1)) # DATA.FRAME QUE CONTIENE LOS RESULTADOS DEL PRONOSTICO
        #--------------------------------------
        print(c(region))
        print(mes.last)
        # la siguiente funcion se encarga de elegir el mejor modelo y escribir a disco las graficas y archivos que requiere el dashboard
        RESULT <- INFLACION(precios.row = precios.row, price=precios, costos=costos, demanda=demanda,
                                  monetario=monetario, region=region, variable=variable,
                                  mes.first=mes.first, mes.last=mes.last,
                                  length.fore=length.fore, lag.max=lag.max,
                                  c.sig=c.sig, show.data=show.data, seas.max=seas.max,
                                  length.test=length.test, n.try=n.try, restrict=restrict,
                                  objective=objective, ec.det=ec.det)
        #--------------------------------------
        last <- which(rownames(precios)==mes.last)
        real <- precios[(last):(last+length.fore),index]
        forecast <- c(precios[last,index],RESULT[[2]][1:length.fore, 'Pronostico'])
        linf <-  RESULT[[2]][1:length.fore, 'LimInf']
        lsup <-  RESULT[[2]][1:length.fore, 'LimSup']
        precision <- (1 - abs(real-forecast)/real)*100
        # calculo de la tasa interanual que utiliza el mapa del dashboard
        precios_tasa_interanual <- precios[(last-anual):last,index]
        tasa_interanual <- (forecast/precios_tasa_interanual[1:(length.fore+1)] -1)*100
        precios_tasa_mensual <- c(precios[(last-1),index], forecast[1:length.fore])
        tasa_mensual <- (forecast/precios_tasa_mensual -1)*100

        mes.format <- as.Date(mes.last, format = "%d/%m/%Y")
        mes_pronostico <-as.character(seq(as.Date(mes.format),by = "month", length = length.fore+1))
        pronostico <- cbind(as.data.frame(mes_pronostico), as.data.frame(forecast),
                                  as.data.frame(tasa_mensual), as.data.frame(tasa_interanual))
        pronostico_regional <-cbind(pronostico_regional, forecast)
        pronostico_tasa_mensual <-cbind(pronostico_tasa_mensual, tasa_mensual)
        pronostico_tasa_interanual <- cbind(pronostico_tasa_interanual, tasa_interanual)
        #--------------------------------------
        # ARCHIVANDO RESULTADOS
        #--------------------------------------
        mes.format <- sub("-01", "", mes.format)
        colnames(pronostico) <- c("FECHA", "INPC","Tasa Mensual","Tasa Interanual")
        dir.create(paste0(dt.file, "resultados_boletinMax/"))
        setwd(paste0(dt.file, "resultados_boletinMax/"))
        write.csv(pronostico, paste0("Pronostico_Inflacion", gsub('\\.', '_', region),"_",mes.format,".csv"),row.names = F)
        # se termina calculo de todas las regiones en un solo mes de un solo año
        }
        pronostico_regional[,1] <- mes_pronostico
        colnames(pronostico_regional) <- c('FECHA', regiones)
        write.csv(pronostico_regional, paste0("Pronostico_Regional_",gsub('\\.', '_', region),".csv"),row.names = F)
        pronostico_tasa_mensual[,1] <- mes_pronostico
        colnames(pronostico_tasa_mensual) <- c('FECHA', regiones)
        write.csv(pronostico_tasa_mensual, paste0("PronosticoINPC_Tasa_Mensual_",gsub('\\.', '_', region),".csv"),row.names = F)
        pronostico_tasa_interanual[,1] <- mes_pronostico
        colnames(pronostico_tasa_interanual) <- c('FECHA', regiones)
        write.csv(pronostico_tasa_interanual, paste0("PronosticoINPC_Tasa_Interanual_anio",anio.shiny,"mes_", mes.format, "horizonte_",length.fore, ".csv"),row.names = F)
  }
}
t.fin<- Sys.time()
t.fin-t.inicio



