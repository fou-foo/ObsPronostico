#--------------------------------------
# Actualizacion en el servidor de CIMAT 11 de abril de 2019. Antonio-
# Cambio de rango max() por min()
#--------------------------------------
{
  remove(list=ls())
  t.inicio <- Sys.time()
  library(pls)
  library(lubridate)
  library(vars)
  library(forecast)
  #library(ks)
  dt.file <-"/home/josegarcia/obsPronostico/"
  source(paste0(dt.file, "model_functions.r", sep =""))
  source(paste(dt.file, "inflacionMax.r", sep =""))

  #---------------------------------------
  # DATOS
  #---------------------------------------
  #cuidamos de que la lectura no incluya nulos para el futuro desarrollo con la API
  monetario <- read.csv(paste(dt.file, "/Econ.DataMax/Monetario.csv", sep = ""), row.names = 1)
  row.names(monetario) <- as.character(dmy(row.names(monetario)))

  monetario <- read.csv(paste(dt.file, "/Econ.DataMax/Monetario.csv", sep = ""), row.names = 1)
  row.names(monetario) <- as.character(dmy(row.names(monetario)))

  costos <- read.csv(paste(dt.file, "/Econ.DataMax/Costos.csv", sep = ""), row.names = 1)
  row.names(costos) <- as.character(dmy(row.names(costos)))

  demanda <- read.csv(paste(dt.file, "/Econ.DataMax/Demanda.csv", sep = ""), row.names = 1)
  row.names(demanda) <- as.character(dmy(row.names(demanda)))

  precios <- read.csv(paste(dt.file, "/Econ.DataMax/Precios.csv", sep = ""), row.names = 1)
  row.names(precios) <- as.character(dmy(row.names(precios)))
  precios.row <- precios

  #---------------------------------------
  # PARAMETROS
  #---------------------------------------
}
{
  # paralelizacion en bash
  args <- commandArgs(TRUE)
  print(c(args))
  length.fore <- strtoi(args[1])  # Num. de meses a pronosticar
  mes.shiny <- strtoi(args[2])
 # length.fore <-1; mes.shiny <- 4
  
  # 
  print(paste0('Número de meses a pronosticar ',length.fore ))
  print(paste0('Mes a pronosticar ', mes.shiny))
  # borrar para Tipo de cambio
  regiones <- c("Frontera.norte", "Noroeste", "Noreste", "Centro.norte",
              "Centro.sur", "Sur", "Mexico", "Nacional")
  variable <- "no se usa"
  c.sig <- 0.10      # Nivel de significancia
  show.data <- 48    # ventana de tiempo
  length.test <- 6    # Meses a probar  intramuestra
  n.try <- 5         # Rezagos a probar
  restrict <- FALSE  # TRUE Si pronostico no puede superar (min,max)
  objective <- 3     # Lo usa en el bias - Ahora el objetivo de BM es 3
#  lag.max <- 12    # Para el numero de modelos
#  seas.max <- 12    # Para el numero de modelos
  # mientras seguimos en desarrollo 
  #>> para acortar los tiempos d eejecucion 
  lag.max <- 6    # Para el numero de modelos
  seas.max <- 6    # Para el numero de modelos
  ####
  anual <- 12             # Para tasa interanual >> lo añadio Andres
  ec.det <- c("none", "const", "trend")
  anio.parser <- c(paste0('0',1:9), 10:12)
  names(anio.parser) <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio',
                'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')
  anio.loop <- as.character(2017:2018) # consideremos despues parelizar esto
  #length.fore  <- 6# Num. de meses a pronosticar
}
  # anio.shiny <- anio.loop[2];mes.shiny <- anio.parser[12]

  # length.fore <- 1
    for( anio.shiny in anio.loop)
    {
      # anio.shiny <- '2017'
      print(paste0('anio: ', anio.shiny))
        for(mes.shiny in anio.parser )
        {
            mes.first <- dmy("01/01/2005")
            mes.last <- dmy(paste0('01/', mes.shiny, '/', anio.shiny))
            mes.format <- as.Date(mes.last, format = "%d/%m/%Y")
            dir.create(paste0(dt.file,'resultados_boletinMax/', mes.format, '/'))
            setwd(paste0(dt.file,'resultados_boletinMax/', mes.format))
            #------------------------------------------
            # LOOP sobre las regiones
            #------------------------------------------
          {
            pronostico_regional <- as.data.frame(matrix(NA, nrow=length.fore+1))
            pronostico_tasa_mensual <- as.data.frame(matrix(NA, nrow=length.fore+1))
            pronostico_tasa_interanual <- as.data.frame(matrix(NA, nrow=length.fore+1))
          }
            for(index in 1:length(regiones))
            { #regiones a iterar
              # index <- 8
              region <- regiones[index]
              #--------------------------------------
              # OUTPUT
              #--------------------------------------
              pronostico <- as.data.frame(matrix(NA, nrow=length.fore+1))
              #--------------------------------------
              print(c(region))
              print(mes.last)
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
            }
            pronostico_regional[,1] <- mes_pronostico
            colnames(pronostico_regional) <- c('FECHA', regiones)
            getwd()
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
# se tardo 5 hrs en los archivos que hay


