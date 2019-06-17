#############################################################
# Actualizacion en el servidor de CIMAT 17 de junio de 2019 #
# J. Antonio García Ramirez  jose.antonio@cimat.mx          #
#############################################################
# Esta funcion se encarga de elegir el modelo con los mejores 
# pronosticos para un mes y dia fijo 
#############################################################
# bibliotecas
{
  library(lubridate) # manjeo sencillo de fechas
  library(ggplot2) # creacion de graficos
  library(reshape2) # manipulacion de datos 
  library(plotly) # creacion de graficos interactivos
}
INFLACION <- function(precios.row, price, costos, demanda, monetario,
                      region, variable, mes.first, mes.last,
                      length.fore, lag.max, c.sig, show.data,
                      seas.max, length.test, n.try, restrict,
                      objective, ec.det)
{
    #Entradas:
        # price (data.frame): con las series a pronosticar (historico)
        # costos (data.frame): con las series del eje economico de costos
        # demanda (data.frame): con las series del eje economico de demanda
        # monetario (data.frame): con las series del eje economico monetario
    # region (string): Region economica de interes
    # mes.first (date): Punto fijo en el tiempo (enero de 2005 debido a la variable 'desempleo')
    # mes.last (date): Limite temproal para realizar el pronostico
    # length.fore (int): Numero de meses a pronosticar
    # lag.max (int): Numero maximo de lags a considerar
    # c.sig (numeric): Nivel de significancia
    # show.data (int): Numero de meses con el cual probar intramuestra
    # seas.max (int): maximo numero de lags a considerar para seasonability
    # length.test (int): Anaologo a meses a pronosticar pero se utiliza en la segunda fase de la metodoligia
    # n.try (int)
    # restirct (bool) : Condicion de restriccion dentro de los limites historicos
    # objetive (int) : unkwon
    # ec.det (vector:character): Los tres tipos de tendencia
  # preparacion de datos
  first <- which(rownames(precios)==mes.first) 
  last <- which(rownames(precios)==mes.last)
  costos    <- (costos[first:last,])
  monetario <- (monetario[first:last,])
  demanda   <- (demanda[first:last,])
  precios   <- (precios[first:last,])
  precios <- as.data.frame(precios)
  price <- as.data.frame(precios)
  names(precios) <- names(price) <- names(precios.row)
  #REDUCCION DE DIMENSION#--------------------------------

  costos.pls <- factor.pls(price, costos)
  monetario.pls <- factor.pls(price, monetario)
  demanda.pls <- factor.pls(price, demanda)

  endog.mat <- cbind(precios[,region], as.data.frame(costos.pls$score.pls),
                     as.data.frame(monetario.pls$score.pls),
                     as.data.frame(demanda.pls$score.pls))
  colnames(endog.mat) <- c(region, "Costos", "Monetario", "Demanda")
  load.mat.costos <- as.data.frame(costos.pls$load.mat.w)
  load.mat.monetario <- as.data.frame(monetario.pls$load.mat.w)
  load.mat.demanda <- as.data.frame(demanda.pls$load.mat.w)

  # SELECCION DE MODELO#---------------
  ## Formando especificacion de todas las combinaciones posibles de parametros de los modelos
  c.num.cols <- ncol(endog.mat)
  n <- nrow(endog.mat)
  exog.mat <- NULL
  exog.new <- as.data.frame(matrix(rep(0, length.fore)))
  colnames(exog.new) <- c("Outliers")
  idx.matrix  <- as.data.frame(matrix(0, (lag.max - 1)*(seas.max - 1)*length(ec.det), 3))
  colnames(idx.matrix) <- c("seas", "lag", "ec.det")
  idx.matrix[,"seas"] <- rep(2:seas.max, each = (length(ec.det))*(lag.max - 1))
  idx.matrix[,"lag"] <- rep(c(2:lag.max), nrow(idx.matrix)/(lag.max - 1))
  idx.matrix[,"ec.det"] <- rep(rep(ec.det, each = lag.max - 1), seas.max - 1)

  ## Variables que cargan los elementos con los que se mide el desempe?o del modelo de pronostico
  crit.stat <- c("MAPE", "MSE",
                 "THEIL", "BIAS",
                 "LOG.LIK","CONF", "R2.ADJ", "COMMON.TREND")
  idx.endog <- as.data.frame(matrix(NA, nrow(idx.matrix), length(crit.stat)))
  colnames(idx.endog) <- crit.stat
  values <-  c("forecast", "vec", "ce", "w", "common.trends", "common.trend", "statistic", "try.forecast")
  mape <- mse <- theil <- bias <- log.lik <- c.i <- r.2 <- trend <- rep(list(NA), length(values))
  names(mape) <- names(mse) <- names(theil) <- names(bias) <- names(log.lik) <-
    names(c.i) <- names(r.2) <- names(trend) <- values
  list.forecast.test <- list() #Lista para almacenar los rendimientos
  ### NOTA: Core del codigo. Loop que corre todos los modelos siguiendo:
  ### Loop que corre todos los modelos siguiendo:
  ### (a) Cointegracion de variables originales
  ### (b) Pronostico
  ### (c) Evaluacion desempenio
  for(i in  1 : nrow(idx.endog))
  {
    seas <- idx.matrix[i, "seas"]
    i.lag <- idx.matrix[i, "lag"]
    ec.det <- idx.matrix[i, "ec.det"]

    if(seas == 2)
      seas <- NULL

    # (a) COINTEGRANCION VARIABLES ORIGINALES
    # Primera estimacion de Johansen
    johansen <- ca.jo(endog.mat, K = i.lag, season = seas,
                      ecdet = ec.det, spec=c("longrun"), dumvar = exog.mat)
    # Obtenemos el rango de la matriz de cointegracion
    test.ca <- johansen@teststat < johansen@cval[,"10pct"]
    names(test.ca) <- c(c(ncol(endog.mat) - 1) : 0)
    r <- as.numeric(names(test.ca)[which(test.ca)])
    if(min(r)<1) r <-1#cuchareo
    if(any(r == 0) || length(r) == 0) next
        vec <- vec2var(johansen, r = max(r)) #aqui es donde creo que es MAX

    # Identificamos Outliers Multivariados
    if(sum(mult.outliers(vec)$out.var) == 0)
    {
      outliers <- rep(0, nrow(vec$y))
      outliers[which.max(rowSums(abs(scale(resid(vec))))) + vec$p] <- 1
      # lo siguiente es la justificacion del cambio de linea

      #>>>outliers.foo <- outliers
      #outliers[(which(rowSums(abs(scale(resid(vec)))) ==
       #                   max(rowSums(abs(scale(resid(vec)))))))+ vec$p] <- 1 # esta linea se puede hacer mas breve
      # >>> identical(outliers.foo,outliers)
      #outliers[(which(rowSums(abs(scale(resid(vec)))) == max(rowSums(abs(scale(resid(vec)))))))+ vec$p] <- 1
    }
    outliers <- c(rep(0, vec$p), mult.outliers(vec)$out.var)

    # Nuevas Exogenas
    exog.test <- cbind(exog.mat, outliers)
    colnames(exog.test) <- c(colnames(exog.mat), "Outliers")

    # Reestimamos Johansen
    johansen <- ca.jo(endog.mat, K = i.lag, season = seas,
                      ecdet = ec.det, spec=c("longrun"), dumvar = exog.test)

    # Obtenemos el rango de la matriz de cointegracion nuevamente
    test.ca <- johansen@teststat < johansen@cval[,"10pct"]
    names(test.ca) <- c(c(ncol(endog.mat) - 1) : 0)
    r <- as.numeric(names(test.ca)[which(test.ca)])
    if(min(r)<1) r <-1#cuchareo
    # Reestimamos el rango de la matriz de cointegracion
    if(any(r == 0) || length(r) == 0) next
    vec <- vec2var(johansen, r = max(r))

    # (b) COINTEGRANCION CON PRONOSTICO PARA SIGUIENTES (length.fore) MESES
    # Creamos la matriz para guardar los resultados
    forecast.mult <- matrix(NA, length.fore, ncol(vec$y) + 3)
    colnames(forecast.mult) <- c(colnames(vec$y), "CI", "lower", "upper")

    # Ciclo para recursion de pronostico de modelo cointegrado
    for(j in 1 : length.fore){

      ## Pronostico del modelo
      #
      forecast <- sapply(1 : ncol(vec$y), function(x) predict.vec2var(vec, n.ahead = 1,
                                                                      dumvar = as.matrix(exog.new)[j,,drop = FALSE],
                                                                      mat.res = NULL)$fcst[[x]][,c("fcst", "CI", "lower", "upper")])
      colnames(forecast) <- colnames(vec$y)

      # Reestimamos Johansen
      y <- rbind(vec$y, forecast["fcst",])
      x <- rbind(eval(vec$vecm@dumvar), exog.new[j, ])
      rownames(y) <- rownames(x) <- NULL
      johansen <- ca.jo(y, K = i.lag, season = seas,
                        ecdet = ec.det, spec=c("longrun"), dumvar = x)

      # Obtenemos el rango de la matriz de cointegracion nuevamente
      test.ca <- johansen@teststat < johansen@cval[,"10pct"]
      names(test.ca) <- c(c(ncol(endog.mat) - 1) : 0)
      r <- as.numeric(names(test.ca)[which(test.ca)])
      if(min(r)<1) r <-1#cuchareo
      if(any(r == 0) || length(r) == 0) next
      vec <- vec2var(johansen, r = max(r)) # >> max
      dim(vec)

      # Aqui va llenando los datos del pronostico
      forecast.mult[j,1:ncol(vec$y)] <- forecast["fcst",]
      forecast.mult[j,"CI"] <- forecast["CI", region]
      forecast.mult[j,"lower"] <- forecast["lower", region]
      forecast.mult[j,"upper"] <- forecast["upper", region]
    }

    # Hubo algun modelo con r = n o r = 0
    if(any(is.na(forecast.mult))) next
    # Restriccion pronostico atipico #>>> el detalle es que truena si lo prendemos
    if(restrict)
    {
        if(any(forecast.mult[,region] > max(price)) ||
         any(forecast.mult[,region] < min(price))) next
    }
    # Matriz de cointegracion normalizada
    coint.mat <- sapply(1 : max(r),
                        function(x) scale(-(cointegration(vec$vecm)[,x] - vec$y[,region])))
    # Ecuaciones de cointegracion
    coint.equa <- sapply(1 : max(r) , function(x) -round(vec$vecm@V[,x][-1], 4))
    # Pesos de acuerdo a Lambdas
    w.fore <- johansen@lambda[1 : max(r)]/sum(johansen@lambda[1 : max(r)])
    # Tendencia comun unica

    common.trend <- rowSums(sapply(1 : max(r), function(x) w.fore[x] *
                                     coint.mat[,x]))*sapply(price,sd) + sapply(price,mean)
    # Pronostico de la tendencia comun
    fore.common.trend <- common.trend[c(n + 1): c(n + length.fore)]
    # Restriccion de tendencia comun atipica
    if(restrict)
    {
      if(any(fore.common.trend > max(price)) ||
         any(fore.common.trend < min(price))) next
    }
    # Estadistico de relacion
    beta <- ca.jo(cbind(vec$y[,region], common.trend), K = i.lag, season = seas,ecdet = ec.det, spec=c("longrun"),
                  dumvar = rbind(exog.test, exog.new[,colnames(exog.test), drop = FALSE]))@V[2,1]

    # (c) DESEMPENIO DEL MODELO
    ## Seleccion recursiva de modelos de pronosticos. Pronostica 'n.try' periodos de muestra tres pasos adelante
    mat.predict <- matrix(NA, n.try, 4)
    colnames(mat.predict) <- crit.stat[1:4]

    # Empieza el ciclo para seleccionar el modelo
    for(k in 1 : n.try)
    {
        i.test.1 <- c(1:(n-k-length.test+1))
        i.test.2 <- c(n-k-length.test + 2):c(nrow(vec$y[c(1:(n-k-length.test+1)),]) + length.test)
        endog.try <- vec$y[1:n, ,drop = FALSE]
        exog.try <- x[1:n, ,drop = FALSE]
        endog.try <- vec$y[i.test.1,,drop = FALSE]
        exog.try <- x[i.test.1,,drop = FALSE]
        exog.real <- x[i.test.2,, drop = FALSE]
        endog.real <- vec$y[i.test.2, region, drop = FALSE]
        idx.x <- apply(exog.try, 2, sum) != 0
        #cambio en esta linea el K= vec$p por i.lag
        johansen.predict <- ca.jo(endog.try, K = i.lag,
                                seas = seas, dumvar = exog.try[,idx.x, drop = FALSE],
                                ecdet = ec.det, spec=c("longrun"))
      # Obtenemos el rango de la matriz de cointegracion nuevamente
      test.ca <- johansen.predict@teststat < johansen.predict@cval[,"10pct"]
      names(test.ca) <- c(c(ncol(endog.try) - 1) : 0)
      r <- as.numeric(names(test.ca)[which(test.ca)])
      if(min(r)<1) r <-1#cuchareo
      # Reestimamos el rango de la matriz de cointegracion
      if(any(r == 0) || length(r) == 0) next
      vec.test <- vec2var(johansen.predict, r = max(r)) #>> max
      fore.mult.test <- matrix(NA, length.test, ncol(vec$y))
      colnames(fore.mult.test) <- colnames(vec$y)
      for(j in 1 : length.test) #error
      {
        ## Pronostico del modelo
        forecast.test <- sapply(1 : ncol(vec.test$y), function(x) predict.vec2var(vec.test, n.ahead = 1,
                                                                                  dumvar = exog.real[j,idx.x,drop = FALSE],
                                                                                  mat.res = NULL)$fcst[[x]][,"fcst"])
        # Reestimamos Johansen
        endog.try <- rbind(vec.test$y, forecast.test)
        exog.try <- rbind(eval(vec.test$vecm@dumvar), exog.real[j,idx.x,drop = FALSE])
        rownames(endog.try) <- rownames(exog.try) <- NULL
        johansen.test <- ca.jo(endog.try, K = i.lag, season = seas,
                               ecdet = ec.det, spec=c("longrun"), dumvar = exog.try)
        # Obtenemos el rango de la matriz de cointegracion nuevamente
        test.ca <- johansen.test@teststat < johansen.test@cval[,"10pct"]
        names(test.ca) <- c(c(ncol(endog.try) - 1) : 0)
        r <- as.numeric(names(test.ca)[which(test.ca)])
        if(any(r == 0) || length(r) == 0) next
        vec.test <- vec2var(johansen.test, r = max(r)) #>> max
        fore.mult.test[j,] <- forecast.test
      }
      list.forecast.test[[k]] <- fore.mult.test
      # se guardan los desemnios
      mat.predict[k, "MAPE" ] <- mean(abs(c(endog.real - fore.mult.test[,region])/endog.real))
      mat.predict[k, "MSE"] <- sqrt(mean((fore.mult.test[,region] - endog.real)^2))
      mat.predict[k, "THEIL"] <- sqrt(mean((fore.mult.test[,region] - endog.real)^2))/
        (sqrt(sum((fore.mult.test[,region])^2)/length.test)
         + sqrt(sum((endog.real)^2)/length.test))
      mat.predict[k, "BIAS"] <-(mean(fore.mult.test[,region])-mean(endog.real))^2/mean((fore.mult.test[,region] - endog.real)^2)
    }
    mean.mat.predict <- round(colMeans(mat.predict), 4)

    ## Estadisticos del Modelo
    # Porcentaje relativo de error
    idx.endog[i, "MAPE"]  <-  mean.mat.predict["MAPE"]
    # Raiz de Error Cuadratic Medio
    idx.endog[i, "MSE"]  <- mean.mat.predict["MSE"]
    # Estadistico de Theil
    idx.endog[i, "THEIL"] <- mean.mat.predict["THEIL"]
    idx.endog[i, "BIAS"] <- mean.mat.predict["BIAS"]
    # Modelo que maximiza la funcion de verosimilitud
    idx.endog[i, "LOG.LIK"] <- -logLik(vec)
    # MSE Teorico
    idx.endog[i, "CONF"] <- mean(forecast.mult[,"CI"])
    # R2
    idx.endog[i, "R2.ADJ"] <- 1 - summary(cajools(johansen))[[1]]$adj.r.squared
    # Common Trend
    idx.endog[i, "COMMON.TREND"] <- abs(1 - beta)
    # Condicionamos valores
    if(!is.na(idx.endog[i, "MAPE"]))
    {
      if(all(idx.endog[1:c(i - 1), "MAPE"] >= idx.endog[i, "MAPE"], na.rm = TRUE))
      {
          mape[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
          mape[["vec"]] <- vec
          mape[["ce"]] <- coint.equa
          mape[["w"]] <- w.fore
          mape[["common.trends"]] <- coint.mat
          mape[["common.trend"]] <- common.trend
          mape[["statistic"]] <- mean.mat.predict["MAPE"]
          mape[["try.forecast"]] <- list.forecast.test
      }
    }
    if(!is.na(idx.endog[i, "MSE"]))
    {
        if(all(idx.endog[1:c(i - 1), "MSE"] >= idx.endog[i, "MSE"], na.rm = TRUE))
        {
            mse[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
            mse[["vec"]] <- vec
            mse[["ce"]] <- coint.equa
            mse[["w"]] <- w.fore
            mse[["common.trends"]] <- coint.mat
            mse[["common.trend"]] <- common.trend
            mse[["statistic"]] <- mean.mat.predict["MSE"]
            mse[["try.forecast"]] <- list.forecast.test
        }
    }
    if(!is.na(idx.endog[i, "THEIL"]))
    {
      if(all(idx.endog[1:c(i - 1), "THEIL"] >= idx.endog[i, "THEIL"], na.rm = TRUE))
      {
          theil[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
          theil[["vec"]] <- vec
          theil[["ce"]] <- coint.equa
          theil[["w"]] <- w.fore
          theil[["common.trends"]] <- coint.mat
          theil[["common.trend"]] <- common.trend
          theil[["statistic"]] <- mean.mat.predict["THEIL"]
          theil[["try.forecast"]] <- list.forecast.test
      }
    }
    if(!is.na(idx.endog[i, "BIAS"]))
    {
      if(all(idx.endog[1:c(i - 1), "BIAS"] >= idx.endog[i, "BIAS"], na.rm = TRUE))
      {
          bias[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
          bias[["vec"]] <- vec
          bias[["ce"]] <- coint.equa
          bias[["w"]] <- w.fore
          bias[["common.trends"]] <- coint.mat
          bias[["common.trend"]] <- common.trend
          bias[["statistic"]] <- mean.mat.predict["BIAS"]
          bias[["try.forecast"]] <- list.forecast.test
      }
    }
    if(!is.na(idx.endog[i, "LOG.LIK"]))
    {
      if(all(idx.endog[1:c(i - 1), "LOG.LIK"] <= idx.endog[i, "LOG.LIK"], na.rm = TRUE))
      {
          log.lik[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
          log.lik[["vec"]] <- vec
          log.lik[["ce"]] <- coint.equa
          log.lik[["w"]] <- w.fore
          log.lik[["common.trends"]] <- coint.mat
          log.lik[["common.trend"]] <- common.trend
          log.lik[["statistic"]] <- idx.endog[i, "LOG.LIK"]
          log.lik[["try.forecast"]] <- list.forecast.test
      }
    }
    if(!is.na(idx.endog[i, "CONF"]))
    {
      if(all(idx.endog[1:c(i - 1), "CONF"] >= idx.endog[i, "CONF"], na.rm = TRUE))
      {
          c.i[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
          c.i[["vec"]] <- vec
          c.i[["ce"]] <- coint.equa
          c.i[["w"]] <- w.fore
          c.i[["common.trends"]] <- coint.mat
          c.i[["common.trend"]] <- common.trend
          c.i[["statistic"]] <- idx.endog[i, "CONF"]
          c.i[["try.forecast"]] <- list.forecast.test
      }
    }
    if(!is.na(idx.endog[i, "R2.ADJ"]))
    {
      if(all(idx.endog[1:c(i - 1), "R2.ADJ"] >= idx.endog[i, "R2.ADJ"], na.rm = TRUE))
      {
          r.2[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
          r.2[["vec"]] <- vec
          r.2[["ce"]] <- coint.equa
          r.2[["w"]] <- w.fore
          r.2[["common.trends"]] <- coint.mat
          r.2[["common.trend"]] <- common.trend
          r.2[["statistic"]] <- 1 - idx.endog[i, "R2.ADJ"]
          r.2[["try.forecast"]] <- list.forecast.test
      }
    }
    if(!is.na(idx.endog[i, "COMMON.TREND"]))
    {
      if(all(idx.endog[1:c(i - 1), "COMMON.TREND"] >= idx.endog[i, "COMMON.TREND"], na.rm = TRUE))
      {
          trend[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
          trend[["vec"]] <- vec
          trend[["ce"]] <- coint.equa
          trend[["w"]] <- w.fore
          trend[["common.trends"]] <- coint.mat
          trend[["common.trend"]] <- common.trend
          trend[["statistic"]] <- idx.endog[i, "COMMON.TREND"]
          trend[["try.forecast"]] <- list.forecast.test
      }
    }
  }
  # Resultados
  # caso de un mes a pronosticar
  if (length.fore!=1){
    forecast.price <- cbind(mape[["forecast"]][,region], mse[["forecast"]][,region],
                            theil[["forecast"]][,region], bias[["forecast"]][,region],
                            log.lik[["forecast"]][,region], c.i[["forecast"]][,region],
                            r.2[["forecast"]][,region], trend[["forecast"]][, region])
    colnames(forecast.price) <- crit.stat


    forecast.costos <- cbind(mape[["forecast"]][,"Costos"], mse[["forecast"]][,"Costos"],
                             theil[["forecast"]][,"Costos"], bias[["forecast"]][,"Costos"],
                             log.lik[["forecast"]][,"Costos"], c.i[["forecast"]][,"Costos"],
                             r.2[["forecast"]][,"Costos"], trend[["forecast"]][, "Costos"])
    colnames(forecast.costos) <- crit.stat

    forecast.monetario <- cbind(mape[["forecast"]][,"Monetario"], mse[["forecast"]][,"Monetario"],
                                theil[["forecast"]][,"Monetario"], bias[["forecast"]][,"Monetario"],
                                log.lik[["forecast"]][,"Monetario"], c.i[["forecast"]][,"Monetario"],
                                r.2[["forecast"]][,"Monetario"], trend[["forecast"]][, "Monetario"])
    colnames(forecast.monetario) <- crit.stat

    forecast.demanda <- cbind(mape[["forecast"]][,"Demanda"], mse[["forecast"]][,"Demanda"],
                              theil[["forecast"]][,"Demanda"], bias[["forecast"]][,"Demanda"],
                              log.lik[["forecast"]][,"Demanda"], c.i[["forecast"]][,"Demanda"],
                              r.2[["forecast"]][,"Demanda"], trend[["forecast"]][, "Demanda"])
    colnames(forecast.demanda) <- crit.stat


    fore.costos    <- rowMeans(forecast.costos)
    fore.monetario <- rowMeans(forecast.monetario)
    fore.demanda   <- rowMeans(forecast.demanda)



    linf <- rowMeans(cbind(mape[["forecast"]][,"lower"], mse[["forecast"]][,"lower"],
                           theil[["forecast"]][,"lower"], bias[["forecast"]][,"lower"],
                           log.lik[["forecast"]][,"lower"], c.i[["forecast"]][,"lower"],
                           r.2[["forecast"]][,"lower"], trend[["forecast"]][, "lower"]))

    lsup <- rowMeans(cbind(mape[["forecast"]][,"upper"], mse[["forecast"]][,"upper"],
                           theil[["forecast"]][,"upper"], bias[["forecast"]][,"upper"],
                           log.lik[["forecast"]][,"upper"], c.i[["forecast"]][,"upper"],
                           r.2[["forecast"]][,"upper"], trend[["forecast"]][, "upper"]))

    forecast.mean <- cbind(rowMeans(forecast.price), linf, lsup, lsup -
                             rowMeans(forecast.price))
    colnames(forecast.mean) <- c("Forecast", "linf", "lsup", "CI")

  } else{
    # caso a pronosticar mas de un mes
    forecast.price <- cbind(mape[["forecast"]][region], mse[["forecast"]][region],
                            theil[["forecast"]][region], bias[["forecast"]][region],
                            log.lik[["forecast"]][region], c.i[["forecast"]][region],
                            r.2[["forecast"]][region], trend[["forecast"]][region])
    colnames(forecast.price) <- crit.stat


    forecast.costos <- cbind(mape[["forecast"]]["Costos"], mse[["forecast"]]["Costos"],
                             theil[["forecast"]]["Costos"], bias[["forecast"]]["Costos"],
                             log.lik[["forecast"]]["Costos"], c.i[["forecast"]]["Costos"],
                             r.2[["forecast"]]["Costos"], trend[["forecast"]]["Costos"])
    colnames(forecast.costos) <- crit.stat

    forecast.monetario <- cbind(mape[["forecast"]]["Monetario"], mse[["forecast"]]["Monetario"],
                                theil[["forecast"]]["Monetario"], bias[["forecast"]]["Monetario"],
                                log.lik[["forecast"]]["Monetario"], c.i[["forecast"]]["Monetario"],
                                r.2[["forecast"]]["Monetario"], trend[["forecast"]]["Monetario"])
    colnames(forecast.monetario) <- crit.stat

    forecast.demanda <- cbind(mape[["forecast"]]["Demanda"], mse[["forecast"]]["Demanda"],
                              theil[["forecast"]]["Demanda"], bias[["forecast"]]["Demanda"],
                              log.lik[["forecast"]]["Demanda"], c.i[["forecast"]]["Demanda"],
                              r.2[["forecast"]]["Demanda"], trend[["forecast"]]["Demanda"])
    colnames(forecast.demanda) <- crit.stat


    fore.costos    <- rowMeans(forecast.costos)
    fore.monetario <- rowMeans(forecast.monetario)
    fore.demanda   <- rowMeans(forecast.demanda)



    linf <- rowMeans(cbind(mape[["forecast"]]["lower"], mse[["forecast"]]["lower"],
                           theil[["forecast"]]["lower"], bias[["forecast"]]["lower"],
                           log.lik[["forecast"]]["lower"], c.i[["forecast"]]["lower"],
                           r.2[["forecast"]]["lower"], trend[["forecast"]]["lower"]))

    lsup <- rowMeans(cbind(mape[["forecast"]]["upper"], mse[["forecast"]]["upper"],
                           theil[["forecast"]]["upper"], bias[["forecast"]]["upper"],
                           log.lik[["forecast"]]["upper"], c.i[["forecast"]]["upper"],
                           r.2[["forecast"]]["upper"], trend[["forecast"]]["upper"]))

    forecast.mean <- cbind(rowMeans(forecast.price), linf, lsup, lsup -
                             rowMeans(forecast.price))
    forecast.mean <- as.data.frame(forecast.mean)
    colnames(forecast.mean) <- c("Forecast", "linf", "lsup", "CI")


  }
  #creacion de las graficas
  mat.plot <- cbind(c(vec$y[1:n,region], rep(NA, length.fore)), c(rep(NA, n - 1),
                      vec$y[n], forecast.mean[,"Forecast"]),
                    c(rep(NA, n), linf), c(rep(NA, n), lsup))
  mat.plot <- as.data.frame(mat.plot)
  colnames(mat.plot) <- c(region, "Pronostico", "Limite Inferior", "Limite Superior")
  mat.plot.exog <- scale(cbind(c(endog.mat[,"Costos"], fore.costos), c(endog.mat[,"Monetario"], fore.monetario),
                               c(endog.mat[,"Demanda"], fore.demanda)))
  mat.plot.exog <- as.data.frame(mat.plot.exog)
  colnames(mat.plot.exog) <- c("Costos", "Monetario", "Demanda")
  mat.graph <- as.data.frame(vec$y)
  seq.graph <- c(nrow(mat.graph) - show.data - length.fore  + 1): nrow(mat.graph)
  mat.plot <- mat.plot[seq.graph,]
  #####
  name.mes <- substring(as.character(seq(as.Date("2005/01/01"),
                                         by = "month", length = nrow(mat.graph))), 3, 7)
  name.mes  <- name.mes[seq.graph]
  temp <- as.data.frame(mat.plot)
  temp$tiempo <- ymd(paste0('20',name.mes,'-01'))
  temp$color <- 'indianred4'
  temp$color[temp$tiempo<= (max(temp$tiempo) - months(length.fore+10))] <- 'indianred4'
  extra <- as.data.frame(precios.row[, region])
  names(extra) <- 'value'
  extra$tiempo <- ymd(row.names(precios.row))
  temp2 <- temp[,c('Limite Inferior', 'Limite Superior', "color", 'tiempo') ]
  temp2$color <- 'khaki4'
  temp <- temp[,c(region, "Pronostico" , "color", 'tiempo') ]
  temp <- melt(temp, id=c('color', 'tiempo'))
  temp2 <- melt(temp2, id=c('color', 'tiempo'))
  index <- which(as.character(temp2$variable)=='Limite Inferior')
  temp2.1 <- temp2[index, ]
  names(temp2.1) <- c('color', 'tiempo', 'variable', 'value.inf' )
  temp2.2 <- temp2[-index,]
  temp2 <- cbind(temp2.1, temp2.2$value)
  names(temp2) <- c(names(temp2)[1:4], 'value')
  seq.forecast <- c(nrow(mat.plot) - length.fore + 1) : nrow(mat.plot)
  media <- mean(c(mat.plot[,region],mat.plot[seq.forecast, "Pronostico"]), na.rm = TRUE)
  sdd <-  sd(c(mat.plot[,region],mat.plot[seq.forecast, "Pronostico"]), na.rm = TRUE)
  sectores <- data.frame(Costos=mat.plot.exog[, "Costos"]*sdd + media,
                         Monetario=mat.plot.exog[, "Monetario"]*sdd + media,
                         Demanda=mat.plot.exog[, "Demanda"]*sdd + media)
  sectores$tiempo <- mes.first + months(0:(nrow(sectores)-1))
  sectores <- melt(sectores, id='tiempo')
  # construccion d ela grafica
  p.hist <- ggplot(data=extra,  aes(x=tiempo, y=value, color='Historico'  ))+
          geom_line(size=1.5)+
          geom_line(data=subset(temp, as.character(variable)!=region),
                    aes(x=tiempo, y=value, color='Pronostico'), lty=2, size=1.5)+
          #geom_line(data=temp2.1, aes(x=tiempo, y=value.inf, color='Lim. inf 95%', size=0.1))+
          #geom_line(data=temp2.2, aes(x=tiempo, y=value, color= 'Lim. sup 95%', size=0.1)) +
          geom_line(data=temp2, aes(x = tiempo, y=value.inf, color='IC 95%'))+
          geom_line(data=temp2, aes(x = tiempo, y=value, color='IC 95%')) + 
          scale_fill_discrete(guide=FALSE)+
          scale_color_manual(
              values=c(   "lightblue",   'orange', 'purple' ),
              labels=c(  'Historico' , 'I.C. 95%', 'Pronostico' ))+
          xlim(c(ymd('2005-01-01'), max(c(extra$tiempo, temp$tiempo, temp2$tiempo))))+
          theme_minimal()+
          guides( size = FALSE, fill=FALSE, alpha=FALSE) + labs(color= "") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.title = element_blank())+
          ylab('') + xlab('Fecha') + ylim(range(extra$value))
      # escritura en disco de la grafica
      dir.create(paste0(dt.file, "BinariosMax/"))
      setwd(paste0(dt.file,'BinariosMax/' ))
      save(p.hist, file=paste0( 'GGplotpronostico_fecha_corteMes_ ',
                                month(mes.last ), ' anio_', year(mes.last ),
                                'numero_de_meses_pronostico_', length.fore, " region ", region,  '.Rdata' ))
      p.hist <- ggplotly(p.hist, tooltip = c('x','y'), dynamicTicks = TRUE )
      p.hist <- p.hist %>% config(collaborate=FALSE , displaylogo = FALSE) %>%
          layout(legend = list(orientation = 'h'))
      save(p.hist, file=paste0( 'pronostico_fecha_corteMes_ ',
                               month(mes.last ), ' anio_', year(mes.last ),
                               'numero_de_meses_pronostico_', length.fore, " region ", region,  '.Rdata' ))
      # termina escritura en disco
      save(forecast.price,
           file=paste0('Inflacionpronostico_metricas_desempenio_mes_', month((mes.last)), ' anio_', year((mes.last)), 'numero_de_meses_pronostico_', length.fore, '.Rdata'))
      forecast.mean <- as.data.frame(forecast.mean)
      forecast.mean$Fecha <-   as.character(tail(temp, length.fore)$tiempo)
      index <- match(forecast.mean$Fecha , row.names(precios.row))
      forecast.mean$Real <- precios.row[index, region]
      # calculo del error para la tabla del dashboard
      forecast.mean$Error <- 100*(1- abs(forecast.mean$Real -  forecast.mean$Forecast)/forecast.mean$Real)
      colnames(forecast.mean) <- c('Pronostico', 'LimInf', 'LimSup', 'SemiLongitudIC', 'Fecha', 'Real', 'Precision%')
      row.names(forecast.mean) <- NULL
      forecast.mean <- forecast.mean[, c('Fecha', 'Pronostico', 'LimInf', 'LimSup', 'SemiLongitudIC',  'Real', 'Precision%')]
      #redondeo a decimales
      forecast.mean[, c('Pronostico', 'LimInf', 'LimSup', 'SemiLongitudIC',  'Real', 'Precision%')] <-
          round(forecast.mean[,c('Pronostico', 'LimInf', 'LimSup', 'SemiLongitudIC',  'Real', 'Precision%') ], 2)
      forecast.mean$Fecha <- substr(forecast.mean$Fecha, 1, 7)
      row.names(forecast.mean) <- forecast.mean$Fecha
      forecast.mean$Fecha <- NULL
      save(forecast.mean,
           file=paste0('pronostico_intervalosConfi_mes_', month((mes.last)), ' anio_', year((mes.last)), 'numero_de_meses_pronostico_', length.fore,
                       " region ", region, '.Rdata'))
      #####
      write.csv(forecast.price,
                file=paste0('Inflacionpronostico_metricas_desempenio_mes_', month((mes.last)), ' anio_', year((mes.last)), 'numero_de_meses_pronostico_', length.fore,' region ', gsub('\\.', '_', region),
                            '.csv'))
  return(list(forecast.price, forecast.mean))
}
