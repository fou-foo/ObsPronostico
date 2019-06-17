
## Cointegration relationship, common trends of long run into variables
cointegration <- function(johansenTest)
{
  if(!(class(johansenTest) == "ca.jo"))
  {
      stop("\nPlease, provide object of class 'ca.jo' as 'z'.\n")
  }
  if(johansenTest@P == nrow(johansenTest@V))
  {
    ci <- johansenTest@x%*%johansenTest@V
  }else{
    ci <- johansenTest@x%*%johansenTest@V[-c(johansenTest@P + 1),]
   }
  return(ci)
}

## HP filter
hpfilter <-function(x,lambda=1600)
{
  eye <-diag(length(x))
  result <-solve(eye+lambda*crossprod(diff(eye,lag=1,d=2)),x)
  return(result)
}

## Forecast variance - covariance matrix in models VAR
fecov <- function(x, n.ahead) {
  sigma.u <- crossprod(resid(x))/(x$obs - ncol(x$datamat[, -c(1:x$K)]))
  Sigma.yh <- array(NA, dim = c(x$K, x$K, n.ahead))
  Sigma.yh[, , 1] <- sigma.u
  Phi <- Phi(x, nstep = n.ahead)
  if (n.ahead > 1) {
    for (i in 2:n.ahead) {
      temp <- matrix(0, nrow = x$K, ncol = x$K)
      for (j in 2:i) {
        temp <- temp + Phi[, , j] %*% sigma.u %*% t(Phi[, , j])
      }
      Sigma.yh[, , i] <- temp + Sigma.yh[, , 1]
    }
  }
  return(Sigma.yh)
}

## Mean Squared Error Matrix for models VAR or VEC
mseVAR <- function(x, n.ahead = 3)
{
    if(class(x) == "varest")
    msey <- fecov(x, n.ahead = n.ahead)

    if(class(x) == "vec2var")
    msey <-fecovvec2var(x, n.ahead = n.ahead)

    n.ahead <- abs(as.integer(n.ahead))
    K <- x$K
    p <- x$p
    ynames <- colnames(x$datamat[, 1:K])
     mse <- matrix(NA, nrow = n.ahead, ncol = K)
    for (i in 1:n.ahead)
      mse[i, ] <- diag(msey[, , i])
      colnames(mse) <- colnames(x$y)

    return(mse)
}

## Stability forecast
stability.fore <- function(object, n.ahead = 3, exogNEW = NULL)
{
  if(!(class(object) == "varest") && !(class(object) == "vec2var")) {
        stop("\nPlease, provide object of class 'varest' as 'object'.\n")
  }

  final.y <- object$y[nrow(object$y),]
  matFore <- matrix(0, ncol = ncol(object$y), nrow = n.ahead)
  if(class(object) == "varest"){
    for(i in 1 : ncol(object$y))
      matFore[,i] <- predict(object, n.ahead = n.ahead, dumvar = exogNEW)[[1]][[i]][,1]
  }


  matFore <- apply(rbind(final.y, matFore), 2, diff)

  if(n.ahead == 1){
  f.stat <- nrow(object$y)*t(matFore)%*%solve(fecov(object, n.ahead)[,,1])%*%matFore/((nrow(object$y)+object$K*object$p+1)*object$K*1)}else{
  f.stat <- 0
  ind.n.ahead <- c(1:n.ahead)
  for(i in 1 : n.ahead){
    f.stat <- f.stat + nrow(object$y)*t(matFore[i,])%*%solve(fecov(object, n.ahead)[,,i])%*%matFore[i,]/((nrow(object$y)+object$K*object$p+1)*object$K*ind.n.ahead[i])}}

  p.value <- pf(f.stat, object$K*n.ahead, nrow(object$y)-object$K*object$p-1, F, F)
  options(warn = -1)

  result <- list(object$K*n.ahead,nrow(object$y)-object$K*object$p-1, c(f.stat), c(p.value))
  options(warn = -1)
  names(result) <- c("df1","df2","f.stat","p.value")
  return(result)
}

## Integral Forecast
integral.forecast <- function(v.forecast, sd.forecast, hh, c.sig)
{
  forecast.matrix <- matrix(0, hh, 3)
  colnames(forecast.matrix) <- c("Forecast", "LI", "LS")
  mat.mean <- matrix(0, ncol = 3, nrow = length(v.forecast))
  colnames(mat.mean) <- colnames(v.forecast[[1]])
  for(j in 1 : 3){
  for(i in 1 : length(v.forecast)){
  mat.mean[i,j] <- mean(v.forecast[[i]][,j])}}


  for(k in 1 : 3){
  for(j in 1 : hh){
  forecast.optim <- function(v.mean)
  {
    distance    <- abs(v.forecast[[j]][,k] - v.mean)/sd.forecast[j,]
    center.prob <- pnorm(distance) - 0.50
    return(sum(center.prob^2))
  }

  v.mean               <- mat.mean[j,k]
  if(sum(v.mean < 0) == 0){
    forecast.matrix[j,k] <- optimize(forecast.optim, lower = (1-c.sig)*v.mean, upper = (1+c.sig)*v.mean)$minimum}else{
    forecast.matrix[j,k] <- optimize(forecast.optim, lower = (1+c.sig)*v.mean, upper = (1-c.sig)*v.mean)$minimum}}}

  return(forecast.matrix)
}

## Multivariate Outliers
mult.outliers <- function(object, c.sig = 0.05)
{
    # Entradas:
        # object (vec2var): modelo VAR proveniente de un MCE
        # c.sig (numeric): Significancia
    # Salidas:
        # result (list de matrices): "mat.out", "p.values" (p-valor del test ji-cuadrado para observaciones outliers), "out.var" (bandera de outlier)
  resid.object <- scale(resid(object))
  var.cov <- round(cov(resid.object), 4) #covarianza entre las series
  resid.scale <- scale(resid(object), center = TRUE, FALSE) #residuos escalados
  # deteccion de ouliers USANDO UNA TECNICA DE LA CONFERENCIA DE LA UNIVERSIDAD DE SALAMANCA
  mat.out <- resid.scale%*%solve(cov(resid.scale))%*%t(resid.scale) # PORQUE SOBRE LOS RESIDUOS ESCALADOS??
  test.out <- diag(mat.out)
  p.values <- 1 - matrix(pchisq(test.out, ncol(resid.object))) # implicitamente asumen normalidad??
  out.var  <- matrix(0, nrow = nrow(resid.object), ncol = 1)
  out.var[p.values < c.sig,1] <- 1 # MEJORAR ESTA LINEA
  result <- list(round(test.out,4),round(p.values,4),out.var)
  names(result) <- c("mat.out", "p.values", "out.var")
  return(result)
}


## Calcular la bondad de ajuste
r2.model <- function(object, num.eq = 1){
  lm.object <- object$varresult[[num.eq]]
  u   <- resid(lm.object)
  n   <- length(u)
  fit <- fitted(lm.object)
  dat <- fit + u
  stc <- sum((dat-mean(dat))^2)
  sec <- sum((fit-mean(fit))^2)
  r2  <- sec/stc
  g.l <- n - length(coef(lm.object))
  r2.adj <- 1-(1-r2)*((n-1)/(g.l))
  result <- list(r2, r2.adj)
  names(result) <- c("r2", "r2.adj")
  return(result)
}

##
predict.vec2var <- function (object, ..., n.ahead = 10, ci = 0.95, dumvar = NULL, mat.res = NULL)
{
    if(!is.null(mat.res)){
      if(any(colnames(mat.res) != colnames(object$y)))
        stop("Please check the names of the matrix 'mat.res'")
    }

    if(!is.null(mat.res)){
      if(nrow(mat.res) != n.ahead)
        stop(" The nrow the 'mat.res' is different the n.ahead selected")
    }
    n.ahead <- as.integer(n.ahead)
    K <- object$K
    p <- object$p
    obs <- object$obs
    data.all <- object$datamat
    ynames <- colnames(object$y)
    Z <- object$datamat[, -c(1:K)]
    B <- object$deterministic
    for (i in 1:object$p) {
        B <- cbind(B, object$A[[i]])
    }
    Zdet <- matrix(rep(1, n.ahead), nrow = n.ahead, ncol = 1)
    rownames(Zdet) <- seq(nrow(data.all) + 1, length = n.ahead)
    if (eval(object$vecm@ecdet) == "trend") {
        trendf <- seq(obs + p, length = n.ahead)
        Zdet <- cbind(Zdet, trendf)
    }
    if (!is.null(eval(object$vecm@season))) {
        season <- eval(object$vecm@season)
        seas.names <- paste("sd", 1:(season - 1), sep = "")
        cycle <- tail(data.all[, seas.names], season)
        seasonal <- matrix(cycle, nrow = season, ncol = season -
            1)
        if (nrow(seasonal) >= n.ahead) {
            seasonal <- matrix(cycle[1:n.ahead, ], nrow = n.ahead,
                ncol = season - 1)
        }
        else {
            while (nrow(seasonal) < n.ahead) {
                seasonal <- rbind(seasonal, cycle)
            }
            seasonal <- seasonal[1:n.ahead, ]
        }
        rownames(seasonal) <- seq(nrow(data.all) + 1, length = n.ahead)
        Zdet <- cbind(Zdet, seasonal)
    }
    if (!is.null(eval(object$vecm@dumvar))) {
        if (is.null(dumvar)) {
            stop(paste("\nPlease, provide a matrix x for argument 'dumvar' with",
                n.ahead, "rows.\n", sep = " "))
        }
        if (!identical(nrow(dumvar), n.ahead)) {
            stop("\nNumber of rows of 'dumvar' is not equal to 'n.ahead'.\n")
        }
        testsum <- sum((colnames(dumvar) %in% colnames(B)))
        if (!(testsum == ncol(dumvar))) {
            stop("\nColumn names of 'dumvar' do not match with column names in 'object$datamat'.\n")
        }
        Zdet <- cbind(Zdet, dumvar)
    }
    exogen.cols <- which(colnames(data.all) %in% colnames(object$deterministic))
    Zy <- data.all[, -exogen.cols]
    yse <- matrix(NA, nrow = n.ahead, ncol = K)
    sig.y <- fecov(x = object, n.ahead = n.ahead)
    for (i in 1:n.ahead) {
        yse[i, ] <- sqrt(diag(sig.y[, , i]))
    }
    yse <- -1 * qnorm((1 - ci)/2) * yse
    colnames(yse) <- paste(ci, "of", ynames)
    forecast <- matrix(NA, ncol = K, nrow = n.ahead)
    colnames(forecast) <- colnames(object$y)

    lasty <- c(Zy[nrow(Zy), ])
    for (i in 1:n.ahead) {
        lasty <- lasty[1:(K * p)]
        Z <- c(Zdet[i, ], lasty)
        forecast[i, ] <- B %*% Z

        if(!is.null(mat.res))
          forecast[i,names(which(!is.na(mat.res[i,])))] <- mat.res[i ,which(!is.na(mat.res[i,]))]

        temp <- forecast[i, ]
        lasty <- c(temp, lasty)
    }
    colnames(forecast) <- paste(ynames, ".fcst", sep = "")
    lower <- forecast - yse
    colnames(lower) <- paste(ynames, ".lower", sep = "")
    upper <- forecast + yse
    colnames(upper) <- paste(ynames, ".upper", sep = "")
    forecasts <- list()
    for (i in 1:K) {
        forecasts[[i]] <- cbind(forecast[, i], lower[, i], upper[,
            i], yse[, i])
        colnames(forecasts[[i]]) <- c("fcst", "lower", "upper",
            "CI")
    }
    names(forecasts) <- ynames
    result <- list(fcst = forecasts, endog = object$y, model = object,
        exo.fcst = dumvar)
    class(result) <- "varprd"
    return(result)
}


##
predict.varest <- function (object, ..., n.ahead = 10, ci = 0.95, dumvar = NULL, mat.res = NULL)
{

  if(!is.null(mat.res)){
    if(any(colnames(mat.res) != colnames(object$y)))
      stop("Please check the names of the matrix 'mat.res'")
  }

  if(!is.null(mat.res)){
    if(nrow(mat.res) != n.ahead)
      stop(" The nrow the 'mat.res' is different the n.ahead selected")
  }

    K <- object$K
    p <- object$p
    obs <- object$obs
    type <- object$type
    data.all <- object$datamat
    ynames <- colnames(object$y)
    n.ahead <- as.integer(n.ahead)
    Z <- object$datamat[, -c(1:K)]
    B <- Bcoef(object)
    if (type == "const") {
        Zdet <- matrix(rep(1, n.ahead), nrow = n.ahead, ncol = 1)
        colnames(Zdet) <- "const"
    }else if (type == "trend") {
        trdstart <- nrow(Z) + 1 + p
        Zdet <- matrix(seq(trdstart, length = n.ahead), nrow = n.ahead,
            ncol = 1)
        colnames(Zdet) <- "trend"
    }else if (type == "both") {
        trdstart <- nrow(Z) + 1 + p
        Zdet <- matrix(c(rep(1, n.ahead), seq(trdstart, length = n.ahead)),
            nrow = n.ahead, ncol = 2)
        colnames(Zdet) <- c("const", "trend")
    }else if (type == "none") {
        Zdet <- NULL
    }
    if (!is.null(eval(object$call$season))) {
        season <- eval(object$call$season)
        seas.names <- paste("sd", 1:(season - 1), sep = "")
        cycle <- tail(data.all[, seas.names], season)
        seasonal <- as.matrix(cycle, nrow = season, ncol = season -
            1)
        if (nrow(seasonal) >= n.ahead) {
            seasonal <- as.matrix(cycle[1:n.ahead, ], nrow = n.ahead,
                ncol = season - 1)
        }else {
            while (nrow(seasonal) < n.ahead) {
                seasonal <- rbind(seasonal, cycle)
            }
            seasonal <- seasonal[1:n.ahead, ]
        }
        rownames(seasonal) <- seq(nrow(data.all) + 1, length = n.ahead)
        if (!is.null(Zdet)) {
            Zdet <- as.matrix(cbind(Zdet, seasonal))
        }else {
            Zdet <- as.matrix(seasonal)
        }
    }
    if (!is.null(eval(object$call$exogen))) {
        if (is.null(dumvar)) {
            stop("\nNo matrix for dumvar supplied, but object varest contains exogenous variables.\n")
        }
        if (!all(colnames(dumvar) %in% colnames(data.all))) {
            stop("\nColumn names of dumvar do not coincide with exogen.\n")
        }
        if (!identical(nrow(dumvar), n.ahead)) {
            stop("\nRow number of dumvar is unequal to n.ahead.\n")
        }
        if (!is.null(Zdet)) {
            Zdet <- as.matrix(cbind(Zdet, dumvar))
        }else {
            Zdet <- as.matrix(dumvar)
        }
    }
    Zy <- as.matrix(object$datamat[, 1:(K * (p + 1))])
    yse <- matrix(NA, nrow = n.ahead, ncol = K)
    sig.y <- fecov(x = object, n.ahead = n.ahead)
    for (i in 1:n.ahead) {
        yse[i, ] <- sqrt(diag(sig.y[, , i]))
    }
    yse <- -1 * qnorm((1 - ci)/2) * yse
    colnames(yse) <- paste(ci, "of", ynames)
    forecast <- matrix(NA, ncol = K, nrow = n.ahead)
    colnames(forecast) <- colnames(object$y)
    lasty <- c(Zy[nrow(Zy), ])


    for (i in 1:n.ahead){
        lasty <- lasty[1:(K * p)]
        Z <- c(lasty, Zdet[i, ])
        forecast[i, ] <- B %*% Z
        temp <- forecast[i, ]

        if(!is.null(mat.res))
          forecast[i,names(which(!is.na(mat.res[i,])))] <- mat.res[i ,which(!is.na(mat.res[i,]))]

        lasty <- c(temp, lasty)
    }

    colnames(forecast) <- paste(ynames, ".fcst", sep = "")
    lower <- forecast - yse
    colnames(lower) <- paste(ynames, ".lower", sep = "")
    upper <- forecast + yse
    colnames(upper) <- paste(ynames, ".upper", sep = "")
    forecasts <- list()
    for (i in 1:K) {
        forecasts[[i]] <- cbind(forecast[, i], lower[, i], upper[,
            i], yse[, i])
        colnames(forecasts[[i]]) <- c("fcst", "lower", "upper",
            "CI")
    }
    names(forecasts) <- ynames
    result <- list(fcst = forecasts, endog = object$y, model = object,
        exo.fcst = dumvar)
    class(result) <- "varprd"
    return(result)
}

predict.manual <- function (object, ..., n.ahead = 10, ci = 0.95, dumvar = NULL, mat.res = NULL)
{

  if(!is.null(mat.res)){
    if(any(colnames(mat.res) != colnames(object$y)))
      stop("Please check the names of the matrix 'mat.res'")
  }

  if(!is.null(mat.res)){
    if(nrow(mat.res) != n.ahead)
      stop(" The nrow the 'mat.res' is different the n.ahead selected")
  }

    K <- object$K
    p <- object$p
    obs <- object$obs
    type <- object$type
    data.all <- object$datamat
    ynames <- colnames(object$y)
    n.ahead <- as.integer(n.ahead)
    Z <- object$datamat[, -c(1:K)]
    B <- Bcoef(object)
    if (type == "const") {
        Zdet <- matrix(rep(1, n.ahead), nrow = n.ahead, ncol = 1)
        colnames(Zdet) <- "const"
    }else if (type == "trend") {
        trdstart <- nrow(Z) + 1 + p
        Zdet <- matrix(seq(trdstart, length = n.ahead), nrow = n.ahead,
            ncol = 1)
        colnames(Zdet) <- "trend"
    }else if (type == "both") {
        trdstart <- nrow(Z) + 1 + p
        Zdet <- matrix(c(rep(1, n.ahead), seq(trdstart, length = n.ahead)),
            nrow = n.ahead, ncol = 2)
        colnames(Zdet) <- c("const", "trend")
    }else if (type == "none") {
        Zdet <- NULL
    }
    if (!is.null(eval(object$call$season))) {
        season <- eval(object$call$season)
        seas.names <- paste("sd", 1:(season - 1), sep = "")
        cycle <- tail(data.all[, seas.names], season)
        seasonal <- as.matrix(cycle, nrow = season, ncol = season -
            1)
        if (nrow(seasonal) >= n.ahead) {
            seasonal <- as.matrix(cycle[1:n.ahead, ], nrow = n.ahead,
                ncol = season - 1)
        }else {
            while (nrow(seasonal) < n.ahead) {
                seasonal <- rbind(seasonal, cycle)
            }
            seasonal <- seasonal[1:n.ahead, ]
        }
        rownames(seasonal) <- seq(nrow(data.all) + 1, length = n.ahead)
        if (!is.null(Zdet)) {
            Zdet <- as.matrix(cbind(Zdet, seasonal))
        }else {
            Zdet <- as.matrix(seasonal)
        }
    }
    if (!is.null(eval(object$call$exogen))) {
        if (is.null(dumvar)) {
            stop("\nNo matrix for dumvar supplied, but object varest contains exogenous variables.\n")
        }
        if (!all(colnames(dumvar) %in% colnames(data.all))) {
            stop("\nColumn names of dumvar do not coincide with exogen.\n")
        }
        if (!identical(nrow(dumvar), n.ahead)) {
            stop("\nRow number of dumvar is unequal to n.ahead.\n")
        }
        if (!is.null(Zdet)) {
            Zdet <- as.matrix(cbind(Zdet, dumvar))
        }else {
            Zdet <- as.matrix(dumvar)
        }
    }
    Zy <- as.matrix(object$datamat[, 1:(K * (p + 1))])
    yse <- matrix(NA, nrow = n.ahead, ncol = K)
    sig.y <- fecov(x = object, n.ahead = n.ahead)
    for (i in 1:n.ahead) {
        yse[i, ] <- sqrt(diag(sig.y[, , i]))
    }
    yse <- -1 * qnorm((1 - ci)/2) * yse
    colnames(yse) <- paste(ci, "of", ynames)
    forecast <- matrix(NA, ncol = K, nrow = n.ahead)
    colnames(forecast) <- colnames(object$y)
    lasty <- c(Zy[nrow(Zy), ])


    for (i in 1:n.ahead){
        lasty <- lasty[1:(K * p)]
        Z <- c(lasty, Zdet[i, ])
        forecast[i, ] <- B %*% Z
        temp <- forecast[i, ]

        if(!is.null(mat.res))
          forecast[i,names(which(!is.na(mat.res[i,])))] <- mat.res[i ,which(!is.na(mat.res[i,]))]

        lasty <- c(temp, lasty)
    }

    colnames(forecast) <- paste(ynames, ".fcst", sep = "")
    lower <- forecast - yse
    colnames(lower) <- paste(ynames, ".lower", sep = "")
    upper <- forecast + yse
    colnames(upper) <- paste(ynames, ".upper", sep = "")
    forecasts <- list()
    for (i in 1:K) {
        forecasts[[i]] <- cbind(forecast[, i], lower[, i], upper[,
            i], yse[, i])
        colnames(forecasts[[i]]) <- c("fcst", "lower", "upper",
            "CI")
    }
    names(forecasts) <- ynames
    result <- list(fcst = forecasts, endog = object$y, model = object,
        exo.fcst = dumvar)
    class(result) <- "varprd"
    return(result)
}


## Forecast variance - covariance matrix in models VEC
fecovvec2var <-function(x, n.ahead) {
  sigma.u <- crossprod(resid(x))/x$obs
  Sigma.yh <- array(NA, dim = c(x$K, x$K, n.ahead))
  Sigma.yh[, , 1] <- sigma.u
  Phi <- Phi(x, nstep = n.ahead)
  if (n.ahead > 1) {
    for (i in 2:n.ahead) {
      temp <- matrix(0, nrow = x$K, ncol = x$K)
      for (j in 2:i) {
        temp <- temp + Phi[, , j] %*% sigma.u %*% t(Phi[, , j])
      }
      Sigma.yh[, , i] <- temp + Sigma.yh[, , 1]
    }
  }
  return(Sigma.yh)
}


idx.mean <- function(y){
  bp.idx <- ur.za(y)@bpoint
  n <- length(y)
  i <- 1
  list.bp <- c()
  list.bp[i] <- bp.idx
  test.bp <- NULL
  while(class(test.bp) != "try-error"){
    y.d <- y[bp.idx:n]
    test.bp <- try(ur.za(y.d)@bpoint, TRUE)
    if(class(test.bp) != "try-error"){
      bp.idx <- bp.idx + test.bp
      i <- i + 1
      list.bp[i] <- bp.idx
    }
  }

  list.bp <- c(1, list.bp - 1, list.bp, n)[order(c(1, list.bp - 1, list.bp, n))]
  idx.1 <- seq(1, length(list.bp), 2)
  idx.2 <- seq(2, length(list.bp), 2)

  list.mean <- sapply(1:length(idx.1), function(x) mean(y[list.bp[idx.1[x]:idx.2[x]]]))
  result <- list(list.bp, list.mean)
  names(result) <- c("list.bp", "list.mean")
  return(result)
}

factor.pls <- function(y, x, xreg = NULL, c.var = 0.80)
{
    # Entradas:
        #   y, x (data.frame): Y (variable a predecir, INPC), X variables endogeneas
        # c.var (numeric): proporcion de varianza explicada a retener
    # Salida:
        # lista con "load.mat", "load.mat.w", "score.pls"
        # "load.mat" (matrix): Matriz de loadings de las componentes utiles
        # "load.mat.w" (matrix): Matriz de loadings de las componentes utiles ponderadas
        # "score.pls" (matrix): Matriz con los scores de las componentes oncadenado con los pronosticos hechos con estos
    y <- as.matrix(y)
    x <- as.matrix(x)
    m.pls <- plsr(log(y) ~ scale(x)) # siempre jala bien usar el log ??
    w.pls <- m.pls$Xvar/sum(m.pls$Xvar) # pesos de las componenetes
    w.pls <- w.pls[1: which(cumsum(w.pls) > c.var)[1]] #retencion de las primeras componentes hasta tener 'c.var' de varianza muestral
    w.pls <- w.pls/sum(w.pls) # normalizacion de los pesos
        # en la siguiente linea se seleccionan solo las componentes utiles y se castean a 'matrix' para poder usar la implementacion de crossvalidation
    load.mat <- matrix(m.pls$loading.weights[,1:length(w.pls), drop = FALSE], ncol = length(w.pls))
    rownames(load.mat) <- colnames(x)
    score.pls <<- matrix(rowSums(sapply(1 : length(w.pls),
                                        function(i) m.pls$scores[, i, drop=FALSE] * w.pls[i])))
    n <- nrow(y)
    if(n > nrow(score.pls))
    {
        # caso en que los scores sean mayores a las observaciones ~debe de haber un error
        # aqui hay UN ERROR 'creg' ES NULO
        # se intenta pronosticar con  los scores
        fore.score <- forecast(auto.arima(score.pls,
                                          xreg = xreg[1: nrow(score.pls), , drop = FALSE]),
                    newxreg = xreg[c(nrow(score.pls) + 1): n, , drop = FALSE],
                    h = n -  nrow(score.pls))$mean
        score.pls <<- matrix(c(score.pls, fore.score)) # se concadenan 'scores' d ela regresion y el pronostico
    }
    if(nrow(load.mat) > 1)
    {
        # si hay mas de una carga se ponderan en funcion de la imporatncia d elas componentes
        load.mat.w <- matrix(rowSums(sapply(1 : length(w.pls), function(x) load.mat[, x, drop=FALSE] * w.pls[x])))
        rownames(load.mat.w) <- rownames(load.mat)
    }else{
        load.mat.w <- load.mat
    }
    result <- list(load.mat, load.mat.w, score.pls)
    names(result) <- c("load.mat", "load.mat.w", "score.pls")
    return(result)
}

#####
# Permanent and Transitory decomposition Gonzalo-Granger(1995)
# P = A1%*%t(gamma.c)%*%f[t] & T = A2%*%Z[t]
# x is a object with class ca.jo (cointegration johansen) the package 'ur.ca'
# this function estimates the cointegration rank with a 5%, according to
# specification 'eigen' or 'trace' in 'ca.jo' object. before
# (the use the gon.gra function) the user must make the exercise cointegration
# or indicate in 'x' the 'lags', seasonality, exogenous variables, etc. Please,
# check the function 'ca.jo'
# the output is a list with important results into P-T decomposition
#####

gon.gra <- function(x){
  # Cointegration object: Johansen VECM
  if(class(x)!= "ca.jo")
    stop("Please provide an object of class 'ca.jo'")
  # Propierties of data matrix
  y <- x@x
  N <- nrow(y)
  p <- ncol(y)
  # Range of cointegration maxtri PI = gamma%*%t(alpha)
  x.cval <- x@cval[,"5pct"]
  x.teststat <- x@teststat
  r <- p - sum((x.teststat < x.cval)[which(x.teststat < x.cval)])
  # r = p: VAR(p) model
  if(r == p)
    stop(paste("This model is a representation VAR(p) because the rank of",
      "cointegration matrix is r"))
  # r = 0: VAR(p-1) model with diff(y) [first differences]
  if(r == 0)
    stop(paste("This model is a representation VAR(p-1) in first differences",
      "because the rank of cointegration matrix is null"))
  # alpha and gamma matrix according the rank of cointegration matrix
  alpha <- x@V[1:p,1:r,drop=FALSE]
  gamma <- x@W[,1:r,drop=FALSE]
  # orthogonal complements for alpha and gamma according p = r + p,
  # gamma.c = (m[r+1],...m[rk])(for example), ie eigenvectors normalized
  # according t(M)%*%S00%*%M
  alpha.c <- eigen(alpha%*%t(alpha))$vectors[,c(r+1):p,drop=FALSE]
  # matrix in levels, differences, and lags levels
  Z0 <- x@Z0
  Z1 <- x@Z1
  ZK <- x@ZK
  # cross product for the regressions
  M00 <- crossprod(Z0)/N
  M11 <- crossprod(Z1)/N
  MKK <- crossprod(ZK)/N
  M01 <- crossprod(Z0, Z1)/N
  M0K <- crossprod(Z0, ZK)/N
  MK0 <- crossprod(ZK, Z0)/N
  M10 <- crossprod(Z1, Z0)/N
  M1K <- crossprod(Z1, ZK)/N
  MK1 <- crossprod(ZK, Z1)/N
  M11inv <- solve(M11)
  # residuals (see Johansen 1988)
  S00 <- M00 - M01 %*% M11inv %*% M10
  S0K <- M0K - M01 %*% M11inv %*% M1K
  SK0 <- MK0 - MK1 %*% M11inv %*% M10
  SKK <- MKK - MK1 %*% M11inv %*% M1K
  # solve equation for estimate gamma.c
  Ctemp <- chol(S00, pivot = TRUE)
  pivot <- attr(Ctemp, "pivot")
  oo <- order(pivot)
  C <- t(Ctemp[, oo])
  Cinv <- solve(C)
  SKKinv <- solve(SKK)
  # equation 23, page 30
  valeigen <- eigen(Cinv %*% S0K %*% SKKinv %*% SK0 %*% t(Cinv))
  lambda <- valeigen$values
  # matrix eigenvalues
  p <- length(lambda)
  L <- matrix(0, p, p)
  diag(L) <- lambda
  # abs(round(L%*%S00 - S0K %*% SKKinv %*% SK0, 4)) = 0
  mat.0 <- abs(round(L%*%S00 - S0K %*% SKKinv %*% SK0))
  # eigen vectors: estimator of gamma.c
  e <- valeigen$vector
  M <- t(Cinv) %*% e
  # gamma.c
  gamma.c <- M[,c(r+1):p,drop=FALSE]
  # restriction of normalization
  Imat <- round(t(M)%*%S00%*%M)
  # page 29. decomposition P - T proposed
  A1 <- alpha.c%*%solve(t(gamma.c)%*%alpha.c)
  A2 <- gamma%*%solve(t(alpha)%*%gamma)
  # common factor and long run matrix
  ft <- sapply(1:N, function(x) t(gamma.c)%*%t(y[x,,drop=FALSE]))
  zt <- sapply(1:N, function(x) t(alpha)%*%t(y[x,,drop=FALSE]))
  # finally, matrix P (permanent) and T (transitory)
  P <- t(A1%*%ft)
  T <- t(A2%*%zt)
  # output attributes
  rownames(alpha.c) <- rownames(A1) <- rownames(alpha)
  colnames(alpha) <- colnames(gamma) <- colnames(A2) <- colnames(y)[1:r]
  colnames(alpha.c) <- colnames(gamma.c) <- colnames(A1) <-
    colnames(y)[c(r+1):p]
  ft <-  matrix(ft, N, p - r)
  colnames(ft) <- sapply(1 : ncol(ft), function(x) paste0("Common.factor.", x))
  zt <- matrix(zt, N, r)
  colnames(zt) <- sapply(1 : ncol(zt), function(x) paste0("Cointegration.", x))
  colnames(P) <- paste("P.", colnames(y))
  colnames(T) <- paste("T.", colnames(y))
  # output: list of items
  result <- list(alpha, gamma, alpha.c, gamma.c, A1, A2, r, lambda, S00, SKK,
              S0K, SKKinv, SK0, mat.0, Imat, ft, zt, P, T)
  names(result) <- c("alpha", "gamma", "alpha.c", "gamma.c", "A1", "A2", "r",
                    "lambda", "S00", "SKK", "S0K", "SKKinv", "SK0", "mat.0",
                    "Imat","ft","zt","P", "T")
  # class object
  class(result) <- "gon.gra"
  return(result)
}

###
# Function about restrictions over gamma.c (log likelihood ratio test)
# z: object the 'gon.gra' class
# G: restriction matrix. Must have number of columns and rows appropiate
# Estimate the equation 29  THEOREM 3
###
test.gamma.c <- function(z, G){
  # object the correct class
  if(class(z)!= "gon.gra")
    stop("Please provide an object the class 'gon.gra'")

  # N, rank and p columns
  N <- nrow(z$T)
  r <- z$r
  p <- ncol(z$T)

  # restriction of specification incorrect the 'G' matrix
  if(ncol(G)!= p - r || nrow(G) != p)
    stop("The dimension of 'G' is incorrect, please check this detail")

  # valus of restrict (equation 29)
  lambda <- z$lambda
  S00 <- z$S00
  SKK <- z$SKK
  S0K <- z$S0K
  SKKinv <- z$SKKinv
  SK0 <- z$SK0

  # solve equation for estimate gamma.c
  Ctemp <- chol(t(G) %*% S00 %*%G, pivot = TRUE)
  pivot <- attr(Ctemp, "pivot")
  oo <- order(pivot)
  C <- t(Ctemp[, oo])
  Cinv <- solve(C)
  SKKinv <- solve(SKK)

  # equation 29  (Theorem 3)
  valeigen <- eigen(Cinv %*% t(G)%*%S0K %*% SKKinv %*% SK0%*%G %*% t(Cinv))
  lambda.res <- valeigen$values

  # estimate teta
  M4b <- G%*%t(Cinv)%*%valeigen$vectors

  # t(M4b)%*%t(G)%*%S00%*%G%*%M4b = I
  Imat <- round(t(M4b)%*%S00%*%M4b)

  # matrix eigenvalues
  L <- matrix(0, p - r, p - r)
  diag(L) <- lambda.res

  # abs(round(L%*%t(G)%*%S00%*%G - t(G)%*%S0K %*% SKKinv %*% SK0%*%G, 4)) = 0
  mat.0 <- round(abs(L%*%t(G)%*%S00%*%G - t(G)%*%S0K %*% SKKinv %*% SK0%*%G))

  # test equation 32
  teststat <- -N * sum(log((1 - lambda.res)/(1 - lambda[c(r+1):p])))
  df <- c(p - r)*c(p - ncol(G))

  # p.value
  pval <- dchisq(teststat, df) * 2

  # list of results
  result <- list(mat.0, Imat, M4b, pval)
  names(result) <- c("mat.0", "Imat", "M4b", "p.value")

  return(result)
}

# test about cointegration coefficients
test.coint <- function(x, r) {
  names.col <- colnames(x@V)
  vecm.r <- cajorls(x, r)

  list.t <- list()
  m.test <- matrix(NA, length(names.col) - 1, 2)
  colnames(m.test) <- c("t.stat", "p.value")
  rownames(m.test) <- names.col[-1]

  for(j in 1 : r){
    alpha <- coef(vecm.r$rlm)[j,]
    beta.r <- vecm.r$beta

    resids <- resid(vecm.r$rlm)
    N <- nrow(resids)
    sigma  <- crossprod(resids) / N

    beta.se <- sqrt(diag(kronecker(solve(crossprod(x@RK[,-j])), solve(t(alpha)%*%solve(sigma)%*%alpha))))
    m.test[,"t.stat"] <- round(c(NA, beta.r[-1, j] / beta.se), 3)[-1]
    m.test[,"p.value"] <- round(dt(m.test[,"t.stat"], vecm.r$rlm$df), 3)

    list.t[[j]] <- m.test
  }
  return(list.t)
}

inf <- function(x, q){
  inf.x <- c()
  for(i in q : c(length(x) - 1))
    inf.x[i - (q-1)] <- c(x[i + 1]/x[i - (q-1)])*100-100

  return(inf.x)
}
