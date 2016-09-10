###############################################################################################################
## Funcion durationtotableA                                                                                  ##
##                                                                                                           ##
## Elaborado por: Martha Lucía Corrales                                                                      ##
## Revisado y modificado por: Campo Elías Pardo INGLÉS Fecha                                                 ##
## Universidad Nacional de Colombia                                                                          ##
##                                                                                                           ##
## duration (datos = a data frame containing ID, Modality of the Longitudinal Variable,                      ##
##           periodos = a vector containing the duration of each period time                                 ##
##                                                                                                           ##
###############################################################################################################
durationtotableA <- function(x, periodos){
## x es la tabla que tiene ID, MOD, DURATION
# # Esta funcion trasforma la tabla de duracion en cada modalidad
# # en la tabla unidades a unidades antes de agrupar en cada periodo
# # de tiempo y luego agrupa por períodos de tiempo
# # períodos es un vector que contiene los períodos a ser agrupados
canalesvistos <- function(x) {
# # x es un individuo: ID, MOD, DURATION

duracionTotal           <- sum(periodos)
  duracionesIndividuales  <- matrix(by(x[, 3], x[, 1], sum))
  l.var.levels            <- levels(factor(x[, 2]))

  # # Verificacion de los datos
  if (ncol(x) != 3) {
    stop("Number of columns of x must be 3 (ID, MOD, DURATION)\n")
  }

  if (dim(table(duracionesIndividuales)) > 1) {
    stop("All units total duration must be equal\n")
  }
  
  if (as.numeric(names(table(duracionesIndividuales))) != duracionTotal) {
    stop("Sum of periods does not equal the units total duration\n")
  }
  
  # #
  f <- function(y) rep(y[2], y[3])
  unlist(apply(x, 1, f))
}

listacanales <- function(x) {
# # x es la tabla de individuos con ID, MOD, DURATION
  cbind(unique(x[,1]),
  t(sapply(
    by(x, x[,1], canalesvistos),
    rbind
  )))
}
  t1 <- listacanales(x)
  rownames(t1) <- t1[, 1]
  t1 <- t1[, -1]

  agrup <- factor(rep(1:length(periodos), periodos))
  x[, 2] <- as.factor(x[, 2])
  canales <- levels(x[, 2])  # # canal es la segunda columna de x
  conteo <- c()
  for(k in canales){
    temp <- apply(t1, 1, function(x)tapply(x, agrup, function(x)sum(x == k)))
    rownames(temp) <- paste(rownames(temp), k, sep = '.')
    conteo <- rbind(conteo, temp)
  }
  conteo <- t(conteo[order(rownames(conteo)), ])
  return(Conteo = conteo)
}

