##########################################################################################################
## Funcion Combinaci?n del AAC y el AFM                                                                 ##
##                                                                                                      ##
## Elaborado por: Martha Luc?a Corrales                                                                 ##
## Revisado y modificado por: Campo El?as Pardo INGL?S Fecha                                            ##
## Universidad Nacional de Colombia                                                                     ##
##                                                                                                      ##
## requiere: ade4,FactoMineR,FactoClass   library(ade4)                                                 ##
##                     library(FactoMineR)                                                              ##
##                     library(FactoClass)                                                              ##
##   combination <- function(datos, vectorc, ilustra = NULL, ilustc=NULL, ilust.type=NULL)              ##
##      datos = a dataframe or matrixcontaining positive values (counts or proportions)                 ##
##      vectorc  = a vector containing the number of categories for each fuzzy variable                 ##
##      ilustra = a data frame or matrix with the illustrative variables (default NULL)                 ##
##      ilustc = a vector containing the number of variables in each ilustrative group                  ##
##      ilustype = the type of variable in each ilustrative group: "c" for quantitative variables,      ##
##                 "s" for quantitative variables scales to unit variance, "n" for qualitative          ##
##                  variables. By default all variables are qualitative                                 ##
##      pesos =                                                                                         ##
##########################################################################################################


combination <- function(datos, vectorc, ilustra = NULL, ilustc=NULL, ilust.type=NULL, pesos=NULL){
#controles de la funcion
duracionTotal <- sum(vectorc)
if (ncol(datos) != duracionTotal) {
    stop("Sum of periods does not equal the units total duration\n")
}

#necesita las  librerias ade4 y FactoMineR
#require(ade4)
#require(FactoMineR)
#require(FactoClass)

# Realiza el analisis de correspondencias difuso

w <- prep.fuzzy.var (datos, vectorc,pesos)
fuzzya <- dudi.fca(w, scannf = TRUE, nf = 2) 
barplot(fuzzya$eig, main="Fuzzy Analysis Scree Plot", col = "lightblue")

matrizAFM <- (rep(1,nrow(datos))%*%t(sqrt(fuzzya$cw))) * fuzzya$tab

if (is.null(ilustra)) 
{
var.act <- length(vectorc)
AnalFactM = MFA(matrizAFM, group=vectorc, type=c(rep("c", var.act)))
}
else {

if (is.null(ilustc)) ilustc <- ncol(ilustra)
if (is.null(ilust.type)) ilust.type <- "n"

archivod <- cbind(matrizAFM, ilustra)
vectorAFM <- c(vectorc, ilustc)
var.act <- length(vectorc)
grup.ilus <- length(vectorAFM)
AnalFactM <- MFA(archivod, group=vectorAFM, type=c(rep("c", var.act), ilust.type), num.group.sup=c((var.act + 1) : length(vectorAFM)))
}

return(AnalFactM)
}

