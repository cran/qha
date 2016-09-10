##########################################################################################################
## Funcion Clasificaci?n de? AAC o de la Combinaci?n del AAC y el AFM                                   ##
##                                                                                                      ##
## Elaborado por: Martha Luc?a Corrales                                                                 ##
## Revisado y modificado por: Campo El?as Pardo INGL?S Fecha                                            ##
## Universidad Nacional de Colombia                                                                     ##
##                                                                                                      ##
## requiere:                                                                                            ##
##                     library(FactoClass)                                                              ##
##   classification ( base = a data frame or matrix with coordinates of factorial analisys              ##
##      numaxes  = number of dimensions for the classification (default 5 )                             ## 
##      numclass = number of class (default 2)                                                          ##
##      pesos = a vector of row weights  (default NULL)                                                 ##   
##########################################################################################################


classification<-function(datos,numclass=2,numaxes=5,pesos=NULL)
{
par(las=1)
#require(FactoClass)

## Controles de la funcion
# datos que no se encuentre en blanco
if(!is.matrix(datos) & !is.data.frame(datos)) {
	stop("base is not a matrix nor a dataframe\n")
}

#if(is.matrix(datos) & is.null(dim(datos[complete.cases(datos)]))) {
#	stop("datos is an empty matrix\n")
#}

if(is.data.frame(datos) & prod(dim(datos))==0) {
	stop("datos is an empty data frame\n")
}



if (numclass < 2) {
    stop("Number of clusters must be >= 2")
  }

if (numaxes < 1) {
    stop("Number of axes must be >= 1")
  }

coo<-datos[,1:numaxes]
 W <- dist(coo)
 HW <- ward.cluster(W,h.clust=1, peso=pesos)
 memb <- cutree(HW, k = numclass)
 
#C?lculo de los centros de las clases a caracterizar
 cent <- NULL
 for(k in 1:numclass){
   cent <- rbind(cent, colMeans(coo[memb == k, , drop = FALSE]))}

#Optimizacion de la particion

if (is.null(pesos)) 
{
 clases<-kmeans(coo,cent)
}

else
{
clases<-kmeansW(coo,cent,weight=pesos)
}

return(clases)
}

