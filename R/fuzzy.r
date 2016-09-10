##########################################################################################################
## Funcion fuzzy                                                                                        ##
##                                                                                                      ##
## Elaborado por: Martha Luc?a Corrales                                                                 ##
## Revisado y modificado por: Campo El?as Pardo INGL?S Fecha                                            ##
## Universidad Nacional de Colombia                                                                     ##
##                                                                                                      ##
## requiere: ade4                                                                                       ##
##                                                                                                      ##
##   fuzzy (datos = a dataframe containing positive values (counts or proportions),                     ##
##      vectorc  = a vector containing the number of categories for each fuzzy variable                 ##
##      nf = an integer indicating the number of kept axes                                              ##
##      pesos = a vector of row weights                                                                 ##
##########################################################################################################

fuzzy<-function(datos,vectorc, nf=5, pesos=NULL)
{
#controles de la funcion
duracionTotal <- sum(vectorc)
if (ncol(datos) != duracionTotal) {
    stop("Sum of periods does not equal the units total duration\n")
}

#Analisis de correspondencias difuso
#datos es la matriz de datos  que va a ser analizada
#vector es una lista que contiene las modalidades de la variable difusa por cada periodo
#esta funcion necesita el paquete ade4
#Regresa un objeto tipo dudi llamado fuzzya
#require(ade4)
#require(FactoClass)
w<-prep.fuzzy.var (datos,vectorc,row.w=pesos) 
fuzzya<-dudi.fca(w, scannf = TRUE, nf) 
return(fuzzya)
}
