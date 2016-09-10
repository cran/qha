##########################################################################################################
## Funcion de enlace: Combinaci?n del AAC y el AFM                                                      ##
##                                                                                                      ##
## Elaborado por: Martha Luc?a Corrales                                                                 ##
## Revisado y modificado por: Campo El?as Pardo INGL?S Fecha                                            ##
## Universidad Nacional de Colombia                                                                     ##
##                                                                                                      ##
## requiere:ade4,FactoMineR,FactoClass   library(ade4)                                                  ##
##                     library(FactoMineR)                                                              ##
##                     library(FactoClass)                                                              ##
##   hqa ( base = a data frame with individuals and 3 or 4 columns                                      ##
##      with the longitudinal active variable.                                                          ##
##      La estructura de base es ID,MOD,DURATION or                                                     ##
##                   ID, START-TIME, END-TIME, MODALIDAD                                                ## 
##      o la tabla ya con los conteos realizados                                                        ##
##      conteos = boolean, if TRUE do data frame ID,MOD,DURATION (default TRUE)                         ##
##      vector  = a vector containing the number of categories for each fuzzy variable                  ##
##      ilustra = data frame with the illustrative variables (deafault NULL)                            ##
##      ilustc =  a vector containing the number of variables in each ilustrative group                 ##
##      ilustype = tipo de variables que contiene                                                       ##
##      nfact  = number of axes to use into the factorial analysis  (by default 5)                      ##
##      nfcl  = number of dimensions for the classification(default 5 )                                 ## 
##      k.clust = number of class (default NULL)                                                        ##
##      combinat = boolean, if TRUE do combination HQA and MFA (default TRUE)                           ##
##      durat = boolean, if TRUE calculate duration by the function (default FALSE)                     ##
##      units = time: "secs", "mins", "hours", "days", "weeks", "months",                               ##
##           "years" (default = NULL)                                                                   ##                         
##      periodos = a vector containing the duration of each period of time                              ##
##      tableclass = boolean, if TRUE Suggests the number of axes to realize the classificacion         ##
##      clasifica = boolean, if TRUE realizes the classificacion                                        ##
##      pesos = a vector of row weights  (default NULL)                                                 ##   
##########################################################################################################


hqa <- function(base, conteos=FALSE, units=NULL, 
                durat=FALSE, periodos=NULL, pesos = NULL,
                ilustra=NULL, ilustc = NULL,ilust.type = NULL, 
                nfact=5, nfcl=5, k.clust=NULL, combinat=TRUE, 
                vector, tableclass=FALSE, clasifica=TRUE)

{

#require(FactoClass)
#require(FactoMineR)
#require(ade4)


## Controles de la funcion
# base que no se encuentre en blanco
if(!is.matrix(base) & !is.data.frame(base)) {
	stop("base is not a matrix nor a dataframe\n")
}

#if(is.matrix(base) & is.null(dim(base[complete.cases(base)]))) {
#	stop("base is an empty matrix\n")
#}

if(is.data.frame(base) & prod(dim(base))==0) {
	stop("base is an empty data frame\n")
}


if(durat==TRUE)
 { #### Calcular la duracion en la modalidad de la variable longitudinal
   datos <- duration(base, units)
   base  <- datos
 }


if(conteos==TRUE)
 { #### Calcular la tabla de conteos
   conteos <- durationtotableA(base, periodos)
   base    <- data.frame(conteos$Conteo)
 }


if(combinat==TRUE)
 {   #### Realiza la combinacion del analisis armonico cualitativo y el analisis factorial multiple
    # AAC es un objeto tipo MFA
 	AAC   <- combination(base, vector, ilustra, ilustc, ilust.type, pesos)
 	tabla <- AAC$global.pca$ind$coord
	#Para calcular el numero de ejes
	#solo para la combinacion
	nval.propios  <- nrow(AAC$eig) 
	val.propios  <- AAC$eig[,1]
	dif.valores  <- c(rep(0, nval.propios-1))
	for (i in 1:nval.propios-1) 
	  { dif.valores[i]  <- abs(val.propios[i+1]-val.propios[i])}
		nfcl  <- which(dif.valores==max(dif.valores))
 } else { ## Realiza solamente el an?lisis armonico cualitativo
	AAC  <- fuzzy(base, vector, nfact, pesos)
	## AAC es un objeto tipo dudi
	nfcl  <- dim(AAC$li)[2]
	tabla  <- AAC$li
}

if(clasifica==TRUE)
{



if(is.null(k.clust)) {
# Clasificacion inicial para tomar el numero de clases
coo  <- tabla[, 1:nfcl]
W <- dist(coo)^2/nrow(coo)/2
HW <- hclust(W,method="ward.D")
coord  <- as.vector(HW$height)
dif.saltos  <- c(rep(0, length(coord)-1))
for (i in 1:length(coord)-1) dif.saltos[i] <- abs(coord[i+1]-coord[i])
	
k.clust  <- which(dif.saltos==max(dif.saltos))
k.clust  <- length(coord) - k.clust + 1
}

#### Sugiere el numero de ejes para realizar la clasificacion
if(tableclass==TRUE)
 {   
 tablaclases  <- tableclass(tabla, nfcl, k.clust)  
 nfcl  <- tablaclases[which(tablaclases[,2]== min(tablaclases[,2])), 1] 

}


# Analisis de clasificacion
clasificar <- classification(tabla, k.clust, nfcl,pesos)

#Para las var ilustrativas
if (!is.null(ilustra)) Ilust <- cluster.carac(ilustra, clasificar$cluster, tipo.v="n") else Ilust <- NULL
Active <- cluster.carac(base, clasificar$cluster, tipo.v="co")

# Graficos
#Para la variable longitudinal 
#windows()
#s.class(coor,as.factor(factorc),cellipse=0)

} else {
clasificar <- NULL
Active <- NULL
Ilust <- NULL
}

return(list(HQA=AAC, Clases=clasificar, Active = Active, Ilust = Ilust))
}
####################################################################################################
