###############################################################################################################
## Funcion tableclass                                                                                        ##
##                                                                                                           ##
## Elaborado por: Martha Luc?a Corrales                                                                      ##
## Revisado y modificado por: Campo El?as Pardo INGL?S Fecha                                                 ##
## Universidad Nacional de Colombia                                                                          ##
##                                                                                                           ##
## tableclass (tabla = a data frame or matrix containing  coordinates of a factorial analysis,               ##
##           numaxes =  number of dimensions for the classification. Default 2,                              ##
##           numclass =  number of class. Default 2 )                                                        ##
##                                                                                                           ##
###############################################################################################################


tableclass<-function(tabla,numaxes=2,numclass=2)
{

## Verificacion de los datos
  

  if (numaxes < 2) {
    stop("Number of axes must to be 2 or greater 2")
  }
  
 if (numclass < 2) {
    stop("Number of classes must to be 2 or greater 2")
  }

 if (ncol(tabla) < numaxes) {
    stop("Number of columns of tabla must to be greater than number of axes")
  }


##
#-----------------
  # modificado por PardoCE ago 30 2016
 # numrows<-as.integer(numaxes+15)
 # if (ncol(tabla) < numrows) {numrows = ncol(tabla)-numaxes}
  numrows = ncol(tabla)-numaxes+1
#-----------------

  tableclas<-matrix(0,numrows,2)
  for(k in 1:numrows)
    {
      coo<-tabla[,1:numaxes]
      W <- dist(coo)^2/nrow(coo)/2
      HW <- hclust(W,method="ward.D")
      memb <- cutree(HW, k = numclass)
      tableclas[k,1]<-numaxes
      tableclas[k,2]<-sort(table(memb))[length(table(memb))]
      numaxes<-numaxes+1
}
colnames(tableclas)<-c("Axes","Individuals in the majority class")
Chosen <- tableclas[ ,2] == min(tableclas[ ,2])
tableclas <- cbind(tableclas, Chosen)
return(tableclas)
}


