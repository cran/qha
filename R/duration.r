###############################################################################################################
## Funcion duration                                                                                          ##
##                                                                                                           ##
## Elaborado por: Martha Luc?a Corrales                                                                      ##
## Revisado y modificado por: Campo El?as Pardo INGL?S Fecha                                                 ##
## Universidad Nacional de Colombia                                                                          ##
##                                                                                                           ##
## duration (datos = a data frame containing ID, Start-Time,End-Time, Modality of the Longitudinal Variable, ##
##           units = "secs", "mins", "hours", "days", "weeks", "months", "years")                            ##
##                                                                                                           ##
###############################################################################################################

duration <- function(datos, units = "auto"){

## unidades es : "secs", "mins", "hours", "days", "weeks", "months", "years"
# # datos es la tabla de individuos con ID, START-TIME, END-TIME, MODALIDAD
# verificar la estructura de la tabla.

if (ncol(datos) != 4) {
    stop("Number of columns of datos must be 4 (ID, Start-Time,End-Time, Modality of the Longitudinal Variable)\n")
  }


 ini <- strptime(datos[, 2], format = "%d/%m/%Y %H:%M:%S")
 fin <- strptime(datos[, 3], format = "%d/%m/%Y %H:%M:%S")
 duration <- round(as.numeric(difftime(fin, ini, units = units)),0)
 
 
 if(units == "months"){
   duration <- difftime(fin, ini, units = "days")
   duration <- as.integer((duration/30))
 }
 if(units == "years"){
   duration <- difftime(fin, ini, units = "days")
   duration <- as.integer((duration/365))
 }


 datos <- data.frame(id = datos[, 1], mod = factor(datos[, 4]), duration)
 return(datos)
 
}
