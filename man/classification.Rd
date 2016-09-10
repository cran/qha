\name{classification}
\alias{classification}
\title{Cluster Analysis}
\description{It makes cluster analysis of the data using the nfcl first factorial coordinates 
  }
\usage{
classification(datos,numclass=2,numaxes=5,pesos=NULL)    
}
\arguments{
  \item{datos}{object of type data frame or matrix with coordinates of factorial analysis}
  \item{numclass}{number of class. Default 2}
  \item{numaxes}{number of dimensions for the classification. Default 5}
  \item{pesos}{a vector of row weights. Default NULL}
    
}
\details{
The data given by datos is clustered by the Ward method using the function ward.cluster of the package FactoClass. 
This classification is optimized across the method kmeans.    
}
\value{
An object of class "kmeans", a list with components: cluster, centers, withinss and size.
To more information see the function kmeans.
  
}
\author{Martha Lucia Corrales <martha.corrales@usa.edu.co> & Campo Elias Pardo <cepardot@unal.edu.co>}

\examples{
data(ratingTV)
 

# Fuzzy Correspondence Analysis 
fuzzyb<-fuzzy(datos=ratingTV$tab, vectorc=c(15,15,15,15,15,15))  
6

#Cluster analysis
clases<-classification(datos=fuzzyb$li,numclass=7, numaxes=6)
clases$cluster
clases$centers
clases$size

}
\keyword{cluster analysis}
\keyword{kmeans method}
\keyword{ward method}
