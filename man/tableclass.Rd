\name{tableclass}
\alias{tableclass}

\title{Correspondence Analysis Factorial Planes}
\description{
It suggests the number of axes to using in a cluster analysis 
}
\usage{ 
tableclass(tabla,numaxes=2,numclass=2)

}
\arguments{
  \item{tabla}{object of type data frame or matrix with coordinates of factorial analysis}
  \item{numaxes}{number of dimensions for the classification. Default 2}
  \item{numclass}{number of class. Default 2}
    
}
\details{
To understand the differences between elements of differents class,  the planes factoriales are insufficient. 
It is necessary, therefore, to proceed to the classification of the individuals in the vectorial space of the first 
factors of the analysis. 
To decide the number of axes to using, Barbary (1996) proposed a procedure which consists of realizing the classification 
several times changing the number of axes and using as criterion the number of individuals of the class with major quantity
of individuals. The analysis to interpreting is that one that originates the minor number of individuals in the majority class.
  
The function tableclass realizes the classification several times y it proposes a number of axes to the classification.
}
\value{
 \item{tableclas}{object of type matrix}  
}
\author{Martha Lucia Corrales <martha.corrales@usa.edu.co> & Campo Elias Pardo <cepardot@unal.edu.co>}
\examples{
data(ratingTV)

# Fuzzy Correspondence Analysis 
fuzzyb<-fuzzy(datos=ratingTV$tab, vectorc=c(15,15,15,15,15,15))  
20

tabla_clases<-tableclass(fuzzyb$li, numaxes=4, numclass=7)
tabla_clases
}
\keyword{cluster analysis}

