\name{fuzzy}
\alias{fuzzy}
\alias{fuzzy}
\title{Qualitative Harmonic Analysis}
\description{
 It realizes qualitative harmonic analysis like a fuzzy correspondence analysis  
}
\usage{
fuzzy(datos,vectorc,nf=5,pesos=NULL)
}
\arguments{
  \item{datos}{object of type data frame or matrix}
  \item{vectorc}{a vector containing the number of categories for each fuzzy variable }
  \item{nf}{an integer indicating the number of kept axes. Default nf=5} 
  \item{pesos}{a vector of row weights} 
}
\details{
The AAC presented by the first time in 1979 by Deville and Saporta, is an exploratory method of longitudinal categorical 
information which aim is to construct the information of the individuals of a sample, during a certain period of time.
These individuals are described by the chronology of a succession of changes of stages among a set of possible stages. 
Deville and Saporta demonstrated that information of this type can be analyzed of form similar to what would make 
in a process escalar (harmonic analysis).

In its practical application it is the accomplishment of an analysis of correspondences

When in the AAC it is chosen for a recodification that indicates the proportion of time that an individual remains in 
a category, with regard to a specific period of time, there is obtained a table that can turn as a table of 
diffuse codification, that is to say, the individuals take different values of the same variable with different 
degrees of association; and this one can be analyzed across an extension of the analysis of multiple correspondences: 
the diffuse analysis of correspondences, proposed by Chevenet,  Dol?dec and  Chessel in 1994.

   
}
\value{
  \item{fuzzya}{object of type dudi}
}
\author{Martha Lucia Corrales <martha.corrales@usa.edu.co> & Campo Elias Pardo <cepardot@unal.edu.co>}


\examples{

data(ratingTV)

# Fuzzy Correspondence Analysis with weights
fuzzya<-fuzzy(datos=ratingTV$tab, vectorc=c(15,15,15,15,15,15), pesos=ratingTV$weight) 
6


# Fuzzy Correspondence Analysis without weights
fuzzyb<-fuzzy(datos=ratingTV$tab, vectorc=c(15,15,15,15,15,15))  
6

# First factorial plane with FactoClass
#plot(fuzzya,1,2,Tcol=FALSE,ucal=10)
#plot(fuzzyb,1,2,Tcol=FALSE,ucal=10)

}
\keyword{qualitative harmonic analysis}
\keyword{fuzzy correspondence analysis}

