\name{combination}
\alias{combination}
\title{Combination of Qualitative Harmonic and Multiple Factor Analyses}
\description{

It realizes the combination among the  Qualitative Harmonic Analysis and the  
Multiply Factorial Analysis for the treatment of longitudinal qualitative variables.
}
\usage{
combination(datos, vectorc, ilustra = NULL, ilustc=NULL, ilust.type=NULL,pesos=NULL)    
}
\arguments{
  \item{datos}{ object of type dataframe or matrix containing positive values (counts or proportions)  }
  \item{vectorc}{a vector containing the number of categories for each fuzzy variable}
  \item{ilustra}{object of type matrix or data frame with the illustrative variables .Default = NULL)}
  \item{ilustc}{a vector containing the number of variables in each ilustrative group}
  \item{ilust.type}{the type of variable in each ilustrative group: "c" for quantitative variables, 
                   "s" for quantitative variables scales to unit variance, "n" for qualitative  variables. By default 
                    all variables are qualitative) }   
  \item{pesos}{a vector containing the weights of the rows}               
}
\details{
The AAC presented by the first time in 1979 by Deville and Saporta, is an exploratory method of longitudinal categorical 
information which aim is to construct the information of the individuals of a sample, during a certain period of time.
These individuals are described by the chronology of a succession of changes of stages among a set of possible stages. 
Deville and Saporta demonstrated that information of this type can be analyzed of form similar to what would make 
in a process escalar (harmonic analysis).

When in the AAC it is chosen for a recodification that indicates the proportion of time that an individual remains in 
a category, with regard to a specific period of time, there is obtained a table that can turn as a table of 
diffuse codification, that is to say, the individuals take different values of the same variable with different 
degrees of association; and this one can be analyzed across an extension of the analysis of multiple correspondences: 
the diffuse analysis of correspondences, proposed by Chevenet,  Doledec and  Chessel in 1994. 

The analysis factorial multiple appears as a method factorial for the description of the same set of individuals across 
several groups of variables. In the longitudinal studies, the groups are formed according to the moment of the time in 
which the variables have been measured. It is obtained then, a table with combinations of transverse cuts in the time.

The function combination, considers the use of both methods to analyze longitudinal categorical data.

}
\value{
 Returns the individuals factor map, the variables factor map and the groups factor map.
 The function combination returns an object type MFA of the package FactoMineR.  
}
\author{Martha Lucia Corrales <martha.corrales@usa.edu.co> & Campo Elias Pardo <cepardot@unal.edu.co>}
\examples{
# data(ratingTV)
# AAC_FMA<-combination(datos=ratingTV$tab,vectorc=c(15,15,15,15,15,15),
#        ilustra=ratingTV$ilus,ilustc=c(4))
#10
#AAC_FMA$separate.analyses
#AAC_FMA$eig
#AAC_FMA$group
#AAC_FMA$summary.quali
#AAC_FMA$partial.axes
}
\keyword{qualitative harmonic analysis}
\keyword{multiple factor analysis}
\keyword{fuzzy correspondence analysis}
