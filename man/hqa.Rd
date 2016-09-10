\name{hqa}
\alias{hqa}

\title{Combination of Qualitative Harmonic and Multiple Factor Analyses and Clustering}
\description{It realizes the combination between two methods for the processing of longitudinal 
categorical data: the Qualitative Harmonic and the Multiple Factorial Analysis.
}
\usage{
hqa(base, conteos=FALSE, units=NULL, durat=FALSE, periodos=NULL, pesos = NULL,
    ilustra=NULL,ilustc = NULL,ilust.type = NULL, nfact=5, nfcl=5, k.clust=NULL, 
    combinat=TRUE, vector, tableclass=FALSE, clasifica=TRUE)
}
\arguments{
  \item{base}{object of type data frame or matrix}
  \item{conteos}{TRUE if you want to do data frame ID,MOD,DURATION. Default TRUE}
  \item{units}{time: "secs", "mins", "hours", "days", "weeks", "months", "years". Default = NULL}
  \item{durat}{TRUE if you want to calculate the DURATION by the function.  Default FALSE}
  \item{periodos}{a vector containing the duration of each period of time}  
  \item{pesos}{a vector of row weights}
  \item{ilustra}{object of type data frame or matrix with the illustrative variables. Default NULL}
  \item{ilustc}{a vector containing the number of variables in each ilustrative group}
  \item{ilust.type}{the type of variable in each ilustrative group: "c" for quantitative variables, "s" for quantitative 
        variables scales to unit variance, "n" for qualitative variables. By default all variables are qualitative}
  \item{nfact}{number of axes to use into the factorial analysis . Default nfact=5}
  \item{nfcl}{number of axes to use in the classification. Default nfcl=5}
  \item{k.clust}{number of classes to work. Default k.clust= NULL}
  \item{combinat}{TRUE if you want to do combination HQA and MFA, FALSE if you want to do only AAC. Default TRUE)}
  \item{vector}{a vector containing the number of categories for each fuzzy variable}
  \item{tableclass}{TRUE if you want a function to Suggest you the number of axes to use in the classificacion. Default FALSE}
  \item{clasifica}{TRUE if you want to do the classificacion. Default TRUE}
}
\details{A new statistical methodology is proposed in order to analyze longitudinal categorical data. This
 methodology considers the use of two methods: Qualitative Harmonic and Multiple Factor Analysis. 
The analysis is complemented by an analysis of classification using the first coordinates factoriales of the data.
 
}
\value{
 \item{HQA}{An object of type dudi or MFA}  
  \item{Clases}{An object of class "kmeans"}  
  \item{Active}{characterization of the classes considering the longitudinal active variable}
  \item{Ilust}{characterization of the classes considering the ilustratives variables}  
}
\author{Martha Lucia Corrales <martha.corrales@usa.edu.co> & Campo Elias Pardo <cepardot@unal.edu.co>}
\references{Corrales, M. L., & Pardo, C. E. (2015). Analisis de datos longitudinales cualitativos con analisis de correspondencias y clasificacion. Comunicaciones en Estadistica, 8(1), 11-32.
\url{http://dx.doi.org/10.15332/s2027-3355.2015.0001.01}
}
\examples{
#data(ratingTV)
#rating <- hqa(base=ratingTV$tab,ilustra=ratingTV$ilus, #vector=c(15,15,15,15,15,15), ilustc=c(4))
#10
#rating$HQA
#rating$Clases
#rating$Active
#rating$Ilust
}
\keyword{qualitative harmonic analysis}
\keyword{multiple factor analysis}
\keyword{fuzzy correspondence analysis}
\keyword{longitudinal variables}
\keyword{cluster analysis}