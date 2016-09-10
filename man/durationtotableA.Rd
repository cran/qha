\name{durationtotableA}
\alias{durationtotableA}
\title{Groups by Time Periods}
\description{
It realizes the transformation of the table that contains the durations 
in every modality of the longitudinal variable in the table that it groups for periods of time
  }
\usage{
durationtotableA(x,periodos)    
}
\arguments{
  \item{x}{a data frame containing ID, Modality of the Longitudinal Variable, Duration}
  \item{periodos}{a vector containing the duration of each period time to do a groups}
}

\details{This function transforms the table that contains the durations passed by the individuals 
in every modality of a longitudinal variable, in the table that groups these modalities in periods of time. 
That is to say, in the table that contains the quantity of time
that an individual remains in a category of the longitudinal variable, in every period of time  
  }
\value{
  \item{Conteo}{a data frame with categories by number  of  time  units} 
 
}
\author{Martha Lucia Corrales <martha.corrales@usa.edu.co> & Campo Elias Pardo <cepardot@unal.edu.co>}
\examples{
#data(zonas)
#tableA1<-durationtotableA(zonas$zona1,c(360,240,60,150,160,230))
}


