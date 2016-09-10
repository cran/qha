\name{duration}
\alias{duration}
\title{Calculate of duration time}
\description{
 It calculates the duration time for every individual in the category of the longitudinal variable 
}
\usage{
duration(datos, units = "auto")
}
\arguments{
  \item{datos}{a data frame containing ID, Start-Time,End-Time, Modality of the Longitudinal Variable}
  \item{units}{unis time:"secs", "mins", "hours", "days", "weeks", "months", "years"}
    
}
\details{
 It calculates the duration time to the table containing ID, START_TIME,END_TIME,MODALIDAD  
}
\value{
 \item{datos}{a data frame containing ID, Modality of the Longitudinal Variable, Duration time}   
}
\author{Martha Lucia Corrales <martha.corrales@usa.edu.co> & Campo Elias Pardo <cepardot@unal.edu.co>}
\keyword{periods of time}


