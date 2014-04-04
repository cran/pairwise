\name{sim200x3}
\alias{sim200x3}
\docType{data}
\title{Simulated Data
}
\description{
Simulated data for 200 'subjects' 'answering' to 3 items with unequal number of categories -- one dichotomous and two polytoumous items.
}
\usage{
  data(sim200x3)
}

\format{
  A data.frame containing 3 variables and 200 obsevations.
}

\details{
This simulated data is used as an example in the rasch module of the 'ALMO - Statistiksystem'. 
}
\source{
\href{http://www.almo-statistik.de/}{http://www.almo-statistik.de/}
}

\references{
To come ...
}

\examples{
data(sim200x3)
dim(sim200x3)
##############################################################
apply(sim200x3,2,table)
}

\keyword{datasets}
