\name{Neoffi5}
\alias{Neoffi5}
\docType{data}
\title{Polytomous example data in Rost 2004 
}
\description{
Data for 1000 subjects answering to 5 polytomous items assessing neuroticism contained in the german version of the NEO--five--factor--inventory (NEOFFI) by Borkenau and Ostendorf (1991). This data is used as an example in the textbook by J. Rost (2004) to demonstrate some principles of rasch measurement. 
}
\usage{
  data(Neoffi5)
}

\format{
  A \code{"matrix"} containing 5 columns (variables) and 1000 rows (obsevations).
}

\details{
An detailed description of the data can be found in Rost (2004) at page 202.  
}

\references{
  Rost, J. (2004). \emph{Lehrbuch Testtheorie - Testkonstruktion} (2 nd Ed.) Huber: Bern.
  
  Borkenau. P. & Ostendorf F. (1991). Ein Fragebogen zur Erfassung fünf robuster Persönlichkeitsfaktoren. \emph{Diagnostica, 37}, (1), 29--41.
}

\examples{
data(Neoffi5)
dim(Neoffi5)
###########
# frequencies
ftab(Neoffi5)
# Itemparameter to be compared with Rost (2004), page 211.
summary(pair(Neoffi5)) 
# Itemparameter to be compared with Rost (2004), page 213.
summary(pers(pair(Neoffi5))) 
}

\keyword{datasets}
