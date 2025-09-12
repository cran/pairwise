\name{kft5}
\alias{kft5}
\docType{data}
\title{Dichotomous example data in Rost 2004
}
\description{
Data for 300 subjects answering to 5 dichotomous items out of 'Kognitiver Fähigkeits Test' [Cognitive Skills Test] (KFT - Gaedike & Weinläder, 1976) . This data is used as an example in the textbook by J. Rost (2004) to demonstrate some principles of rasch measurement. 
}
\usage{
  data(kft5)
}

\format{
  A \code{"matrix"} containing 5 columns (variables) and 300 rows (obsevations).
}

\details{
The instrument KFT and the data are described in Rost (2004) at page 95.  
}

\references{
  Rost, J. (2004). \emph{Lehrbuch Testtheorie - Testkonstruktion} (2 nd Ed.) Huber: Bern.
  
  Heller, K, Gaedike, A.-K & Weinläder, H. (1976). \emph{Kognitiver Fähigkeits-Test (KFT 4-13)}. Weinheim: Beltz.
}

\examples{
data(kft5)
dim(kft5)
###########
# frequencies
ftab(kft5)
# Itemparameter to be compared with Rost (2004), page 120.
summary(pair(kft5)) 
# Itemparameter to be compared with Rost (2004), page 120.
summary(pers(pair(kft5))) 
}
\keyword{datasets}
