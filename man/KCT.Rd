\name{KCT}
\alias{KCT}
\docType{data}
\title{Knox Cube Test Data from Wright & Stone (1979)
}
\description{
Data from the Book 'Best Test Design' from Wright & Stone (1979, p. 31, table 2.3.1) comprising responses from 35 subjects scored in 18 dichotomous items.
}
\usage{
  data(KCT)
}

\format{
  A \code{"data.frame"} containing 18 numeric variables (coded 0,1) and 35 obsevations.
}

\details{
The so called 'Knox Cube Test' was initially developed as a cube imitation test around 1913 by Howard A. Knox as a nonverbal test of intelligence to screen and identify potential immigrants with mental deficits at the Ellis Island immigration station in New York Harbor -- see Richardson (2005) for a historical review. 


Quoted from Wright & Stone (1979):

"\emph{Success on this subtest requires the application of visual attention and short-term memory to a simple sequencing task. It appears to be free from school-related tasks and hence to be an indicator of nonverbal intellectual capacity.}" (Wright & Stone 1979, p. 28).
}


\references{
Wright, B. D. & Stone, M. H. (1979). \emph{Best Test Design: Rasch Measurement}. Chicago: MESA Press.

Richardson, J. T. E. (2005). Knox’s cube imitation test: A historical review and an experimental analysis. \emph{Brain and Cognition, 59}(2), 183–213. https://doi.org/10.1016/j.bandc.2005.06.001
}

\examples{
data(KCT)
dim(KCT)
############# some item calibrations ###############
data(KCT)
IP_pair <- pair(daten = KCT[,4:17], m = 2)
summary(IP_pair)
####################################################
####################################################
#########MIKE error message never received##########
####################################################
####################################################
}

\keyword{datasets}
