\name{bfiN_miss}
\alias{bfiN_miss}
\docType{data}
\title{5 polytomous personality items
}
\description{
Data from 2800 subjects answering to 5 neuroticism items with 6 answer categories (0-5) of the bfi dataset originally included in the R-package \code{{psych}} with artificial missing data (see details) .
}
\usage{
  data(bfiN_miss)
}

\format{
  A data frame containing 5 variables and 2800 obsevations.
}

\details{
This dataset is the same like the dataset \code{{bfiN}} included in this package, exept for the amount of missing data, which were additional created in that way, having aprox. 15\% missing for each of the 5 variables by random.

The other variables from the original bfi dataset were skipped to have a simple example data frame. For further Information on the original dataset see R-package \code{{psych}}.
}
\source{
\href{http://cran.r-project.org/web/packages/psych/index.html}{http://cran.r-project.org/web/packages/psych/index.html}
}

\references{
Revelle, William (2012), psych: Procedures for Psychological, Psychometric, and Personality Research.\emph{R package version 1.2.12}
}

\examples{
data(bfiN_miss)
dim(bfiN_miss)
##############################################################
names(bfiN_miss) # show all variable names of data.frame bfiN_miss
range(bfiN_miss,na.rm=TRUE) # checking the valid response range
colSums(is.na(bfiN_miss))/dim(bfiN_miss)[1] # percentage of missing per variable
}

\keyword{datasets}
