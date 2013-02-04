\name{bfiN}
\alias{bfiN}
\docType{data}
\title{5 polytomous personality items
}
\description{
Data from 2800 subjects answering to 5 neuroticism items with 6 answer categories (0-5) of the bfi dataset originally included in the R-package \code{{psych}}.
}
\usage{
  data(bfiN)
}

\format{
  A data frame containing 5 variables and 2800 obsevations.
}

\details{
The other variables from the original bfi dataset were skipped to have a simple example data frame. For further Information on the original dataset see R-package \code{{psych}}.
}
\source{
\href{http://cran.r-project.org/web/packages/psych/index.html}{http://cran.r-project.org/web/packages/psych/index.html}
}

\references{
Revelle, William (2012), psych: Procedures for Psychological, Psychometric, and Personality Research.\emph{R package version 1.2.12}
}

\examples{
data(bfiN)
dim(bfiN)
##############################################################
names(bfiN) # show all variable names of data.frame bfiN
range(bfiN,na.rm=TRUE) # checking the valid response range

}

\keyword{datasets}
