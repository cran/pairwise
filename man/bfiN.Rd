\name{bfiN}
\alias{bfiN}
\docType{data}
\title{5 polytomous personality items
}
\description{
Data from 2800 subjects answering to 5 neuroticism items with 6 answer categories (0-5) of the \code{bfi} dataset originally included in the R-package \code{{psych}} - see \url{https://cran.r-project.org/package=psych}.
}
\usage{
  data(bfiN)
}

\format{
  A \code{"data.frame"} containing 5 variables and 2800 obsevations.
}

\details{
The other variables from the original \code{bfi} dataset were skipped and the categories are 'downcoded' to '0,1,2,3,4,5' to have a simple, ready to use example data frame. For further Information on the original dataset see R-package \code{{psych}}. 

The category meanings (after downcoding) are as follows:

\describe{
    \item{\code{score 0}}{Very Inaccurate}
    \item{\code{score 1}}{Moderately Inaccurate}
    \item{\code{score 2}}{Slightly Inaccurate}
    \item{\code{score 3}}{Slightly Accurate}
    \item{\code{score 4}}{Moderately Accurate}
    \item{\code{score 5}}{Very Accurate}
}

The Item meanings are as follows:
\describe{
    \item{\code{N1}}{Get angry easily.}
    \item{\code{N2}}{Get irritated easily.}
    \item{\code{N3}}{Have frequent mood swings.}
    \item{\code{N4}}{Often feel blue.}
    \item{\code{N5}}{Panic easily.}
}

The covariates like gender, education and age are in a seperate dataset \code{cov_bfi}

}
\source{
\href{https://cran.r-project.org/package=psych}{https://cran.r-project.org/package=psych}
}

\references{
Revelle, William (2015), psych: Procedures for Psychological, Psychometric, and Personality Research.\emph{R package version 1.5.1}
}

\examples{
data(bfiN)
dim(bfiN)
##############################################################
names(bfiN) # show all variable names of data.frame bfiN
range(bfiN,na.rm=TRUE) # checking the valid response range

}

\keyword{datasets}
