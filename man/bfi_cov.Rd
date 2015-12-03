\name{bfi_cov}
\alias{bfi_cov}
\docType{data}
\title{Covariates to the bfiN Data
}
\description{
Covaraites to the data from 2800 subjects answering to 5 neuroticism items of the bfi dataset originally included in the R-package \code{{psych}} - see \url{https://cran.r-project.org/package=psych}.
}
\usage{
  data(bfi_cov)
}

\format{
  A \code{"data.frame"} containing 3 variables (gender, education, and age) for 2800 obsevations.
}

\details{
The covariates are in the same row (person) order as the responses to the 5 neuroticism items in the seperate datasets \code{\link{bfiN}} and \code{\link{bfiN_miss}}.
The coding is as follows:
\describe{
   \item{\code{gender}}{Males = 1, Females =2}
   \item{\code{education}}{1 = HS, 2 = finished HS, 3 = some college, 4 = college graduate 5 = graduate degree}
   \item{\code{age}}{age in years}
}

}
\source{
\href{https://cran.r-project.org/package=psych}{https://cran.r-project.org/package=psych}
}

\references{
Revelle, William (2015), psych: Procedures for Psychological, Psychometric, and Personality Research.\emph{R package version 1.5.1}
}

\examples{
data(bfi_cov)
dim(bfi_cov)
##############################################################
names(bfi_cov) # show all variable names of data
}

\keyword{datasets}
