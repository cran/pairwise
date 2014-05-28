\name{rfa}
\alias{rfa}
\title{Rasch Residual Factor Analysis}
\usage{
  rfa(pers_obj, na_treat = 0, tr = FALSE,
    use = "complete.obs", res = "stdr", method = "pearson",
    cor = TRUE)
}
\arguments{
  \item{pers_obj}{an object of class \code{"pers"} as a
  result from function \code{\link{pers}}.}

  \item{na_treat}{value to be assigned to residual cells
  which have missing data in the original response matrix.
  default is set to \code{na_treat=0} to set the residuals
  to 0, which implys that they are imputed as 'fitting
  data', i.e., zero residuals. This can attenuate contrasts
  (see. http://www.rasch.org/rmt/rmt142m.htm). An option is
  to set it to \code{na_treat=NA}.}

  \item{tr}{a logical value indicating whether the data
  (the residual matrix) is transposed prior to calculation.
  This would perform a person analysis rather than a item
  analysis. The default is set to item analysis.}

  \item{use}{a character string as used in function
  \code{\link{cor}} or \code{\link{cov}}, giving a method
  for computing covariances or correlations in the presence
  of missing values. This must be (an abbreviation of) one
  of the strings "everything", "all.obs", "complete.obs",
  "na.or.complete", or "pairwise.complete.obs". The default
  is set to \code{use="complete.obs"} which will exclude
  cases by listwise deletion to keep the correlation matrix
  positive definit.}

  \item{res}{a character string defining which type of
  (rasch--) residual to analyze when computing covariances
  or correlations. This must be (exactly) one of the
  strings "sr" for score residuals , "stdr" for
  standardised residuals, "srsq" for score residuals
  squared, or "stdrsq" for standardised residuals squared.
  The default is set to \code{res="stdr"} refering to
  Linacre (1998).}

  \item{method}{a character string as used in function
  \code{\link{cor}} or \code{\link{cov}}, indicating which
  correlation coefficient (or covariance) is to be
  computed. One of "pearson" (default), "kendall", or
  "spearman", can be abbreviated. The default is set to
  \code{method="pearson"}.}

  \item{cor}{a logical value indicating whether the
  calculation should use the correlation matrix or the
  covariance matrix.The default is set to \code{cor=TRUE}
  to use the correlation matrix.}
}
\value{
  An object of class \code{c("rfa","list")}.
}
\description{
  Calculation of the rasch residual factor analysis
  proposed by Wright (1996) and further discussed by
  Linacre (1998) to detect multidimensionality.
}
\details{
  no details in the moment.
}
\examples{
######################
########
data(bfiN) # loading reponse data
pers_obj <- pers(pair(bfiN))
result <- rfa(pers_obj)
summary(result)
plot(result)
####
}
\references{
  Wright, B. D. (1996). Comparing Rasch measurement and
  factor analysis. \emph{Structural Equation Modeling: A
  Multidisciplinary Journal, 3}(1), 3–24.

  Linacre, J. M. (1998). Detecting multidimensionality:
  which residual data-type works best? \emph{Journal of
  outcome measurement, 2}, 266–283.
}

