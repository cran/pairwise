\name{pairwise.item.fit}
\alias{pairwise.item.fit}
\title{Item Fit Indices}
\usage{
  pairwise.item.fit(pers_obj)
}
\arguments{
  \item{pers_obj}{an object of class \code{"pers"} as a
  result from function \code{\link{pers}}}
}
\value{
  an object of class \code{c("pairwise_item_fit",
  "data.frame")} containing item fit indices.
}
\description{
  function for calculating item fit indices. The procedures
  for calculating the fit indices are based on the formulas
  given in Wright & Masters, (1982, P. 100), with further
  clarification given in
  \code{http://www.rasch.org/rmt/rmt34e.htm}.
}
\details{
  contrary to many IRT software using Ml based item
  parameter estimation, \code{pairwise} will not exclude
  persons, showing perfect response vectors (e.g. c(0,0,0)
  for dataset with three variables), prior to the scaling.
  Therefor the fit statistics computed with \code{pairwise}
  may deviate somewhat from the fit statistics produced by
  IRT software using Ml based item parameter estimation
  (e.g. R-package \code{eRm}), depending on the amount of
  persons with perfect response vectors in the data.
}
\examples{
########
data(sim200x3)
result <- pers(pair(sim200x3))
pairwise.item.fit(pers_obj=result) # item fit statistic
}
\references{
  Wright, B. D., & Masters, G. N. (1982). \emph{Rating
  Scale Analysis.} Chicago: MESA Press.
}

