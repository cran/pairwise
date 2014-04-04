\name{ptbis}
\alias{ptbis}
\title{Point Biserial Correlations}
\usage{
  ptbis(y, daten = NULL)
}
\arguments{
  \item{y}{either an object of class \code{"pers"}, or an
  numeric vector as an result of any scaling approach (WLE,
  MLE, Rawscore, etc. ) relating to the Items (columns) in
  \code{daten}.}

  \item{daten}{if argument y is not an object of class
  \code{"pers"}, a \code{data.frame}, potentially with
  missing values, comprising dicho- or polytomous items
  (columns).}
}
\value{
  An object of class "data.frame" and "ptbis" containing
  item statistics.
}
\description{
  Calculation of the point biserial correlations for dicho-
  or polytomous item categories with total scale (person
  parameter).
}
\details{
  to come ...
}
\examples{
######################
########
data(sim200x3) # loading reponse data
y <- rowSums(sim200x3)
ptbis(y=y, daten=sim200x3)
####
result <- pers(pair(sim200x3))
ptbis(y= result)
}

