\name{catprob}
\alias{catprob}
\title{Category Probability Plots}
\usage{
  catprob(pair_obj, itemnumber = 1, ra = 4, plot = TRUE,
    ...)
}
\arguments{
  \item{pair_obj}{an object of class \code{"pair"} as a
  result from function \code{\link{pair}}.}

  \item{itemnumber}{an integer, defining the number of the
  item to plot the respective category probability for.
  This is set to an arbitrary default value of
  \code{itemnumber = 1} to avoid error messages when you
  forget to choose an item to plot the expected score
  curves for.}

  \item{ra}{an integer, defining the (logit) range for
  x-axis}

  \item{plot}{a logical (default \code{plot = TRUE}),
  defining wether to supress plotting an just return a
  matrix of category probabilities}

  \item{...}{arguments passed to plot}
}
\value{
  a plot or a matrix with category probabilities.
}
\description{
  plotting function for plotting category probability
  curves.
}
\details{
  no details in the moment.
}
\examples{
########
data(sim200x3)
result <- pair(sim200x3)
catprob(pair_obj = result, itemnumber = 2 )
data(bfiN)
result <- pair(bfiN)
catprob(pair_obj = result, itemnumber = 3 )
}

