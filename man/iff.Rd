\name{iff}
\alias{iff}
\title{Item information function}
\usage{
  iff(pair_obj, itemnumber = 1, x = NULL, plot = TRUE,
    cat = FALSE, lwd = 2, col = 1, ...)
}
\arguments{
  \item{pair_obj}{an object of class \code{"pair"} as a
  result from function \code{\link{pair}}.}

  \item{itemnumber}{an integer, defining the number of the
  item to plot the respective item information function
  for. This is set to an arbitrary default value of
  \code{itemnumber = 1} to avoid error messages when you
  forget to choose an item to plot the item information
  function for.}

  \item{x}{The value(s) of the latent variable, at which
  the IIF will be evaluated. \code{x} should be either a
  numeric vector of theta values or a single numeric value.
  If \code{x} is given as a single numeric value plotting
  is supressed. If not given (default), 99 values spaced
  evenly between -4 and +4 will be used, handy for
  plotting.}

  \item{plot}{a logical (default \code{plot = TRUE}),
  defining wether to supress plotting an just return a
  matrix of the values of the Item information function.}

  \item{cat}{a logical (default \code{cat = FALSE}),
  defining wether to plot or return the values of the Item
  information function based on item categories.}

  \item{lwd}{see parameters for \code{\link{plot}}}

  \item{col}{see parameters for \code{\link{plot}}}

  \item{...}{arguments passed to plot}
}
\value{
  a plot, a matrix or a single numeric with values of the
  Item information function.
}
\description{
  plotting function for plotting the Item information
  function(IIF).
}
\details{
  no details in the moment.
}
\examples{
########
data(sim200x3)
result <- pair(sim200x3)
# IFF plot for Item No. 2
iff(pair_obj = result, itemnumber = 2 )
# IFF plot for Categories of Item No. 2
iff(pair_obj = result, itemnumber = 2 ,cat=TRUE)
# IFF at theta=0 for Item No. 2
iff(pair_obj = result, itemnumber = 2 ,x=0)
# IFF at theta=0 for Categories of Item No. 2
iff(pair_obj = result, itemnumber = 2 ,x=0,cat=TRUE)
# IFF of Item No. 2 for a given range of thetas
iff(pair_obj = result, itemnumber = 2 ,x=seq(0,4,.1))
# ... etc.
iff(pair_obj = result, itemnumber = 2 ,x=seq(0,4,.1),cat=TRUE)
##### examples with other data ...
data(bfiN)
result <- pair(bfiN)
iff(pair_obj = result, itemnumber = 3 )
iff(pair_obj = result, itemnumber = 3 ,cat=TRUE)
}

