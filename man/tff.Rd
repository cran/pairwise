\name{tff}
\alias{tff}
\title{Test information function}
\usage{
  tff(pair_obj, items = NULL, x = NULL,
    main = "Test Information Function", plot = TRUE,
    cat = FALSE, lwd = 2, col = 1, ...)
}
\arguments{
  \item{pair_obj}{an object of class \code{"pair"} as a
  result from function \code{\link{pair}}.}

  \item{items}{optional a vector (character or numeric)
  identifying the items (according their order in the data)
  to use for plotting the test information function.}

  \item{x}{The value(s) of the latent variable, at which
  the TIF will be evaluated. \code{x} should be either a
  numeric vector of theta values or a single numeric value.
  If \code{x} is given as a single numeric value plotting
  is supressed. If not given (default), 99 values spaced
  evenly between -4 and +4 will be used, handy for
  plotting.}

  \item{main}{see parameters for \code{\link{plot}}}

  \item{plot}{a logical (default \code{plot = TRUE}),
  defining wether to supress plotting an just return a
  matrix of the values of the Item information function.}

  \item{cat}{a logical (default \code{cat = FALSE}),
  defining wether to plot as an overlay to the Test
  information function the item category information
  functions based on item categories. If \code{cat = TRUE}
  and \code{plot = FALSE} the values of the item category
  information functions are returned.}

  \item{lwd}{see parameters for \code{\link{plot}}}

  \item{col}{see parameters for \code{\link{plot}}}

  \item{...}{arguments passed to plot}
}
\value{
  a plot, a "data.frame" or a single numeric with values of
  the Test information function.
}
\description{
  plotting function for plotting the test information
  function (TIF).
}
\details{
  no details in the moment.
}
\examples{
########
data(sim200x3)
result <- pair(sim200x3)
tff(pair_obj = result) # TIF plot
tff(pair_obj = result, cat=TRUE) # TIF plot
tff(pair_obj = result, items=c("V1","V3"), cat=TRUE) # TIF plot
tff(pair_obj = result, x=0) # TIF at theta=0
tff(pair_obj = result, x=seq(0,4,.1)) # TIF for a given range of Thetas
##### examples with other data ...
data(bfiN)
result <- pair(bfiN)
tff(pair_obj = result)
tff(pair_obj = result, cat=TRUE) # TIF plot
}

