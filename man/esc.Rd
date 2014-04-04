\name{esc}
\alias{esc}
\title{Expected Score Curves Plots}
\usage{
  esc(pers_obj, itemnumber = 1, integ = 6, ra = 4,
    nodes = 100, lwd = 2, ...)
}
\arguments{
  \item{pers_obj}{an object of class \code{"pers"} as a
  result from function \code{\link{pers}}.}

  \item{itemnumber}{an integer, defining the number of the
  item to plot the respective categoy probability for. This
  is set to an arbitrary default value of \code{itemnumber
  = 1} to avoid error messages when you forget to choose an
  item to plot the expected score curves for.}

  \item{integ}{either an integer defining the number of
  (ability) groups to integrate the empirical theta vector
  or the character expression \code{"all"} to plot the
  empirical theta distribution at the respective item score
  using symbols (see example).}

  \item{ra}{an integer, defining the (logit) range for
  x-axis}

  \item{nodes}{numer of integration nodes}

  \item{lwd}{see \code{\link{plot}}}

  \item{...}{arguments passed to plot}
}
\description{
  plotting function for plotting expected score curves
}
\details{
  to come ...
}
\examples{
########
data(bfiN)
result <- pers(pair(bfiN))
esc(pers_obj=result,1,lwd=2) # plot for first item
esc(pers_obj=result,2,lwd=2) # plot for second item
for(i in 1:5){esc(pers_obj=result,i,lwd=2)}
#########
esc(pers_obj=result,2,integ="all",lwd=2) # plot for secod item
}

