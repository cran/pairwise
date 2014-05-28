\name{plot.grm}
\alias{plot.grm}
\title{S3 Plotting Graphical Model Check}
\usage{
  \method{plot}{grm} (x, xymin = NULL, xymax = NULL,
    ci = 2, main = NULL, col.error = "blue",
    itemNames = FALSE, cex.names = 0.8, type = "b",
    xlab = NULL, ylab = NULL, pch = 43, las = 3,
    cex.axis = 0.5, ...)
}
\arguments{
  \item{x}{object of class\code{"grm"}}

  \item{xymin}{optional lower limit for xy-axis}

  \item{xymax}{optional upper limit for xy-axis}

  \item{ci}{numeric defining confidence intervall for point
  estimator}

  \item{main}{see \code{\link{plot}}}

  \item{col.error}{vector of colors for error bars}

  \item{itemNames}{logical wether to plot itemnames}

  \item{cex.names}{magnification factor for itemnames}

  \item{type}{see \code{\link{plot}}}

  \item{xlab}{see \code{\link{plot}}}

  \item{ylab}{see \code{\link{plot}}}

  \item{pch}{see \code{\link{plot}}}

  \item{las}{see \code{\link{plot}}}

  \item{cex.axis}{see \code{\link{plot}}}

  \item{...}{other parameters passed to plot}
}
\description{
  S3 plotting Method for object of class\code{"grm"}
}

