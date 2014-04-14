\name{plot.pairSE}
\alias{plot.pairSE}
\title{S3 Plotting Thustonian Thresholds with SE}
\usage{
  \method{plot}{pairSE} (x, ci = 2, sortdif = FALSE,
    ra = "auto", main = NULL,
    col.lines = 1:(dim(x$parameter)[2] - 1),
    col.error = 1:(dim(x$parameter)[2] - 1), type = "b",
    xlab = "items", ylab = "logits", pch = 20, las = 3,
    cex.axis = 0.8, ...)
}
\arguments{
  \item{x}{object of class\code{"pairSE"}}

  \item{sortdif}{logical wether to order items by
  difficulty}

  \item{ra}{either the character \code{"auto"} (default) or
  an numeric, defining the (logit) range for y-axis}

  \item{ci}{numeric defining confidence intervall for point
  estimator}

  \item{main}{see \code{\link{plot}}}

  \item{col.lines}{vector of colors for threshold profile
  lines}

  \item{col.error}{vector of colors for error bars}

  \item{type}{see \code{\link{plot}}}

  \item{xlab}{see \code{\link{plot}}}

  \item{ylab}{see \code{\link{plot}}}

  \item{pch}{see \code{\link{plot}}}

  \item{las}{see \code{\link{plot}}}

  \item{cex.axis}{see \code{\link{plot}}}

  \item{...}{other parameters passed to plot}
}
\description{
  S3 plotting method for object of class\code{"pairSE"}
}

