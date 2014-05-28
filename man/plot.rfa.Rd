\name{plot.rfa}
\alias{plot.rfa}
\title{S3 Plotting Rasch Residual Factor Analysis}
\usage{
  \method{plot}{rfa} (x, com = 1, ra = "auto", main = NULL,
    labels = NULL, xlab = "logits", ylab = "loadings",
    srt = 0, cex.axis = 0.8, cex.text = 0.8,
    col.text = NULL, ...)
}
\arguments{
  \item{x}{object of class\code{"rfa"}}

  \item{com}{an integer giving the number of the principal
  component used for plotting}

  \item{ra}{either the character \code{"auto"} (default) or
  an numeric, defining the (logit) range for x-axis}

  \item{main}{see \code{\link{plot}}}

  \item{labels}{a character vector specifying the plotting
  pattern to use. see \code{\link{text}}. At default the
  itemnames are used.}

  \item{xlab}{see \code{\link{plot}}}

  \item{ylab}{see \code{\link{plot}}}

  \item{srt}{see \code{\link{text}} or \code{\link{par}}}

  \item{cex.axis}{see \code{\link{plot}}}

  \item{cex.text}{see argument \code{cex} in function
  \code{\link{text}}}

  \item{col.text}{see argument \code{col} in function
  \code{\link{text}}}

  \item{...}{other parameters passed through.}
}
\description{
  S3 plotting Method for object of class\code{"rfa"}
}

