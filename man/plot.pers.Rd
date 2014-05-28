\name{plot.pers}
\alias{plot.pers}
\title{S3 Plotting Person - Item Map}
\usage{
  \method{plot}{pers} (x, ra = NULL, sortdif = FALSE,
    main = NULL, ylab = "Logits", fillCol = "grey60",
    lineCol = "grey40", cex = 0.7, pos = 4,
    breaks = "Sturges", ...)
}
\arguments{
  \item{x}{object of class\code{"pers"}}

  \item{ra}{an integer, defining the (logit) range for
  y-axis}

  \item{sortdif}{logical wether to order items by
  difficulty}

  \item{main}{see \code{\link{plot}}}

  \item{ylab}{see \code{\link{plot}}}

  \item{fillCol}{color for bar filling of the ability
  histogram}

  \item{lineCol}{color for bar lines of the ability
  histogram}

  \item{cex}{see \code{\link{text}}}

  \item{pos}{see \code{\link{text}}}

  \item{breaks}{see \code{\link{hist}}}

  \item{...}{other parameters passed to \code{\link{hist}}
  and \code{\link{text}}.}
}
\description{
  S3 plotting method for object of class\code{"pers"}
}

