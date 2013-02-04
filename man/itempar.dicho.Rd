\name{itempar.dicho}
\alias{itempar.dicho}
\title{Item Parameter dichotomous 1PL}
\usage{
  itempar.dicho(daten, sortdif = TRUE, pot = TRUE, ...)
}
\arguments{
  \item{daten}{a data matrix, potentially with missing
  values, comprising dichotomous responses of respondents
  (rows) on some items (colums) coded in the 0 1 manner.}

  \item{sortdif}{logical, if TRUE (default) items are
  sorted in an ascending order by difficulty for output.}

  \item{pot}{logical, if TRUE (default) a power of three of
  the pairwise comparison matrix is used for further
  calculations.}

  \item{...}{additional parameters passed through}
}
\value{
  An object of class ippw containing item difficulties
  sigma.
}
\description{
  Calculation of the item parameter for dichotomous items
  according the 1PL Rasch Model using a pairwise comparison
  algorithm (Choppin, 1968, 1985). Missing values up to an
  high amount in data matrix are allowed, as long as items
  are proper linked together.
}
\details{
  Parameter calculation is based on the construction of a
  paired comparison matrix M\emph{nij} with entries
  f\emph{ij} representing the number of respondents who got
  item \emph{i} right and item \emph{j} wrong according to
  Choppin's (1968, 1985) conditional pairwise algorithm.
  This algorithm is simply realized by matrix
  multiplication.

  To avoid numerical problems with off diagonal zero's when
  constructing the pairwise comparison matrix M\emph{nij},
  powers of the M\emph{nij} matrix, can be used (Choppin,
  1968, 1985). Using powers \emph{k} of M\emph{nij}
  replaces the results of the direct comparisons between
  \emph{i} and \emph{j} with the sum of the indirect
  comparisons of \emph{i} and \emph{j} through an
  intermediate \emph{k}.

  In general, it is recommended to use the argument with
  default value \code{pot=TRUE}.
}
\examples{
data(cog) # loading example data set
sigma<-itempar.dicho(daten=cog[,4:34], pot=TRUE) # calculating itemparameters for 31 math items
sigma
#######
plot(sigma) # plotting item difficulties
}
\references{
  Choppin, B. (1968). Item Bank using Samplefree
  Calibration. \emph{Nature, 219}(5156), 870-872.

  Choppin, B. (1985). A fully conditional estimation
  procedure for Rasch model parameters. \emph{Evaluation in
  Education, 9}(1), 29-42.
}

