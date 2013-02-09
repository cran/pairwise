\docType{package}
\name{pairwise}
\alias{pairwise}
\alias{pairwise-package}
\title{Rasch Model Parameters with pairwise}
\description{
  The package \code{pairwise} performs the explicit
  calculation, not estimation!, of the Rasch item
  parameters for dichotomous an polytomous response formats
  using a pairwise comparison approach (Choppin, 1968,
  1985).
}
\details{
  In case of dichotomous answer formats the parameter
  calculation is based on the construction of a pairwise
  comparison matrix M\emph{nij} with entries f\emph{ij}
  representing the number of respondents who got item
  \emph{i} right and item \emph{j} wrong according to
  Choppin's (1968, 1985) conditional pairwise algorithm.

  For the calculation of the item thresholds and difficulty
  in case of polytomous answer formats, according to the
  Partial Credit Model (Masters, 1982), a generalization of
  the pairwise comparison algorithm is used. The
  construction of the pairwise comparison matrix is
  therefore extended to the comparison of answer
  frequencies for each category of each item. In this case,
  the pairwise comparison matrix M\emph{nicjc} with entries
  f\emph{icjc} represents the number of respondents who
  answered to item \emph{i} in category \emph{c} and to
  item \emph{j} in category \emph{c-1} widening Choppin's
  (1968, 1985) conditional pairwise algorithm to polytomous
  item response formats. Within R this algorithm is simply
  realized by matrix multiplication.

  In general, for both polytomous and dichotomous response
  formats, the benefit in applying this algorithm lies in
  it's capability to return stabel item parameter
  'estimates' even when using data with a relative high
  amount of missing values, as long as the items are still
  proper linked together.

  Based on the explicit calculated item parameters for a
  dataset, the person parameters may thereupon be estimated
  using an mle or wle approach, for example implementetd in
  the R-package \code{{PP}} by Manuel Reif.

  The recent version of the package 'pairwise' (0.1.3-1)
  computes item parameters for dichotomous and polytomous
  item responses according the 1PL (when using the function
  \code{\link{itempar.dicho}}) and the partial credit model
  (when using the function \code{\link{itempar.poly}}).

  The calculation of standard errors, when using functions
  \code{\link{ipSE.dicho}} or \code{\link{ipSE.poly}} is
  realized by bootstrap or jack-knife technique.

  Splitting the data (e.g. by rawscore or by random) and
  calculating model parameter for each subsample can be
  done with the functions \code{\link{grmSE.dicho}} and
  \code{\link{grmSE.poly}}
}
\author{
  Joerg-Henrik Heine <jhheine@googlemail.com>
}
\references{
  Choppin, B. (1968). Item Bank using Samplefree
  Calibration. \emph{Nature, 219}(5156), 870-872.

  Choppin, B. (1985). A fully conditional estimation
  procedure for Rasch model parameters. \emph{Evaluation in
  Education, 9}(1), 29-42.

  Heine, J. H. & Tarnai, Ch. (2011). Item-Parameter
  Bestimmung im Rasch-Modell bei unterschiedlichen
  Datenausfallmechanismen. \emph{Referat im 17. Workshop
  'Angewandte Klassifikationsanalyse'} [Item parameter
  determination in the Rasch model for different missing
  data mechanisms. Talk at 17. workshop 'Applied
  classification analysis'], Landhaus Rothenberge,
  Muenster, Germany 09.-11.11.2011

  Heine, J. H., Tarnai, Ch. & Hartmann, F. G. (2011). Eine
  Methode zur Parameterbestimmung im Rasch-Modell bei
  fehlenden Werten. \emph{Vortrag auf der 10. Tagung der
  Fachgruppe Methoden & Evaluation der DGPs.} [A method for
  parameter estimation in the Rasch model for missing
  values. Paper presented at the 10th Meeting of the
  Section Methods & Evaluation of DGPs.] Bamberg, Germany,
  21.09.2011 - 23.09. 2011.

  Masters, G. N. (1982). A Rasch model for partial credit
  scoring. \emph{Psychometrika, 47}(2), 149-174.

  Reif, M. (2012). PP: Person Parameter estimation. \emph{R
  package version 0.2}, Abgerufen von
  http://CRAN.R-project.org/package=PP
}

