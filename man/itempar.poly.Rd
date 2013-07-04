\name{itempar.poly}
\alias{itempar.poly}
\title{Item Parameter for polytomous Partial Credit Model}
\usage{
  itempar.poly(daten, m = max(daten, na.rm = TRUE) + 1,
    sortdif = TRUE, pot = TRUE, zerocor = TRUE, ...)
}
\arguments{
  \item{daten}{a data matrix with optionaly named colums
  (names of items) or a data.frame, potentially with
  missing values, comprising polytomous responses of
  respondents (rows) on some items (colums) coded starting
  with 0 for lowest category to \emph{m}-1 for highest
  category, with \emph{m} beeing the number of categories
  for all items.}

  \item{m}{number of response categories for all items - by
  default \emph{m} is defined as \code{m =
  max(daten,na.rm=TRUE)+1}.}

  \item{sortdif}{logical, if TRUE (default) items are
  sorted in an ascending order by difficulty for output.}

  \item{pot}{logical, if TRUE (default) a power of three of
  the pairwise comparison matrix is used for further
  calculations.}

  \item{zerocor}{logical, if TRUE (default) unobserved
  combinations (1-0, 0-1) in data for each pair of items
  are given a frequency of one conf. proposal by
  Alexandrowicz(2011, p.373).}

  \item{...}{additional parameters passed through.}
}
\value{
  A (list) object of class ippwpo containing the item
  category thresholds and difficulties sigma.
}
\description{
  Calculation of the item parameter for polytomous items
  according to the Partial Credit Model using a
  generalization of the pairwise comparison algorithm
  (Choppin, 1968, 1985). Missing values up to an high
  amount in data matrix are allowed, as long as items are
  proper linked together.
}
\details{
  Parameter calculation is based on the construction of a
  paired comparison matrix M\emph{nicjc} with entries
  f\emph{icjc} representing the number of respondents who
  answered to item \emph{i} in category \emph{c} and to
  item \emph{j} in category \emph{c-1} widening Choppin's
  (1968, 1985) conditional pairwise algorithm to polytomous
  item response formats. This algorithm is simply realized
  by matrix multiplication.

  To avoid numerical problems with off diagonal zero's when
  constructing the pairwise comparison matrix M\emph{nij},
  powers of the M\emph{nicjc} matrix, can be used (Choppin,
  1968, 1985). Using powers \emph{k} of M\emph{nicjc} -
  argument \code{pot=TRUE} (default), replaces the results
  of the direct comparisons between \emph{i} and \emph{j}
  with the sum of the indirect comparisons of \emph{i} and
  \emph{j} through an intermediate \emph{k}.

  In general, it is recommended to use the argument with
  default value \code{pot=TRUE}.
}
\examples{
data(bfiN) # loading example data set
# calculating itemparameters for 5 neuroticism items with 6 answer categories (0-5).
neuro_itempar<-itempar.poly(daten = bfiN, m = 6)
neuro_itempar
################
# plotting threshold profiles for 5 neuroticism items.
# 6 categories - 5 thresholds
plot(neuro_itempar)
}
\references{
  Choppin, B. (1968). Item Bank using Samplefree
  Calibration. \emph{Nature, 219}(5156), 870-872.

  Choppin, B. (1985). A fully conditional estimation
  procedure for Rasch model parameters. \emph{Evaluation in
  Education, 9}(1), 29-42.

  Alexandrowicz, R. W. (2011). 'GANZ RASCH': A Free
  Software for Categorical Data Analysis. \emph{Social
  Science Computer Review, 30}(3), 369-379.
}

