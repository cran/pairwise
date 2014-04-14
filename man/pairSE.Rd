\name{pairSE}
\alias{pairSE}
\title{Item Parameter calculation with Standard Errors for polytomous Partial Credit  Model}
\usage{
  pairSE(daten, m = NULL, nsample = 30, size = 0.5,
    seed = "no", pot = TRUE, zerocor = TRUE,
    verbose = TRUE, ...)
}
\arguments{
  \item{daten}{a data.frame or matrix with optionaly named
  colums (names of items), potentially with missing values,
  comprising polytomous or dichotomous (or mixted category
  numbers) responses of \code{n} respondents (rows) on
  \code{k} items (colums) coded starting with 0 for lowest
  category to \emph{m}-1 for highest category, with
  \emph{m} beeing a vector (with length k) with the number
  of categories for the respective item.}

  \item{m}{an integer (will be recycled to a vector of
  length k) or a vector giving the number of response
  categories for all items - by default \code{m = NULL},
  \code{m} is calculated from data, assuming that every
  response category is at least once present in data. For
  sparse data it is strongly recomended to explicitly
  define the number of categories by defining this
  argument.}

  \item{nsample}{numeric specifying the number of
  subsamples sampled from data, which is the number of
  replications of the parameter calculation.

  WARNING! specifying high values for \code{nsample} ( >
  100 ) may result in long computing time without leading
  to "better" estimates for SE. This may also be the case
  when choosing argument \code{size="jack"} (see argument
  \code{size}) in combination with large datasets (\emph{N}
  > 5000).}

  \item{size}{numeric with valid range between 0 and 1 (but
  not exactly 0 or 1) specifying the size of the subsample
  of \code{data} when bootstraping for SE estimation. As an
  alternative, \code{size} can be set to the character
  \code{"jack"} (\code{size="jack"}). This will set the
  subsample size to \emph{N}-1 and set \code{nsample=N}
  (see argument \code{nsample}), with \emph{N} beeing the
  number of persons in \code{daten}.}

  \item{seed}{numeric used for \code{set.seed(seed)}.}

  \item{pot}{logical, if TRUE (default) a power of three of
  the pairwise comparison matrix is used for further
  calculations.}

  \item{zerocor}{logical, if TRUE (default) unobserved
  combinations (1-0, 0-1) in data for each pair of items
  are given a frequency of one conf. proposal by
  Alexandrowicz(2011, p.373).}

  \item{verbose}{logical, if \code{verbose = TRUE}
  (default) a message about subsampling is sent to console
  when calculating standard errors.}

  \item{...}{additional parameters passed through.}
}
\value{
  A (list) object of class "pairSE" containing the item
  category thresholds, difficulties sigma and their
  standard errors.
}
\description{
  Calculation of the item parameters for dichotomous
  (difficulty) or polytomous items (thurstonian thresholds)
  and their standard errors (SE) respectively. All
  parameters are calculated using a generalization of the
  pairwise comparison algorithm (Choppin, 1968, 1985).
  Missing values up to an high amount in data matrix are
  allowed, as long as items are proper linked together.
}
\details{
  Parameter calculation is based on the construction of a
  paired comparison matrix M\emph{nicjc} with entries
  f\emph{icjc}, representing the number of respondents who
  answered to item \emph{i} in category \emph{c} and to
  item \emph{j} in category \emph{c-1} widening Choppin's
  (1968, 1985) conditional pairwise algorithm to polytomous
  item response formats. This algorithm is simply realized
  by matrix multiplication.

  Estimation of standard errors is done by repeated
  calculation of item parameters for subsamples of the
  given data.

  To avoid numerical problems with off diagonal zeros when
  constructing the pairwise comparison matrix
  M\emph{nicjc}, powers of the M\emph{nicjc} matrix, can be
  used (Choppin, 1968, 1985). Using powers \emph{k} of
  M\emph{nicjc}, argument \code{pot=TRUE} (default),
  replaces the results of the direct comparisons between
  \emph{i} and \emph{j} with the sum of the indirect
  comparisons of \emph{i} and \emph{j} through an
  intermediate \emph{k}.

  In general, it is recommended to use the argument with
  default value \code{pot=TRUE}.
}
\section{A note on standard errors}{
  Estimation of standard errors is done by repeated
  calculation of item parameters for subsamples of the
  given data. This procedure is mainly controlled by the
  arguments \code{nsample} and \code{size} (see arguments).
  With regard to calculation time, the argument
  \code{nsample} may be the 'time killer'. On the other
  hand, things (estimation of standard errors) will not
  necessarily get better when choosing large values for
  \code{nsample}. For example choosing \code{nsample=400}
  will only result in minimal change for standard error
  estimation in comparison to (\code{nsample=30}) which is
  the default setting (see examples).
}
\examples{
data(bfiN) # loading example data set

# calculating itemparameters and their SE for 5 neuroticism items with 6 answer categories (0-5).
neuro_itempar<-pairSE(daten = bfiN, m = 6)
summary(neuro_itempar) # summary for result

# plotting item thresholds with with their CI = 95\%
plot(neuro_itempar)
plot(neuro_itempar,sortdif=TRUE)

###### example from details section 'Some Notes on Standard Errors' ########
neuro_itempar_400<-pairSE(daten = bfiN, m = 6,nsample=400)
plot(neuro_itempar)
plot(neuro_itempar_400)
}
\references{
  Choppin, B. (1968). Item Bank using Samplefree
  Calibration. \emph{Nature, 219}(5156), 870-872.

  Choppin, B. (1985). A fully conditional estimation
  procedure for Rasch model parameters. \emph{Evaluation in
  Education, 9}(1), 29-42.
}

