\name{ipSE.poly}
\alias{ipSE.poly}
\title{Item Parameter calculation with Standard Errors for polytomous Partial Credit  Model}
\usage{
  ipSE.poly(daten, m = max(daten, na.rm = TRUE) + 1,
    sortdif = TRUE, nsample = 30, size = 0.5, seed = "no",
    pot = TRUE, ...)
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

  \item{nsample}{numeric specifying the number of
  subsamples sampled from data, which is the number of
  replications of the parameter calculation.

  WARNING! specifying high values for \code{nsample} ( >
  100 ) may result in long computing time without leadig to
  "better" estimates for SE. This may also be the case when
  choosing argument \code{size="jack"} (see argument
  \code{size}) in combination with large datasets (\emph{N}
  > 500).}

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

  \item{...}{additional parameters passed through.}
}
\value{
  A (list) object of class ippwpose containing the item
  category thresholds, difficulties sigma and their standrd
  errors.
}
\description{
  Calculation of the item threshold, the difficulty and
  their standard errors (SE) respectively for polytomous
  items according to the Partial Credit Model. All
  parameters are calculatetd using a generalization of the
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
\section{A Note on Standard Errors}{
  Estimation of standard errors is done by repeated
  calculation of item parameters for subsamples of the
  given data. This procedure is mainly controlled by the
  arguments \code{nsample} and \code{size} (see arguments).
  With regard to calculation time, the argument
  \code{nsample} is the 'time killer'. On the other hand,
  things (estimation of standard errors) will not
  necessarily get better when choosing large values for
  \code{nsample}. For example choosing \code{nsample=400}
  will only result in minimal change for standard error
  estimation in comparison to (\code{nsample=30}) which is
  the default setting (see examples).
}
\examples{
data(bfiN) # loading example data set

# calculating itemparameters and their SE for 5 neuroticism items with 6 answer categories (0-5).
neuro_itempar<-ipSE.poly(daten = bfiN, m = 6)

summary(neuro_itempar) # summary for result

# plotting item thresholds with with their CI = 95\%
plot(neuro_itempar)

###### example from details section 'Some Notes on Standard Errors' ########
# neuro_itempar_400<-ipSE.poly(daten = bfiN, m = 6,nsample=400)
# plot(neuro_itempar)
# plot(neuro_itempar_400)
}
\references{
  Choppin, B. (1968). Item Bank using Samplefree
  Calibration. \emph{Nature, 219}(5156), 870-872.

  Choppin, B. (1985). A fully conditional estimation
  procedure for Rasch model parameters. \emph{Evaluation in
  Education, 9}(1), 29-42.
}

