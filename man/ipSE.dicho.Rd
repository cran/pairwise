\name{ipSE.dicho}
\alias{ipSE.dicho}
\title{Item Parameter calaculation with Standard Errors for dichotomous 1PL Model}
\usage{
  ipSE.dicho(daten, sortdif = TRUE, nsample = 30,
    size = 0.5, seed = "no", pot = TRUE, zerocor = TRUE,
    ...)
}
\arguments{
  \item{daten}{The response data as a data.frame or a
  matrix, potentially with missing values, comprising
  dichotomous responses of respondents (rows) on some items
  (colums) coded in the 0 1 manner.}

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

  \item{zerocor}{logical, if TRUE (default) unobserved
  combinations (1-0, 0-1) in data for each pair of items
  are given a frequency of one conf. proposal by
  Alexandrowicz(2011, p.373).}

  \item{...}{additional parameters passed through.}
}
\value{
  An object of class ipSE containing the item difficulty
  parameter Sigma and standard errors for item difficulties
  Sigma.
}
\description{
  Calculation of the item parameter (Sigma) and their
  standard error (SE) for dichotomous items according the
  1PL Rasch Model using a pairwise comparison algorithm
  (Choppin, 1968, 1985). Missing values up to an high
  amount in data matrix are allowed, as long as items are
  proper linked together.
}
\details{
  Item Parameter calculation is based on the construction
  of a paired comparison matrix M\emph{nij} with entries
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
data(cog) # loading example data set

# calculating itemparameters and their SE for 31 math items
se_sigma<-ipSE.dicho(daten=cog[,4:34], pot=TRUE)

summary(se_sigma) # summary for result

# plotting item difficulties with a CI = 95\%
plot(se_sigma)

# use different color for CI
plot(se_sigma, col.error="green")

# without CI bars
plot(se_sigma,ci=0)

###### example from details section 'Some Notes on Standard Errors' ########
# se_sigma_400<-ipSE.dicho(daten=cog[,4:34], pot=TRUE,nsample=400)
# plot(se_sigma)
# plot(se_sigma_400)
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

