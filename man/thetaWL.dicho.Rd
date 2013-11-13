\name{thetaWL.dicho}
\alias{thetaWL.dicho}
\title{WL Theta Parameter dichotomous 1PL}
\usage{
  thetaWL.dicho(response, sigma, na_treat = 0,
    theta_init = log(sum(response, na.rm = TRUE)),
    limit = 1e-05, iter = 20)
}
\arguments{
  \item{response}{numeric vector with responses of a person
  coded in the (0,1) manner.}

  \item{sigma}{numeric vector with item (difficulty)
  parameters.}

  \item{na_treat}{numeric defining the type of treatment to
  missing responses in the argument \code{response}. If set
  to \code{na_treat=0} (default) missing responses are
  treated as wrong answers. An option is to set
  \code{na_treat} to any value between 0 and 1 e.g.:
  \code{na_treat=0.5} which is guessing probability in
  dichotomous case.}

  \item{theta_init}{numeric giving the startvalue for
  estimation of theta. Default is \code{log()} of the
  persons rawscore (while NAs are removed for rawscore
  calculation).}

  \item{limit}{numeric giving the limit at which accuracy
  the WL-algorithm stops.}

  \item{iter}{numeric giving the maximum numer of iteration
  to perform.}
}
\value{
  An object of class thetaWLdicho containing the person
  parameter.
}
\description{
  Calculation of the theta person parameter for dichotomous
  items, given the item parameters and one respose vector
  according the 1PL Rasch Model using an WL approach.
}
\details{
  as this function is usualy not called directly by the
  user, there are no checks for plausibillity of the
  arguments !. Instead of calling this function directly,
  consider using the function \code{\link{ppWL.dicho}}.
}
\examples{
######## ... ####
}

