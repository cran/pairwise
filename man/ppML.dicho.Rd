\name{ppML.dicho}
\alias{ppML.dicho}
\title{ML Person Parameter dichotomous 1PL}
\usage{
  ppML.dicho(daten, SIGMA, incidenz = NULL, na_treat = 0,
    limit = 1e-05, iter = 20, tecout = FALSE)
}
\arguments{
  \item{daten}{a matrix (or data.frame) containing
  (numeric) responses of the persons (rows) coded in the
  (0,1) manner.}

  \item{SIGMA}{numeric vector with item (difficulty)
  parameters.}

  \item{incidenz}{This is only relevant when items are
  assigned to different booklets. For such a booklet-design
  a matrix should be assigned to this argument with the
  same dimensions like \code{daten} containig 0 and 1 codes
  giving the information (for every person) if the
  respective item was in the booklet given to the person.}

  \item{na_treat}{numeric defining the type of treatment to
  missing responses in the argument \code{daten}. If set to
  \code{na_treat=0} (default) missing responses are treated
  as wrong answers. An option is to set \code{na_treat} to
  any value between 0 and 1 e.g.: \code{na_treat=0.5}}

  \item{limit}{numeric giving the limit at which accuracy
  the ML-algorithm stops.}

  \item{iter}{numeric giving the maximum numer of iteration
  to perform.}

  \item{tecout}{logical default set to\code{FALSE}. If set
  to \code{TRUE} the result will be a (very) long list with
  estimation details for every case in \code{daten}. In
  case of a booklet-design the list entrys will be divided
  by "booklet".}
}
\value{
  An object of class "ppMLdicho" or "list" (depending on
  \code{techout}) containing the person parameters.
}
\description{
  Calculation of the person parameters for dichotomous
  items, given the item parameters and and the datamatrix
  (argument \code{daten}) containing the person respose
  vectors (rows), according the 1PL Rasch Model using an ML
  approach.
}
\details{
  no detail in the moment.
}
\examples{
############
data(cog);data(cogBOOKLET) # loading reponse and allocation data
d<-(cog[cog$BOOKID!=14,]) # skip persons which got booklet No.14.
inc<-make.incidenz(tab=cogBOOKLET, bookid=d$BOOKID) # make just the incidenz matrix
result<-ppML.dicho(daten=d[,4:34], SIGMA=itempar.dicho(d[,4:34]),incidenz = inc )
summary(result)
summary(result,FALSE) # return just the ML person estimates and their standard errors.
}

