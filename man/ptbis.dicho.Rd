\name{ptbis.dicho}
\alias{ptbis.dicho}
\title{Point Biserial Correlations dichotomous 1PL}
\usage{
  ptbis.dicho(daten, scale)
}
\arguments{
  \item{daten}{a data matrix, potentially with missing
  values, comprising dichotomous items (columns).}

  \item{scale}{an integer vector as an result of any
  scaling approach (WLE, MLE, Rawscore, etc. ) relating to
  the Items (columns) in \code{daten}.}
}
\value{
  An object of class "list" and "itanaldicho" containing
  item statistics.
}
\description{
  Calculation of the point biserial correlations for
  dichotomous item categories with total scale parameter
  (person parameter).
}
\details{
  no details in the moment.
}
\examples{
######################
##### compute person ML estimates first ########
data(cog);data(cogBOOKLET) # loading reponse and allocation data
d<-(cog[cog$BOOKID!=14,]) # skip persons which got booklet No.14.
inc<-make.incidenz(tab=cogBOOKLET, bookid=d$BOOKID) # make just the incidenz matrix
result<-ppML.dicho(daten=d[,4:34], SIGMA=itempar.dicho(d[,4:34]),incidenz = inc )
MLscale<-summary(result,FALSE)[,1] # return just the ML person estimates and their standard errors.
##### now compute item statistics ########
ptbis.dicho(daten=d[,4:34], scale=MLscale)
}

