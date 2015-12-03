\name{cog}
\alias{cog}
\docType{data}
\title{Math PISA (2003) data
}
\description{
Data from the german sample of the PISA 2003 survey, containing 31 dichotomous items from the math task.
}
\usage{
  data(cog)
}

\format{
  A data frame containing 34 variables and 4660 obsevations.
}

\details{
The first 3 variables are ID variables. For further Information on variables and their meaning see the codebook PDF file 
available at \href{http://pisa2003.acer.edu.au/downloads.php}{http://pisa2003.acer.edu.au/downloads.php}
}

\source{
\href{http://pisa2003.acer.edu.au/downloads.php}{http://pisa2003.acer.edu.au/downloads.php}
}
\references{
Database - PISA 2003, \emph{Downloadable Data}, \href{http://pisa2003.acer.edu.au/downloads.php}{http://pisa2003.acer.edu.au/downloads.php}
}
\examples{
data(cog)
dim(cog)
##############################################################
names(cog) # show all variable names of data.frame cog
names(cog[,4:34]) # show the variable names of the math items
names(cog[,1:3]) # show the variable names of the ID variables

}
\keyword{datasets}
