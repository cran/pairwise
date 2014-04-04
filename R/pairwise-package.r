#' @title Rasch Model Parameters with pairwise
#' @name pairwise
#' @docType package
#' @description The package \code{pairwise} performs the explicit calculation -- not estimation! -- of the Rasch item parameters for dichotomous an polytomous response formats using a pairwise comparison approach (Choppin, 1968, 1985). 
#'
#' @details 
#' In case of dichotomous answer formats the item parameter calculation for the Rasch Model (Rasch, 1960), is based on the construction of a pairwise comparison matrix M\emph{nij} with entries f\emph{ij} representing the number of respondents who got item \emph{i} right and item \emph{j} wrong according to Choppin's (1968, 1985) conditional pairwise algorithm. 
#' 
#' For the calculation of the item thresholds and difficulty in case of polytomous answer formats, according to the Partial Credit Model (Masters, 1982), a generalization of the pairwise comparison algorithm is used. The construction of the pairwise comparison matrix is therefore extended to the comparison of answer frequencies for each category of each item. In this case, the pairwise comparison matrix M\emph{nicjc} with entries f\emph{icjc} represents the number of respondents who answered to item \emph{i} in category \emph{c} and to item \emph{j} in category \emph{c-1} widening Choppin's (1968, 1985) conditional  pairwise algorithm to polytomous item response formats. 
#' Within R this algorithm is simply realized by matrix multiplication.
#' 
#' In general, for both polytomous and dichotomous response formats, the benefit in applying this algorithm lies in it's capability to return stabel item parameter 'estimates' even when using data with a relative high amount of missing values, as long as the items are still proper linked together.
#' 
#' The recent version of the package 'pairwise' computes item parameters for dichotomous and polytomous item responses  -- and a mixture of both -- according the partial credit model using the function \code{\link{pair}}.   
#' 
#' Based on the explicit calculated item parameters for a dataset, the person parameters may thereupon be estimated using any estimation approach. The function \code{\link{pers}} implemented in the package uses WLE for estimation of the person parameter.
#'
#' @author Joerg-Henrik Heine <jhheine@@googlemail.com>
#' @S3method summary pair
#' @S3method summary pairSE
#' @S3method summary grm
#' @S3method summary pers
#' @S3method plot pair
#' @S3method plot pairSE
#' @S3method plot grm
#' @references 
#' Choppin, B. (1968). Item Bank using Samplefree Calibration. \emph{Nature, 219}(5156), 870-872.
#' @references
#' Choppin, B. (1985). A fully conditional estimation procedure for Rasch model parameters. \emph{Evaluation in Education, 9}(1), 29-42.
#' @references
#' Heine, J. H. & Tarnai, Ch. (2011). Item-Parameter Bestimmung im Rasch-Modell bei unterschiedlichen Datenausfallmechanismen. \emph{Referat im 17. Workshop 'Angewandte Klassifikationsanalyse'} [Item parameter determination in the Rasch model for different missing data mechanisms. Talk at 17. workshop 'Applied classification analysis'], Landhaus Rothenberge, Muenster, Germany 09.-11.11.2011
#' @references
#' Heine, J. H., Tarnai, Ch. & Hartmann, F. G. (2011). Eine Methode zur Parameterbestimmung im Rasch-Modell bei fehlenden Werten. \emph{Vortrag auf der 10. Tagung der Fachgruppe Methoden & Evaluation der DGPs.} [A method for parameter estimation in the Rasch model for missing values. Paper presented at the 10th Meeting of the Section Methods & Evaluation of DGPs.] Bamberg, Germany, 21.09.2011 - 23.09. 2011.
#' @references
#' Masters, G. N. (1982). A Rasch model for partial credit scoring. \emph{Psychometrika, 47}(2), 149-174.
#' @references Rasch, G. (1960). \emph{Probabilistic models for some intelligence and attainment tests.} Copenhagen: Danmarks pædagogiske Institut.
NULL
