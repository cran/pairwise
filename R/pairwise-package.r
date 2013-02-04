#' @title Rasch Model Parameters with pairwise
#' @name pairwise
#' @docType package
#' @seealso {PP}
#' @description The package \code{pairwise} performs the explicit calculation, not estimation!, of the Rasch item parameters for dichotomous an polytomous response formats using a pairwise comparison approach (Choppin, 1968, 1985). 
#'
#' @details 
#' In case of dichotomous answer formats the parameter calculation is based on the construction of a pairwise comparison matrix M\emph{nij} with entries f\emph{ij} representing the number of respondents who got item \emph{i} right and item \emph{j} wrong according to Choppin's (1968, 1985) conditional pairwise algorithm. 
#' 
#' For the calculation of the item thresholds and difficulty in case of polytomous answer formats, according to the Partial Credit Model (Masters, 1982), a generalization of the pairwise comparison algorithm is used. The construction of the pairwise comparison matrix is therefore extended to the comparison of answer frequencies for each category of each item. In this case, the pairwise comparison matrix M\emph{nicjc} with entries f\emph{icjc} represents the number of respondents who answered to item \emph{i} in category \emph{c} and to item \emph{j} in category \emph{c-1} widening Choppin's (1968, 1985) conditional  pairwise algorithm to polytomous item response formats. 
#' Within R this algorithm is simply realized by matrix multiplication.
#' 
#' In general, for both polytomous and dichotomous response formats, the benefit in applying this algorithm lies in it's capability to return stabel item parameter 'estimates' even when using data with a relative high amount of missing values, as long as the items are still proper linked together.   
#' 
#' Based on the explicit calculated item parameters for a dataset, the person parameters may thereupon be estimated using an mle or wle approach, for example implementetd in the R-package 'PP' by Manuel Reif.
#' 
#' The recent version (0.1.3) computes item parameters for dichotomous and polytomous item responses according the 1PL and the partial credit model.
#' 
#' The calculation of standard errors, when using functions \code{ipSE.dicho} or \code{ipSE.poly} is realized by bootstrap or jack-knife technique.   
#'
#' @author Joerg-Henrik Heine <jhheine@@googlemail.com>
#' @export itempar.dicho itempar.poly
#' @exportClass ippw ippwpo ipSE ippwpose grmSE grmSEpo
#' @S3method summary ippw
#' @S3method summary ipSE
#' @S3method summary ippwpo
#' @S3method summary ippwpose
#' @S3method plot ippw
#' @S3method plot ippwpo
#' @S3method plot ipSE
#' @S3method plot ippwpose
#' @S3method plot grmSE
#' @S3method plot grmSEpo
#' @references 
#' Choppin, B. (1968). Item Bank using Samplefree Calibration. \emph{Nature, 219}(5156), 870-872.
#' @references
#' Choppin, B. (1985). A fully conditional estimation procedure for Rasch model parameters. \emph{Evaluation in Education, 9}(1), 29-42.
#' @references
#' Masters, G. N. (1982). A Rasch model for partial credit scoring. \emph{Psychometrika, 47}(2), 149-174.

NULL
