#' @title Person Separation Reliability
#' @export pairwise.SepRel
#' @description This function calculates an Index of Person Separation, that is the proportion of person variance that is not due to error.
#'  
#' @details none
#' @param pers_obj an object of class \code{"pers"} as a result from function \code{\link{pers}}.
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA values should be stripped before the computation proceeds. 
#' @return An object of class \code{c("pairwiseSepRel","list")}.
#' @references Andrich, D. (1982). An index of person separation in latent trait theory, the traditional KR.20 index, and the Guttman scale response pattern. \emph{Education Research and Perspectives, 9}(1), 95–104.

#' @examples ######################
#' ########
#' data(bfiN) # loading reponse data
#' pers_obj <- pers(pair(bfiN))
#' result <- pairwise.SepRel(pers_obj)
#' result
#' str(result) # to see whats in ;-)
#' #### 

pairwise.SepRel <- function(pers_obj,na.rm=TRUE){
  reldat1 <- data.frame(WLE=pers_obj$pers$WLE,SE.WLE=pers_obj$pers$SE.WLE)
  if(na.rm==TRUE){
    reldat1 <- reldat1[complete.cases(reldat1),]
  }
  N=dim(reldat1)[1]
  
  # compute the Observed Variance (also known as Total Person Variability or Squared Standard Deviation)
  SSD.PersonScores <- var(reldat1$WLE)
  # compute the Mean Square Measurement error (also known as Model Error variance)
  MSE <- sum((reldat1$SE.WLE)^2) / length(reldat1$SE.WLE)
  
  separation.reliability <- (SSD.PersonScores-MSE) / SSD.PersonScores
  
  # define the outcome of the function "SepRel" as an object of class "separation" 
  result <- structure(
    list(
      "sep.rel" = separation.reliability,
      "SSD.PS" = SSD.PersonScores,
      "MSE" = MSE
    ),
    class=c("pairwiseSepRel","list")
  )
  return(result)
  }

