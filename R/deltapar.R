#' @title Compute delta parameters from thurstonian thresholds
#' @export deltapar
#' @description Calculation of delta parameters or rather item step parameters from thurstonian threshold parameters returned by the function \code{\link{pair}}.   
#'  
#' @details The "Thurstone threshold" or rather thurstonian threshold for a category corresponds to a point on the latent variable at which the probability of being observed in that category or above equals that of being observed in the categories below. Thus these thurstonian threshold parameters can be interpreted in an strait forward and easy way. However, some other computer programs related to Rasch analysis don't return thurstonian threshold parameters from their estimation procedure, but rather so called delta parameters for the item steps. The later are also known as "step measures", "step calibrations", "step difficulties", "tau parameters", and "Rasch-Andrich thresholds". For a better comparability between different Rasch software and estimation procedures the thurstonian threshold parameters can be converted into delta or rather items step parameters.
#' @references Linacre J.M. (1992). Rasch-Andrich Thresholds and Rasch-Thurstone Thresholds. \emph{Rasch Measurement Transactions}, 5:4, 191. https://www.rasch.org/rmt/rmt54r.htm
#' @references Linacre J.M. (2001). Category, Step and Threshold: Definitions & Disordering. \emph{Rasch Measurement Transactions}, 15:1, 794. https://www.rasch.org/rmt/rmt151g.htm
#' @references Adams, R. J., Wu, M. L., & Wilson, M. (2012). The Rasch Rating Model and the Disordered Threshold Controversy. \emph{Educational and Psychological Measurement, 72}(4), 547–573. https://doi.org/10.1177/0013164411432166
#' @references Linacre J.M. (2006). Item Discrimination and Rasch-Andrich Thresholds. \emph{Rasch Measurement Transactions}, 20:1, 1054. https://www.rasch.org/rmt/rmt201k.htm

#' @param object an object of class \code{"pair"} as resulting from item parameter calculation using the function \code{\link{pair}}.
#' @param sigma a logical whether to return item difficulties (sigma) or not
#' @return If \code{sigma=TRUE} an object of class \code{c("data.frame", "deltapar")} containing delta parameters for items and their difficultie (first column). Otherwise a matrix containing only the delta parameters.
#' @examples ######################
#' data(sim200x3) # loading reponse data
#' ip <- pair(sim200x3,m = c(2,3,3)) # compute item parameters
#' summary(ip) # have a look at the results (thurstonian thresholds)
#' deltapar(ip) # compute delta parameters from these 

deltapar <- function(object, sigma=TRUE){
  dix <- as.list(as.data.frame(object$threshold))
  di <- object$sigma 
  delta <- sapply(dix,function(x){x-di})
  colnames(delta) <- paste("delta",colnames(delta),sep = ".")
  if(sigma==TRUE){
    delta <- data.frame(sigma=di,delta)
    class(delta) <- c("data.frame", "deltapar")
  }
  return(delta)
}
