#' @method summary ppfit
#' @title S3 Summary for Person-Fit-Statistics
#' @description S3 summary method for object of class\code{c("ppfit", "data.frame" )}
#' @param object object of class\code{"ppfit", "data.frame" }
#' @param sort logical with default \code{sort=FALSE} - if set to \code{sort=TRUE} persons are ordered by absolute FIT. 
#' @param by character passing the type of Fit-Statistic to sort by - ignored when \code{sort=FALSE}. valid options are: \code{"INFIT.ZSTD"} (default), \code{"OUTFIT.MSQ"}, \code{"OUTFIT.ZSTD"} and \code{"INFIT.MSQ"}.
#' @param decreasing see \code{\link{order}}
#' @param relative logical with default \code{relative=FALSE} to return the fit statistics as proposed by Wright & Masters, (1982, P. 100) with no further modifications. If \code{relative=TRUE} the sample adjusted fit statistics are returned in a way that their mean (for the present sample) equals 1 using formula: fit_i+ = 1 – mean(fit).
#' @param ... other parameters passed trough - see \code{\link{order}}
#' @references Wright, B. D., & Masters, G. N. (1982). \emph{Rating Scale Analysis.} Chicago: MESA Press.
#' @references Wright, B. D., & Masters, G. N. (1990). Computation of OUTFIT and INFIT Statistics. \emph{Rasch Measurement Transactions, 3}(4), 84–85.

########################### hier die summary method fuer pairwise.person.fit #############################
summary.ppfit<-function(object, sort=FALSE, by="INFIT.ZSTD", decreasing=FALSE, relative=FALSE, ...){
  
  if(relative==TRUE){
    fit_1 <- object[ , c(1:3,8:11)]
    colnames(fit_1)[4:7] <- c("OUTFIT.MSQ","OUTFIT.ZSTD","INFIT.MSQ","INFIT.ZSTD" )
  }
  if(relative==FALSE){
  fit_1 <- object[ , c(1:3,4:7)]
  }
  
  if(sort==TRUE){  
    result <- fit_1[order( abs(fit_1[,by]),decreasing = decreasing),]
    cat("(ordered by absolute value of" , by, ")","\n")
    }else{result <- fit_1}
    return(result)
  }
