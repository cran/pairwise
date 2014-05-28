#' @method summary pairSE
########################### hier die summary method für pairSE #############################
summary.pairSE<-function(object, sortdif=FALSE, ...){
  cat("Thurstonian thresholds and item location (sigma) and their std. errors: \n")
  
  if(sortdif==TRUE){
    sorter <- order(object$parameter[,"sigma"])
    object$parameter <- object$parameter[sorter,]
    object$SE <- object$SE[sorter,]
    cat("(ordered by location) \n")
  }
  print( t(data.frame(object))[  unlist(lapply((c(1:(dim(t(data.frame(object)))[1]/2))),function(x){seq(from=x,by=(dim(t(data.frame(object)))[1]/2),length.out=2)}))  ,]   )
}
