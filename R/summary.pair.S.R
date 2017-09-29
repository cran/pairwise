#' @export summary.pair.S
#' @title S3 Summary for S-statistic Test (Wald Test)
#' @description S3 summary method for object of class\code{"pair.S"}
#' @param object object of class\code{"pair.S"}
#' @param thres logical whether to output results based on the thresholds
#' @param ... other parameters passed trough
########################### hier die summary method #class pair.wald #######################
summary.pair.S<-function(object, thres=TRUE, ...){
  if(length(object$S)==1){
    cat("Results for S-statistic (Wald test):",names(object$S),"\n")
    print(object$call)
    cat("\n")
    if(thres==TRUE){
      z <- (object$S[[1]]$threshold$z)
      colnames(z) <- paste(colnames(z),"z-value")
      p <- (object$S[[1]]$threshold$p)
      colnames(p) <- paste(colnames(p),"p-value")
      res <- cbind(z,p)
      erg <- res[,c(matrix(1:ncol(res),ncol = (ncol(res)/2),byrow=T))]
    }
    if(thres==FALSE){
      z <- (object$S[[1]]$sigma$z)
      p <- (object$S[[1]]$sigma$p)
      res <- cbind(z,p)
      colnames(res) <- paste(colnames(res),"value",sep="-")
      erg <- res
    }
  }
  if(length(object$S)!=1){
    cat("summary for S-statistic with more than two groups currently not supportet")
    erg <- NULL
    ## Baustelle
  }
  return(erg)
}