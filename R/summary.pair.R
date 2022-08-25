#' @title S3 Summary for Item Parameter
#' @exportS3Method summary pair
#' @keywords methods
#' @method summary pair
#' @description S3 summary method for object of class\code{"pair"}
#' @param object object of class\code{"pair"}
#' @param sortdif logical with default \code{sortdif=FALSE} whether to order items by difficulty.
#' @param ... other parameters passed trough
########################### here the summary method for pair #############################
summary.pair<-function(object, sortdif=FALSE, ...){
  cat("Thurstonian thresholds and item location (sigma): \n") 
  if(sortdif==TRUE){
    threshold <- object$threshold
    #sb <- object$sb
    sigma <- object$sigma
    #####
    threshold <- threshold[order(sigma), ]
    #sb <- sb[order(sigma), ]
    sigma <- sort(sigma)
    object<-list(threshold=threshold,sigma=sigma)
    #class(object)<-c("PAIRl", "list")
    cat("(ordered by location) \n")
  }
  print(round(data.frame(object[2:1]),5))
  cat("Thurstonian thresholds for rater severity / times of measurement: \n") 
  print(object$taur)
}