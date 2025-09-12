#' @title S3 Summary for Q3 Fit Statistic
#' @exportS3Method summary q3
#' @keywords methods
#' @method summary q3
#' @description S3 summary method for object of class\code{"q3"}
#' @param object object of class\code{"q3"}
#' @param maxrc numerical with default \code{maxrc=3} to specify the output of the maximum number of highest residual correlations in terms of absolute value.
#' @param ... other parameters passed trough
########################### hier die summary method fuer pair #############################
summary.q3<-function(object, maxrc=3, ...){
  cat("Yens (1984) Q3 statistic based on", object$resid_cor$type, "correlation: \n")
  cat("- missing treatment:", object$resid_cor$use, " \n")
  cat("- type of residuals used:", object$residuals$type, " \n")
  print(object$statistic$Q3)
  #object$resid_cor$cor
  cors <- unique(na.omit(c(object$resid_cor$cor)))
  names(cors) <- apply(X = (combn(colnames(object$resid_cor$cor),m = 2)),MARGIN = 2,FUN = function(x){paste(x,collapse = "~")})
  index <- names(sort(abs(cors),decreasing = T)[1:maxrc]) 
  cat("Results for the",maxrc,"highest residual correlations in terms of absolute value: \n")
  print(cors[index])
  invisible((list(statistic=object$statistic$Q3, maxrc=cors[index])))
}
