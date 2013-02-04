#' @method summary
########################### hier die summary method #############################
summary.ippw<-function(object, ...){
  Sigma<-matrix(object,ncol=1,dimnames = list(names(object), "Sigma"))
  cat("Item difficulties Sigma: \n") 
  print(Sigma)
}