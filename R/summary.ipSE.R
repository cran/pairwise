#' @method summary
########################### hier die summary method #############################
summary.ipSE<-function(object, ...){
  cat("Item difficulties and std. errors: \n") 
  print(data.frame(object))
}