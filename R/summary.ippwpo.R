#' @method summary
########################### hier die summary method #############################
summary.ippwpo<-function(object, ...){
  cat("Item thresholds and difficulties Sigma: \n") 
  print(data.frame(object))
}