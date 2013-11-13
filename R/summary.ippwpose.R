#' @method summary
########################### hier die summary method #############################
summary.ippwpoSE<-function(object, ...){
  cat("Item thresholds, difficulties and their std. errors: \n") 
  print(  t(data.frame(object))[  unlist(lapply((c(1:(dim(t(data.frame(object)))[1]/2))),function(x){seq(from=x,by=(dim(t(data.frame(object)))[1]/2),length.out=2)}))  ,]   )
}
