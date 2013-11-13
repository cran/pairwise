#' @method summary
########################### hier die summary method für ppMLdicho #############################
summary.ppMLdicho<-function(object, detailed=TRUE, ...){
  if(detailed==TRUE){ 
    result<-object[ ,c("pattern","theta","SE","raw","converge","iterations")]
    cat("detailed result of person parameter estimation: \n") 
    return(result)
  }
  
  if(detailed==FALSE){ 
    result<-object[ ,c("theta","SE")]
    cat("result of person parameter estimation: \n") 
    return(result)
  }
}