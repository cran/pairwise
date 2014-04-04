#' @method summary pers
#' @title S3 Summary for Thetas
#' @description S3 summary Method for object of class\code{"pers"}
#' @param object object of class\code{"pers"}
#' @param sortwle logical wether to order persons by ability
#' @param ... other parameters passed trough

########################### hier die summary method fuer pers #############################
summary.pers<-function(object, sortwle=FALSE, ...){
  
# if(scrgr==FALSE){
  cat("WLE Estimates and SE by Persons: \n") 
  personen <- object$pers
  items <- object$pair
  result <- as.data.frame(personen[1:6])
  rownames(result) <- personen[["persID"]]
  if(sortwle==TRUE){ result <- result[order(result[,5] ),]  
                     cat("(ordered by Theta) \n")
  }
  print(result)

  
# }
# if(scrgr==TRUE){
#   cat("WLE Estimates and SE Scoregroups Persons: \n") 
#   personen <- object$pers
#   items <- object$pair
#   e1 <- as.data.frame(personen[1:6])
#   rownames(e1) <- personen[["persID"]]
#   e1 <- e1[order(e1[,5] ),] 
#   
#   e2 <- e1[,c(4,5,6)]
#   
#   unique(apply(as.matrix(e1[,c(1,2)]),1, function(x){paste(x,collapse="")}))
#   
#   aggregate(x=e1, by=list( raw = e1[,"raw"],  book = e1[,"book"], NA.group = e1[,"NA.group"]), FUN=unique, simplify = FALSE)# , ..., simplify = TRUE)
  
}