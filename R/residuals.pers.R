#' @method residuals pers
#' @title S3 residuals for Object of class "pers"
#' @description S3 residuals method to extract the (Rasch) residuals for object of class\code{"pers"} 
#' @param object object of class\code{"pers"}
#' @param res a character string defining which type of (rasch–) residual to return. This must be (exactly) one of the strings "exp" for expected scores "sr" for score residuals (default), "stdr" for standardised residuals, "srsq" for score residuals squared, or "stdrsq" for standardised residuals squared. The default is set to res="sr".
#' @param na_treat value to be assigned to residual cells which have missing data in the original response matrix. Default is set to na_treat=0 to set the residuals to 0, which implys that they are imputed as 'fitting data', i.e., zero residuals. This can attenuate contrasts (see. http://www.rasch.org/rmt/rmt142m.htm). An option is to set it to na_treat=NA.
#' @param ... not used jet.

########################### hier die residual method fuer pers #############################
residuals.pers<-function(object, res="sr", na_treat=0, ...){
  obj <- expscore(pers_obj=object, na_treat=na_treat) # calls internal function for residuals
  Eni <- obj$Eni # expected scores 
  Yni <- obj$Yni # "sr" - score residual  
  Zni <- obj$Zni # "stdr" - standardised residual 
  Y2ni <- obj$Y2ni # "srsq" - score Residual Squared
  Z2ni <- obj$Z2ni # "stdrsq" - standardised residual squared 
  # check of arguments 
  if( !(any(res==c("exp","sr","stdr","srsq","stdrsq"))) ){stop("wrong type of residuals selected","\n", "check argument 'res'","\n")}
  #assign the selected residual type
  if(res=="exp"){retur <- Eni}
  if(res=="sr"){retur <- Yni}
  if(res=="stdr"){retur <- Zni}
  if(res=="srsq"){retur <- Y2ni}
  if(res=="stdrsq"){retur <- Z2ni}
  class(retur) <- c("residuals","matrix")
  return(retur)
}
