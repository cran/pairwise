#' @method logLik pers
#' @title S3 logLik for Object of class "pers"
#' @description S3 logLik method to extract the Log-Likelihood for object of class\code{"pers"} 
#' @param object object of class\code{"pers"}
#' @param sat a "logical" with default set to \code{sat=FALSE} to return the Log-Likelihood of the data for the unrestricted modell based on parameters estimated with function \code{\link{pers}}. If set to \code{sat=TRUE} the Log-Likelihood of the saturated model is returned instead.
#' @param ... not used jet.

########################### hier die logLik method fuer pers #############################
logLik.pers<-function(object, sat=FALSE, ...){
  
  # compute m_v - number of categories per item as vector - used in both cases
  m_v <-sapply(1:nrow(object$pair$threshold), function(i) {length(na.omit(object$pair$threshold[i,]))+1})
  
  if(sat==FALSE){ 
    # estimated model
    P <- pvx.super(theta_v=object, dat=TRUE)
    Log_Likelihood <- sum(log(P),na.rm=T) # evtl. : P[complete.cases(P),]
    # object$pair$threshold
    df <- (sum(m_v-1)-1)*2 # number of free model parameters (df)
    #     df_k <- sum(m_v-1)-1
    #     df_k_1 <- sum(m_v-1)-1
    nall <- dim(object$pair$resp)[1]
    nobs <- nall ### ev. check this !!!
    #structure(-213.902975399879, nall = 108L, nobs = 108, df = 8, class = "logLik")
    result <- structure(Log_Likelihood, nall = nall, nobs = nobs, df = df, class = "logLik") 
  }
  
  if(sat==TRUE){
    # saturated model
    #getestet mit WinMira 20-04-2013 --> stimmt!!!  
    x<-object$pair$resp
    #df.sat<-(m^k)-1 # degrees of fredom for saturated model
    df <- prod(m_v) # changed for polytomies
    b<-dim(x)[2]
    l<-dim(x)[1]
    ppaste <- function(z){paste(z, collapse = "")} # creates pattern strings
    zaehl<-(as.matrix(table(apply(x, 1, ppaste))))[ ,1] # compute values for numerator and exponent
    nen<-rep(l,times=length(zaehl))     # compute values for denominator
    lik.sat<-sum(log((zaehl/nen)^zaehl)) # compute log likelihood of saturated model
    result <- structure(lik.sat, nall = l, nobs = l, df = df, class = "logLik")   
  }
  return(result)
}