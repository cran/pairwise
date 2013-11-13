#' @title ML Theta Parameter dichotomous 1PL
#' @export thetaML.dicho
#' @description Calculation of the theta person parameter for dichotomous items, given the item parameters and one respose vector according the 1PL Rasch Model using an ML approach.
#' @details as this function is usualy not called directly by the user, there are no checks for plausibillity of the arguments !. Instead of calling this function directly, consider using the function \code{\link{ppML.dicho}}. 
#'
#' @param response numeric vector with responses of a person coded in the (0,1) manner.
#' @param sigma numeric vector with item (difficulty) parameters.
#' @param na_treat numeric defining the type of treatment to missing responses in the argument \code{response}. If set to \code{na_treat=0} (default) missing responses are treated as wrong answers. An option is to set \code{na_treat} to any value between 0 and 1 e.g.: \code{na_treat=0.5} which is guessing probability in dichotomous case.  
#' @param theta_init numeric giving the startvalue for estimation of theta. Default is \code{log()} of the persons rawscore (while NAs are removed for rawscore calculation).
#' @param limit numeric giving the limit at which accuracy the ML-algorithm stops.
#' @param iter numeric giving the maximum numer of iteration to perform.
#' @return An object of class thetaMLdicho containing the person parameter.
#' @exportClass thetaMLdicho
#' @examples ######## ... ####
thetaML.dicho <- function(response,sigma,na_treat=0,theta_init=log(sum(response,na.rm=TRUE)),limit=0.00001,iter=20){
#----- internal helper function MLE-------------------  
difference <- function(sigma,theta,response){
		# 1PL modell 
		P <- (exp(theta-sigma)) / (1+(exp(theta-sigma)))
		P_odd <- P*(1-P)
		####
		Oben <-  sum(P_odd*((response-P)/P_odd))
		Unten <-  sum(P_odd)
		list(delta = Oben/Unten,odd = Unten)
		}
#-------- keep original response
orig_response<-response

#----- estimation process -------------------  
n_iter <- 0 
converge <- TRUE
# only ML valid response vectors (no perfect response vectors)
if( (any( (na.omit(response)) ==1) & any( (na.omit(response)) ==0)) ) {
  res <- list() # res <- rep(NA,iter)
  response[is.na(response)]<-na_treat # assign na treatment to missing responses
  repeat{
    delta <- difference(sigma,theta=theta_init,response)$delta
    # if(is.na(delta)) { theta_init <- NA; SE <- NA; break }
    if(abs(delta)>2){delta <- delta/abs(delta) * 2} # keeps estimation reasonable
    theta_iter <- theta_init + delta # add difference to new initial theta
    res[[n_iter+1]] <- theta_iter # save result of iteration step
    theta_init <- theta_iter # assigns new start value for next iteration step
    n_iter <- n_iter+1 # count iteration
    if( (n_iter >= iter) & (abs(delta) > limit) ) {converge <- FALSE} # set converge flag to FALSE if limit not reached
    if(abs(delta) < limit | n_iter >= iter) {
      SE <- 1/sqrt(difference(sigma,theta=theta_init,response)$odd)
      break
    }
  } 
}

if(all( (na.omit(response)) ==1)){theta_init <- Inf;  SE <- NA; n_iter=0;res<-list(NA);converge<-NA} # perfect response vector upper 
if(all( (na.omit(response)) ==0)){theta_init <- -Inf; SE <- NA; n_iter=0;res<-list(NA);converge<-NA} # perfect response vector lower

result <- list("response"=orig_response,"theta"=theta_init,"SE"=SE,"iterations"=n_iter,"estimations"=res, "converge"=converge)
class(result) <- c("list","thetaMLdicho")

return(result)
}

