#' @title Q3 Fit Statistic 
#' @export q3
#' @description Calculation of Q3 fit statistic for the rasch model based on the residuals, which was proposed by Yen (1984).
#'  
#' @details The lower level letter 'q' was used (intead of 'Q') for naming the function because the name 'Q3' was already used in another IRT package -- namly \code{TAM}. As perhaps some users like to use both packages simultaniously, an alternative naming convention was choosen for 'pairwise'. No other details in the moment.
#' @param pers_obj an object of class \code{"pers"} as a result from function \code{\link{pers}}.
#' @param na_treat value to be assigned to residual cells which have missing data in the original response matrix. default is set to \code{na_treat=0} to set the residuals to 0, which implys that they are imputed as 'fitting data', i.e., zero residuals. This can attenuate contrasts (see. http://www.rasch.org/rmt/rmt142m.htm). An option is to set it to \code{na_treat=NA}.
#' @param use a character string as used in function \code{\link{cor}} or \code{\link{cov}}, giving a method for computing covariances or correlations in the presence of missing values. This must be (an abbreviation of) one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs". The default is set to \code{use="complete.obs"} which will exclude cases by listwise deletion to keep the correlation matrix positive definit.
#' @param res a character string defining which type of (rasch--) residual to analyze when computing the correlations. This must be (exactly) one of the strings "sr" for score residuals , "stdr" for standardised residuals, "srsq" for score residuals squared, or "stdrsq" for standardised residuals squared. The default is set to \code{res="stdr"} refering to Linacre (1998).
#' @param method a character string as used in function \code{\link{cor}}, indicating which correlation coefficient  is to be computed. One of "pearson" (default), "kendall", or "spearman", can be abbreviated. The default is set to \code{method="pearson"}.
#' @return An object of class \code{c("Q3","list")}.
#' @references Yen, W. M. (1984). Effects of Local Item Dependence on the Fit and Equating Performance of the Three-Parameter Logistic Model. \emph{Applied Psychological Measurement, 8}(2), 125–145. https://doi.org/10.1177/014662168400800201
#' @exportClass Q3
#' @examples ######################
#' ########
#' data(bfiN) # loading reponse data
#' pers_obj <- pers(pair(bfiN))
#' result <- q3(pers_obj)
#' str(result) # to see whats in ;-)
#' #### 

q3 <- function(pers_obj, na_treat=0, use="complete.obs", res="stdr", method="pearson"){
  # returns a list with results for Q3 statistics
  # func. by joerg-henrik heine jhheine(at)googlemail.com
  # needs internal function expscore in i.expscore.R
  obj <- expscore(pers_obj, na_treat=na_treat) # calls internal function for residuals
  Eni <- obj$Eni # expected scores 
  Yni <- obj$Yni # "sr" - score residual  
  Zni <- obj$Zni # "stdr" - standardised residual 
  Y2ni <- obj$Y2ni # "srsq" - score Residual Squared
  Z2ni <- obj$Z2ni # "stdrsq" - standardised residual squared 
  # check of arguments 
  if( !(any(res==c("sr","stdr","srsq","stdrsq"))) ){stop("wrong type of residuals selected","\n", "check argument 'res'","\n")}
  #assign the selected residual type
  if(res=="sr"){resi <- Yni}
  if(res=="stdr"){resi <- Zni}
  if(res=="srsq"){resi <- Y2ni}
  if(res=="stdrsq"){resi <- Z2ni}
  # compute cor matrix using other aruments 
  r_mat <- cor(x=resi, use=use, method=method)
  Z_mat <- .5*(log((1+r_mat)/(1-r_mat))) ## fisher Z transform of r
  Z_mean <- mean(Z_mat[upper.tri(Z_mat,diag = FALSE)])
  Q3_mean <- (exp(Z_mean*2)-1)/(exp(Z_mean*2)+1) ## backtransform of mean Z to (mean) r
  Q3_max <- max(r_mat[upper.tri(r_mat,diag = FALSE)]) # max of positive correlations; JHH edit was: max(Z_mat[upper.tri(Z_mat,diag = FALSE)])
  Q3_min <- min(r_mat[upper.tri(r_mat,diag = FALSE)]) # max!! of ninimum correlations; JHH edit was: min(Z_mat[upper.tri(Z_mat,diag = FALSE)])
  Q3_abs_max <- (r_mat[upper.tri(r_mat,diag = FALSE)])[max(abs(r_mat[upper.tri(r_mat,diag = FALSE)]))==abs(r_mat[upper.tri(r_mat,diag = FALSE)])]# max of absolut cor. values - that is, what is the strongest association (either positive or negative); JHH edit was:  (Z_mat[upper.tri(Z_mat,diag = FALSE)])[max(abs(Z_mat[upper.tri(Z_mat,diag = FALSE)]))==abs(Z_mat[upper.tri(Z_mat,diag = FALSE)])]
  Q3_abs_min <- (r_mat[upper.tri(r_mat,diag = FALSE)])[min(abs(r_mat[upper.tri(r_mat,diag = FALSE)]))==abs(r_mat[upper.tri(r_mat,diag = FALSE)])] # min of absolut cor. values - that is, what is the lowest association (either positive or negative); JHH edit was: (Z_mat[upper.tri(Z_mat,diag = FALSE)])[min(abs(Z_mat[upper.tri(Z_mat,diag = FALSE)]))==abs(Z_mat[upper.tri(Z_mat,diag = FALSE)])]
  `Q3*` <- Q3_max - Q3_mean # this is obviously wrong, becaus it ignores negative correlations, but exactly the thing proposed in the literatur
  rmat <- r_mat; diag(rmat) <- NA # change digaonal to NA for output
  n_cor <- crossprod(1-is.na(resi))
  ############ return section 
  erg <- list(pers_obj=pers_obj, residuals=list(resid=resi,type=res), resid_cor=list(cor=rmat,n_cor=n_cor,type=method,na_treat=na_treat,use=use), statistic=list(Q3=c(mean=Q3_mean, max=Q3_max, min=Q3_min, max_abs=Q3_abs_max, min_abs=Q3_abs_min,`Q3*`=`Q3*`) ) )
  class(erg) <- c("q3","list")
  return(erg)
}
