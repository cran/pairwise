#' @title Person Fit Indicees
#' @export pairwise.person.fit
#' @exportClass pairwise_person_fit
#' @description function for calculating person fit indicees
#' @details contrary to many IRT software using Ml based item parameter estimation, \code{pairwise} will not exclude persons, showing perfect response vectors (e.g. c(0,0,0) for dataset with three variables), prior to the scaling. Therefor the fit statistics computed with \code{pairwise} may deviate somewhat from the fit statistics produced by IRT software using Ml based item parameter estimation (e.g. R-package \code{eRm}, depending on the amount of persons with perfect response vectors in the data.
#' @param pers_obj an object of class \code{"pers"} as a result from function \code{\link{pers}}.
#' @return an object of class \code{c("pairwise_person_fit", "data.frame")} contaning person fit indicees.
#' @examples ########
#' data(sim200x3)
#' result <- pers(pair(sim200x3))
#' pairwise.person.fit(pers_obj=result) # item fit statistic
####################################################
####################################################


pairwise.person.fit <- function(pers_obj){
  # needs internal functions pvx,  pvx.matrix and expscore
  obj <- expscore(pers_obj) # calls internal function  
  emp_resp <- pers_obj$pair$resp
  Eni <- obj$Eni # expected scores (Expected Mean of ...) gegencheck eRm OK
  Wni <- obj$Wni # Variance of ... gegencheck eRm OK
  Cni <- obj$Cni # Kurtosis of ... gegencheck eRm OK
  Yni <- obj$Yni # score residual ... gegencheck eRm OK
  Zni <- obj$Zni # standardised residual ... gegencheck eRm (st.res in itemfit.ppar) OK
  Y2ni <- obj$Y2ni 
  Z2ni <- obj$Z2ni #standardised residual squared ... gegencheck eRm (sq.res in itemfit.ppar) OK
#-----------------------------------------------------------------
# Nna_v <- colSums(!is.na(Z2ni))
P_Nna_v <- rowSums(!is.na(emp_resp))

P_Chi <- rowSums(Z2ni,na.rm=TRUE) # ... gegencheck eRm (pfit in personfit.ppar) OK
P_df <- P_Nna_v-1 # OK  
P_pChi <- 1-pchisq(P_Chi, P_df) # p-value   
#-----------------------------------------------------------------
## Variance Uq2i of -> Unweighted Mean Square Ui () -------
P_Uq2i  <-  (rowSums( (Cni / (Wni^2)), na.rm = TRUE) / (P_Nna_v)^2 - (1/P_Nna_v)   ) # ... gegencheck eRm (qsq.outfitMSQ in personfit.ppar) ~ OK 
P_Uqi <- sqrt(P_Uq2i)

## Unweighted Mean Square Ui (OUTFIT.MEANSQ)-------
# so macht es eRm als alternative (dritte stelle hintem komma versch.):   i.outfitMSQ <- Chi/df
P_Ui <- rowSums(Z2ni, na.rm = TRUE)/P_Nna_v   # nicht m wegen missings!

## Standardised (Un)weighted Mean Square Ti (OUTFIT.ZSTD)-------
P_UTi <- ( ( (P_Ui^(1/3)) -1) * (3/P_Uqi) ) + (P_Uqi/3) # ... gegencheck eRm (p.outfitZ in itemfit.ppar) formel stimmt - werte leicht unterschiedlich - in eRm werden perfecte resp. vorher rausgeworfen OK

#-----------------------------------------------------------------

## Variance Vq2i of -> Weighted Mean Square Vi (INFIT) -------
P_Vq2i  <- rowSums( (Cni - (Wni^2)), na.rm = TRUE) / (rowSums(Wni, na.rm = TRUE)^2) # ... gegencheck eRm (qsq.infitMSQ in itemfit.ppar) OK
P_Vqi <- sqrt(P_Vq2i)

## Weighted Mean Square Vi (INFIT.MEANSQ)-------
P_Vi <- rowSums(Y2ni, na.rm = TRUE) / rowSums(Wni, na.rm = TRUE) # ... gegencheck eRm (p.infitMSQ in personfit.ppar) OK

## Standardised Weighted Mean Square Ti (INFIT.ZSTD)-------
P_VTi <- ( (P_Vi^(1/3)-1) * (3/P_Vqi) ) + (P_Vqi/3) # ... gegencheck eRm (p.infitZ in personfit.ppar) unterschiede! aber formel stimmt OK

#-----------------------------------------------------------------
erg <- as.data.frame(list(Chi=P_Chi, df=P_df, p=P_pChi, OUTFIT.MEANSQ=P_Ui , OUTFIT.ZSTD=P_UTi ,INFIT.MEANSQ=P_Vi, INFIT.ZSTD=P_VTi ))
class(erg) <- c("pairwise_person_fit","data.frame")
return( erg )
}
