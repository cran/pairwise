#' @title Item Fit Indicees
#' @export pairwise.item.fit
#' @exportClass pairwise_item_fit
#' @description function for calculating item fit indicees
#' @details contrary to many IRT software using Ml based item parameter estimation, \code{pairwise} will not exclude persons, showing perfect response vectors (e.g. c(0,0,0) for dataset with three variables), prior to the scaling. Therefor the fit statistics computed with \code{pairwise} may deviate somewhat from the fit statistics produced by IRT software using Ml based item parameter estimation (e.g. R-package \code{eRm}, depending on the amount of persons with perfect response vectors in the data.
#' @param pers_obj an object of class \code{"pers"} as a result from function \code{\link{pers}}
#' @return an object of class \code{c("pairwise_item_fit", "data.frame")} contaning item fit indicees.
#' @examples ########
#' data(sim200x3)
#' result <- pers(pair(sim200x3))
#' pairwise.item.fit(pers_obj=result) # item fit statistic
####################################################
####################################################


pairwise.item.fit <- function(pers_obj){
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
Nna_v <- colSums(!is.na(emp_resp))

Chi <- colSums(Z2ni,na.rm=TRUE) # ... gegencheck eRm (ifit in itemfit.ppar) OK
df <- Nna_v-1 # sowieso besser als eRm, da wird das -1 vergessen  
pChi <- 1-pchisq(Chi, df) # p-value  
#loc.chi.square.p<-1-pchisq(loc.chi.square,loc.df)  
#-----------------------------------------------------------------

## Variance Uq2i of -> Unweighted Mean Square Ui () -------
Uq2i  <-  (colSums( (Cni / (Wni^2)), na.rm = TRUE) / (Nna_v)^2 - (1/Nna_v)   ) # ... gegencheck eRm (qsq.outfitMSQ in itemfit.ppar) OK 
Uqi <- sqrt(Uq2i)

## Unweighted Mean Square Ui (OUTFIT.MEANSQ)-------
# so macht es eRm als alternative (dritte stelle hintem komma versch.):   i.outfitMSQ <- Chi/df
Ui <- colSums(Z2ni, na.rm = TRUE)/Nna_v   # nicht N wegen missings!

## Standardised (Un)weighted Mean Square Ti (OUTFIT.ZSTD)-------
UTi <- ( ( (Ui^(1/3)) -1) * (3/Uqi) ) + (Uqi/3) # ... gegencheck eRm (i.outfitZ in itemfit.ppar) formel stimmt - werte leicht unterschiedlich - in eRm werden perfecte resp. vorher rausgeworfen OK


#-----------------------------------------------------------------

## Variance Vq2i of -> Weighted Mean Square Vi (INFIT) -------
Vq2i  <- colSums( (Cni - (Wni^2)), na.rm = TRUE) / (colSums(Wni, na.rm = TRUE)^2) # ... gegencheck eRm (qsq.infitMSQ in itemfit.ppar) OK
Vqi <- sqrt(Vq2i)

## Weighted Mean Square Vi (INFIT.MEANSQ)-------
# Vi <- colSums(Z2ni*Wni, na.rm = TRUE)/colSums(Wni, na.rm = TRUE) # eRm style -> identisch
Vi <- colSums(Y2ni, na.rm = TRUE) / colSums(Wni, na.rm = TRUE) # ... gegencheck eRm (i.infitMSQ in itemfit.ppar) OK

## Standardised Weighted Mean Square Ti (INFIT.ZSTD)-------
VTi <- ( (Vi^(1/3)-1) * (3/Vqi) ) + (Vqi/3) # ... gegencheck eRm (i.infitZ in itemfit.ppar) unterschiede! aber formel stimmt OK

#-----------------------------------------------------------------
erg <- as.data.frame(list(Chi=Chi, df=df, p=pChi, OUTFIT.MEANSQ=Ui , OUTFIT.ZSTD=UTi ,INFIT.MEANSQ=Vi, INFIT.ZSTD=VTi ))
class(erg) <- c("pairwise_item_fit","data.frame")
return( erg )
}
