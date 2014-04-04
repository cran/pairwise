#' @title Point Biserial Correlations
#' @export ptbis
#' @description Calculation of the point biserial correlations for dicho- or polytomous item categories with total scale (person parameter).
#'  
#' @details to come ...
#' @param y either an object of class \code{"pers"}, or an numeric vector as an result of any scaling approach (WLE, MLE, Rawscore, etc. ) relating to the Items (columns) in \code{daten}.    
#' @param daten if argument y is not an object of class \code{"pers"}, a \code{data.frame}, potentially with missing values, comprising dicho- or polytomous items (columns). 

#' @return An object of class "data.frame" and "ptbis" containing item statistics.
#' @exportClass ptbis
#' @examples ######################
#' ########
#' data(sim200x3) # loading reponse data
#' y <- rowSums(sim200x3)
#' ptbis(y=y, daten=sim200x3)
#' #### 
#' result <- pers(pair(sim200x3))
#' ptbis(y= result)

ptbis <- function(y, daten=NULL){# daten = data.frame mit polytomen variablen; y =skala
  
  if(any(class(y)=="pers")){
    daten <- as.data.frame(y$pair$resp)
    abil <- y$pers$WLE
  }
  if(is.vector(y)==TRUE){
    daten <- daten
    abil <- y
  }
  if(class(daten)!="data.frame"){stop("daten must be a data.frame")}
  stopifnot(dim(daten)[1]==length(abil))
  datenl <- as.list(daten)
  # erg <- lapply(datenl, polyptb, abil)
  erg <- t(as.data.frame(lapply(datenl, polyptb, abil)))
  return(erg)
}
