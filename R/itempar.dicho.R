#' @title Item Parameter dichotomous 1PL
#' @export itempar.dicho
#' @description Calculation of the item parameter for dichotomous items according the 1PL Rasch Model using a pairwise comparison algorithm (Choppin, 1968, 1985).
#' Missing values up to an high amount in data matrix are allowed, as long as items are proper linked together.
#'
#' 
#'@details Parameter calculation is based on the construction of a paired comparison matrix M\emph{nij} with entries f\emph{ij} representing the number of respondents who got item \emph{i} right and item \emph{j} wrong according to Choppin's (1968, 1985) conditional pairwise algorithm. This algorithm is simply realized by matrix multiplication.
#'
#' To avoid numerical problems with off diagonal zero's when constructing the pairwise comparison matrix M\emph{nij}, powers of the M\emph{nij} matrix, can be used (Choppin, 1968, 1985). Using powers \emph{k} of M\emph{nij} replaces the results of the direct comparisons between \emph{i} and \emph{j} with the sum of the indirect comparisons of \emph{i} and \emph{j} through an intermediate \emph{k}.
#' 
#'In general, it is recommended to use the argument with default value \code{pot=TRUE}.
#'
#'    
#' @param daten a data matrix, potentially with missing values, comprising dichotomous responses of respondents (rows) on some items (colums) coded in the 0 1 manner.
#' @param sortdif logical, if TRUE (default) items are sorted in an ascending order by difficulty for output.
#' @param pot logical, if TRUE (default) a power of three of the pairwise comparison matrix is used for further calculations.
#' @param zerocor logical, if TRUE (default) unobserved combinations (1-0, 0-1) in data for each pair of items are given a frequency of one conf. proposal by Alexandrowicz(2011, p.373).
#' @param ... additional parameters passed through
#' @return An object of class ippw containing item difficulties sigma.
#' @exportClass ippw
#' @references Choppin, B. (1968). Item Bank using Samplefree Calibration. \emph{Nature, 219}(5156), 870-872.
#' @references Choppin, B. (1985). A fully conditional estimation procedure for Rasch model parameters. \emph{Evaluation in Education, 9}(1), 29-42.
#' @references Alexandrowicz, R. W. (2011). 'GANZ RASCH': A Free Software for Categorical Data Analysis. \emph{Social Science Computer Review, 30}(3), 369-379.
#' 
#' @examples data(cog) # loading example data set
#' sigma<-itempar.dicho(daten=cog[,4:34], pot=TRUE) # calculating itemparameters for 31 math items
#' sigma
#' #######
#' plot(sigma) # plotting item difficulties
############## funktions beginn ########################################################
itempar.dicho<-function(daten, sortdif=TRUE, pot=TRUE, zerocor=TRUE, ...)
{
  ######### hier erste einzelne hilfsfunktionen 
  ### mat.mult - erweiterte matrix multiplikation mit NA's
  mat.mult<-function(A,B,na.rm=TRUE){
    # new version based on remarks by A. Robitzsch
    A<-as.matrix(A);B<-as.matrix(B)
    if(dim(A)[2]!=dim(B)[1]){stop("not conformable Matrices A B")}
    A[ is.na(A) ] <- 0
    B[ is.na(B) ] <- 0
    erg <- A %*% B
    return(erg)
  }
  
  ### nij.dicho - erstellen einer nij haeufigkeitsmatrix
  nij.dicho<-function(daten,...){
    Itemnames<-colnames(daten)
    nijmat<-t(mat.mult(A = t(!daten),B = daten )) 
    colnames(nijmat)<-Itemnames
    rownames(nijmat)<-Itemnames
    return(nijmat)
  }
  
  ### d3.mat - potenzieren von nij mit 3 und dann positiv reziproke matrix
  d3.mat<-function(nij, pot=TRUE, zerocor=TRUE){
    # new version based on remarks by A. Robitzsch
    Itemnames<-dimnames(nij)
    if(pot==TRUE){nij<-nij%*%nij%*%nij}
    # new zero correction as an option --> conf. R. Alexandrowicz(2011) p.373 
    if(zerocor==TRUE){nij[nij==0]<-1} 
    Dmat <- t(nij) / nij
    diag(Dmat)<-1
    dimnames(Dmat)<-Itemnames
    return (Dmat)
  }
  
  ### sig.dicho -
  sig.dicho<-function(Dmat,...){
    n<-dim(Dmat)[2]
    LDmat<-log(Dmat)
    sig<-rowSums(LDmat, na.rm=T)/n
    names(sig)<-dimnames(Dmat)[[2]]
    return(sig)
  } 
  
  ######### ende einzelne hilfsfunktionen 
  
  ######## check der Daten ------------------
  ### check und ausgabe von warnungen
  lowest_category<-(range(daten,na.rm=TRUE))[1]
  higest_category<-(range(daten,na.rm=TRUE))[2]
  stopifnot(lowest_category==0, higest_category==1)
  
  ##### aufbereiten der daten --------------- 
  daten<-as.matrix(daten)
  if(length(colnames(daten))==0){
    colnames(daten)<-paste("I",1:dim(daten)[2],sep="")  
    cat("no item names found in data" ,"\n", "items are named I1 (first column) to I",dim(daten)[2]," (last column)",sep="")
  }  
 
  ##### berechnung der ergebnisse ----------------
  erg<-(sig.dicho(d3.mat(nij.dicho(as.matrix(daten) ),pot=pot,zerocor=zerocor)))
  
  ##### aufbereitung der ergebnisse und ausgabe ----------------
  Itemnames<-colnames(daten)
  names(erg)<-Itemnames
  
  if(sortdif==TRUE){Sigma<-sort(erg)}
  if(sortdif==FALSE){Sigma<-erg}
  
  class(Sigma) <- c("numeric","ippw")
  return(Sigma)
  # summary.ippw(Sigma)
}  




