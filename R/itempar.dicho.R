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
#' @param ... additional parameters passed through
#' @return An object of class ippw containing item difficulties sigma.
#' @exportClass ippw
#' @references Choppin, B. (1968). Item Bank using Samplefree Calibration. \emph{Nature, 219}(5156), 870-872.
#' @references Choppin, B. (1985). A fully conditional estimation procedure for Rasch model parameters. \emph{Evaluation in Education, 9}(1), 29-42.
#' 
#' @examples data(cog) # loading example data set
#' sigma<-itempar.dicho(daten=cog[,4:34], pot=TRUE) # calculating itemparameters for 31 math items
#' sigma
#' #######
#' plot(sigma) # plotting item difficulties
############## funktions beginn ########################################################
itempar.dicho<-function(daten, sortdif=TRUE, pot=TRUE,...)
{
  ######### hier erste einzelne hilfsfunktionen 
  ### mat.mult - erweiterte matrix multiplikation mit NA's
  mat.mult<-function(A,B,na.rm=TRUE){
    A<-as.matrix(A);B<-as.matrix(B)
    if(dim(A)[2]!=dim(B)[1]){stop("not conformable Matrices A B")}
    erg<-matrix( ,ncol=dim(A)[1] ,nrow=dim(B)[2] )
    for (n in 1:dim(A)[1]){
      for (m in 1:dim(B)[2]){
        erg[n,m]<-sum(A[n,]*B[,m],na.rm=na.rm)
      }
    }
    return(erg)
  }
  
  ### nij.dicho - erstellen einer nij häufigkeitsmatrix
  nij.dicho<-function(daten,...){
    Itemnames<-colnames(daten)
    nijmat<-t(mat.mult(A = t(!daten),B = daten )) 
    colnames(nijmat)<-Itemnames
    rownames(nijmat)<-Itemnames
    return(nijmat)
  }
  
  ### d3.mat - potenzieren von nij mit 3 und dann positiv reziproke matrix
  d3.mat<-function(nij,pot=TRUE){
    Itemnames<-dimnames(nij)
    if(pot==T){nij<-nij%*%nij%*%nij}
    n<-dim(nij)[2]                    # Groeße der quadratischen nij Matrix feststellen
    mat<-matrix( , nrow=n, ncol=n)    # Ergebnis-Matrix Dmat vorbereiten
    for(i in 1:n)
    {
      for(j in i:n)
      {
        mat[i,j] <- nij[j,i]/nij[i,j]
        mat[j,i] <- nij[i,j]/nij[j,i]
      }
    }
    Dmat <- mat
    diag(Dmat) <- 1          # die NAs durch eins ersetzen
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
    colnames(daten)<-paste0("I",1:dim(daten)[2])  
    cat("no item names found in data" ,"\n", "items are named I1 (first column) to I",dim(daten)[2]," (last column)",sep="")
  }  
 
  ##### berechnung der ergebnisse ----------------
  erg<-(sig.dicho(d3.mat(nij.dicho(as.matrix(daten)))))
  
  ##### aufbereitung der ergebnisse und ausgabe ----------------
  Itemnames<-colnames(daten)
  names(erg)<-Itemnames
  
  if(sortdif==TRUE){Sigma<-sort(erg)}
  if(sortdif==FALSE){Sigma<-erg}
  
  class(Sigma) <- c("ippw")
  return(Sigma)
  # summary.ippw(Sigma)
}  




