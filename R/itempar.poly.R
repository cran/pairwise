#' @title Item Parameter for polytomous Partial Credit Model 
#' @export itempar.poly
#' @description Calculation of the item parameter for polytomous items according to the Partial Credit Model using a generalization of the pairwise comparison algorithm (Choppin, 1968, 1985).
#' Missing values up to an high amount in data matrix are allowed, as long as items are proper linked together.
#' 
#'@details Parameter calculation is based on the construction of a paired comparison matrix M\emph{nicjc} with entries f\emph{icjc} representing the number of respondents who answered to item \emph{i} in category \emph{c} and to item \emph{j} in category \emph{c-1} widening Choppin's (1968, 1985) conditional pairwise algorithm to polytomous item response formats. This algorithm is simply realized by matrix multiplication.
#'
#' To avoid numerical problems with off diagonal zero's when constructing the pairwise comparison matrix M\emph{nij}, powers of the M\emph{nicjc} matrix, can be used (Choppin, 1968, 1985). Using powers \emph{k} of M\emph{nicjc} - argument \code{pot=TRUE} (default), replaces the results of the direct comparisons between \emph{i} and \emph{j} with the sum of the indirect comparisons of \emph{i} and \emph{j} through an intermediate \emph{k}.
#' 
#'In general, it is recommended to use the argument with default value \code{pot=TRUE}.
#'
#'    
#' @param daten a data matrix with optionaly named colums (names of items) or a data.frame, potentially with missing values, comprising polytomous responses of respondents (rows) on some items (colums) coded starting with 0 for lowest category to \emph{m}-1 for highest category, with \emph{m} beeing the number of categories for all items.
#' @param m number of response categories for all items - by default \emph{m} is defined as \code{m = max(daten,na.rm=TRUE)+1}.
#' @param sortdif logical, if TRUE (default) items are sorted in an ascending order by difficulty for output.
#' @param pot logical, if TRUE (default) a power of three of the pairwise comparison matrix is used for further calculations.
#' @param zerocor logical, if TRUE (default) unobserved combinations (1-0, 0-1) in data for each pair of items are given a frequency of one conf. proposal by Alexandrowicz(2011, p.373).
#' @param ... additional parameters passed through.
#' @return A (list) object of class ippwpo containing the item category thresholds and difficulties sigma.
#' @exportClass ippwpo
#' @references Choppin, B. (1968). Item Bank using Samplefree Calibration. \emph{Nature, 219}(5156), 870-872.
#' @references Choppin, B. (1985). A fully conditional estimation procedure for Rasch model parameters. \emph{Evaluation in Education, 9}(1), 29-42.
#' @references Alexandrowicz, R. W. (2011). 'GANZ RASCH': A Free Software for Categorical Data Analysis. \emph{Social Science Computer Review, 30}(3), 369-379.
#' 
#' @examples data(bfiN) # loading example data set
#' # calculating itemparameters for 5 neuroticism items with 6 answer categories (0-5).
#' neuro_itempar<-itempar.poly(daten = bfiN, m = 6) 
#' neuro_itempar
#' ################
#' # plotting threshold profiles for 5 neuroticism items.
#' # 6 categories - 5 thresholds
#' plot(neuro_itempar) 
############## funktions beginn ########################################################
itempar.poly<-function(daten, m=max(daten,na.rm=TRUE)+1, sortdif=TRUE, pot=TRUE, zerocor=TRUE, ...)
{
######### hier erste einzelne hilfsfunktionen 
  # schritt 1 ## nij.poly - erstellen einer nij häufigkeitsmatrix
  nij.poly <-function(daten,m,...){
    ########## hier erst hilfsfunktion für nij.poly
    ### - mat.mult - erweiterte matrix multiplikation mit NA's
    mat.mult<-function(A,B,na.rm=TRUE){
      # new version based on remarks by A. Robitzsch
      A<-as.matrix(A);B<-as.matrix(B)
      if(dim(A)[2]!=dim(B)[1]){stop("not conformable Matrices A B")}
      A[ is.na(A) ] <- 0
      B[ is.na(B) ] <- 0
      erg <- A %*% B
      return(erg)
    }
    ########## ende der hilfsfunktion für nij.poly
  items <- dim(daten)[2]
  thx <- m-1                                            # anzahl der thresholds
  jvec<-c(0:(thx-1))                                    # Erzeugung eines Schwellen-Vergleichsvectors j
  ivec<-c(1:thx)                                        # Erzeugung eines Schwellen-Vergleichsvectors i
  lst<-list()                                           # Erzeugen der Zwischenergebnisliste für die Matrizen
  nijmat<-matrix(,dim(daten)[2]*thx,dim(daten)[2]*thx)  # quadratische nij Ergebnismatrix mit NAs - Anzahl der Items mal Anzahl der Schwellen
  for (ii in 1:thx){ i<-ivec[ii]
    for (jj in 1:thx){ j<-jvec[jj]
      #lst[[paste(i,j,sep="")]] <- (t(daten==i)%*%(daten==j)) # alte version 
      lst[[paste(i,j,sep="")]] <- (mat.mult(A = t(daten==i),B = daten==j )) 
      diag(lst[[paste(i,j,sep="")]]) <- 0
    }
  }
  # aufbereiten des zwischenergebnisses lst in eine nij matrix   
  for (item in 0:(items-1)){ # Itemschleife
    for (Z in 1:(thx)){ # Zeilenschleife
      vec<-NULL
      for (S in 0:(thx-1)){ # Spaltenschleife
        vec<-cbind(vec,lst[[paste(Z,S,sep="")]][(item+1),]) # ZeilenVektor über thx spalten generieren
      } # Ende Spaltenschleife
      nijmat[(item*thx)+Z, ]<-as.vector(t(vec))
    } # Ende Zeilenschleife
  } # Ende Itemschleife
  return(nijmat)
} # Ende der Funktion
  
  # schritt 2 ## cube - potenzieren einer matrix mit 3
  cube<-function(M){
    Mp<-M%*%M%*%M
    return(Mp)
  }
  
  # schritt 3 ## Dmat - neue funktion zur erzeugung der positivreziproken matrix
  # new version based on remarks by A. Robitzsch
  Dmat<-function(nij){  
    dmat <- t(nij) / nij
    return (dmat) 
  }
######### ende der hilfsfunktionen 

######## check der Daten ------------------
  ### check und ausgabe von warnungen
  lowest_category<-(range(daten,na.rm=TRUE))[1]
  higest_category<-(range(daten,na.rm=TRUE))[2]
  stopifnot(lowest_category==0, higest_category==m-1)
##### aufbereiten der daten --------------- 
  daten<-as.matrix(daten)
  if(length(colnames(daten))==0){
    colnames(daten)<-paste("I",1:dim(daten)[2],sep="") 
    cat("no item names found in data" ,"\n", "items are named I1 (first column) to I",dim(daten)[2]," (last column)",sep="")
  }    
   

##### berechnung der ergebnisse ----------------
  k<-dim(daten)[2] # anzahl der Items feststellen 
  if(pot==FALSE){ nij<- (nij.poly(daten,m)) }
  if(pot==TRUE){ nij<- cube(nij.poly(daten,m)) }
  
  # new zero correction as an option --> conf. R. Alexandrowicz(2011) p.373 
  if(zerocor==TRUE){nij[nij==0]<-1}
  
  tau<-rowMeans(log(Dmat(nij)), na.rm = TRUE)

  thresholds<-t(matrix(tau,ncol = k))
  # matplot(thresholds,typ="b",pch=c(1,2,3))
  sigma <- rowMeans(thresholds)  
  # matlines(sigma,typ="b",pch=c(4)) 
  
##### aufbereitung der ergebnisse und ausgabe ----------------  
  Itemnames<-colnames(daten)
  rownames(thresholds)<-Itemnames
  colnames(thresholds)<-paste(c(1:(m-1)),sep="") # von paste("threshold ",c(1:(m-1)),sep="") geaendert in 
  names(sigma)<-Itemnames
  if(sortdif==TRUE){
    thresholds <- thresholds[order(sigma), ]
    sigma <- sort(sigma)
  }
  erg<-list(threshold=thresholds,sigma=sigma) # listen element heisst jetzt threshold nicht thresholds 
  class(erg) <- c("ippwpo","list")
  return(erg)
  # summary.ippwpo(erg) 
}  




