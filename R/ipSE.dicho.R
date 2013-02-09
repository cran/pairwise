#' @title Item Parameter calaculation with Standard Errors for dichotomous 1PL Model
#' @export ipSE.dicho
#' @description Calculation of the item parameter (Sigma) and their standard error (SE) for dichotomous items according the 1PL Rasch Model using a pairwise comparison algorithm (Choppin, 1968, 1985).
#' Missing values up to an high amount in data matrix are allowed, as long as items are proper linked together.
#'
#' 
#'@details Item Parameter calculation is based on the construction of a paired comparison matrix M\emph{nij} with entries f\emph{ij} representing the number of respondents who got item \emph{i} right and 
#' item \emph{j} wrong according to Choppin's (1968, 1985) conditional pairwise algorithm. 
#' This algorithm is simply realized by matrix multiplication.
#'
#' To avoid numerical problems with off diagonal zero's when constructing the pairwise comparison matrix M\emph{nij}, powers of the M\emph{nij} matrix, can be used (Choppin, 1968, 1985). Using powers \emph{k} of M\emph{nij} replaces the results of the direct comparisons between \emph{i} and \emph{j} with the sum of the indirect comparisons of \emph{i} and \emph{j} through an intermediate \emph{k}.
#' 
#'In general, it is recommended to use the argument with default value \code{pot=TRUE}.
#'
#'@section A Note on Standard Errors: Estimation of standard errors is done by repeated calculation of item parameters for subsamples of the given data. This procedure is mainly controlled by the arguments \code{nsample} and \code{size} (see arguments). With regard to calculation time, the argument \code{nsample} is the 'time killer'. On the other hand, things (estimation of standard errors) will not necessarily get better when choosing large values for \code{nsample}. For example choosing \code{nsample=400} will only result in minimal change for standard error estimation in comparison to (\code{nsample=30}) which is the default setting (see examples).      
#'
#' @param daten The response data as a data.frame or a matrix, potentially with missing values, comprising dichotomous responses of respondents (rows) on some items (colums) coded in the 0 1 manner.
#' 
#' @param sortdif logical, if TRUE (default) items are sorted in an ascending order by difficulty for output.
#' 
#' @param nsample numeric specifying the number of subsamples sampled from data, which is the number of replications of the parameter calculation. 
#' 
#' WARNING! specifying high values for \code{nsample} ( > 100 ) may result in long computing time without leadig to "better" estimates for SE. This may also be the case when choosing argument \code{size="jack"} (see argument \code{size}) in combination with large datasets (\emph{N} > 500).
#'   
#' @param size numeric with valid range between 0 and 1 (but not exactly 0 or 1) specifying the size of the subsample of \code{data} when bootstraping for SE estimation. As an alternative, \code{size} can be set to the character \code{"jack"} (\code{size="jack"}). This will set the subsample size to \emph{N}-1 and set \code{nsample=N} (see argument \code{nsample}), with \emph{N} beeing the number of persons in \code{daten}.
#'  
#' @param seed numeric used for \code{set.seed(seed)}.
#' 
#' @param pot logical, if TRUE (default) a power of three of the pairwise comparison matrix is used for further calculations.
#' 
#' @param ... additional parameters passed through.
#' 
#' @return An object of class ipSE containing the item difficulty parameter Sigma and standard errors for item difficulties Sigma.
#' @exportClass ipSE
#' @references Choppin, B. (1968). Item Bank using Samplefree Calibration. \emph{Nature, 219}(5156), 870-872.
#' @references Choppin, B. (1985). A fully conditional estimation procedure for Rasch model parameters. \emph{Evaluation in Education, 9}(1), 29-42.
#' 
#' @examples data(cog) # loading example data set
#' 
#' # calculating itemparameters and their SE for 31 math items
#' se_sigma<-ipSE.dicho(daten=cog[,4:34], pot=TRUE) 
#' 
#' summary(se_sigma) # summary for result
#' 
#' # plotting item difficulties with a CI = 95% 
#' plot(se_sigma) 
#' 
#' # use different color for CI
#' plot(se_sigma, col.error="green")
#' 
#' # without CI bars
#' plot(se_sigma,ci=0) 
#' 
#' ###### example from details section 'Some Notes on Standard Errors' ########
#' # se_sigma_400<-ipSE.dicho(daten=cog[,4:34], pot=TRUE,nsample=400)
#' # plot(se_sigma) 
#' # plot(se_sigma_400) 
#' 
############## funktions beginn ########################################################
ipSE.dicho<-function(daten, sortdif=TRUE, nsample=30, size=0.50, seed="no", pot=TRUE, ...){
# Berechnung von Standardfehlern nach dem Bootstrap Verfahren 

##### check der Daten ---------------------
lowest_category<-(range(daten,na.rm=TRUE))[1]
higest_category<-(range(daten,na.rm=TRUE))[2]
stopifnot(lowest_category==0, higest_category==1)

##### aufbereiten der daten --------------- 
daten<-as.matrix(daten)
if(length(colnames(daten))==0){
  colnames(daten)<-paste("I",1:dim(daten)[2],sep="")  
  cat("no item names found in data" ,"\n", "items are named I1 (first column) to I",dim(daten)[2]," (last column)",sep="")
}  
  
  N<-dim(daten)[1] # anzahl personen
  k<-dim(daten)[2] # anzahl items
  if(mode(size)=="character") {if(size=="jack"){nsize<-N-1 ; nsample=N } }
  if(mode(size)=="numeric") {if(size >= 1 | size <= 0 ) stop("size should have values between 0 and 1")  ;nsize<-round(N*size)}
  if(mode(seed)=="numeric") {set.seed(seed)}
  
  ergmat<-matrix( , ncol=k ,nrow=nsample)
  for (i in 1:nsample){
  sx<-daten[sample(1:dim(daten)[1],nsize),] # ziehen einer stichprobe mit größe size aus daten  
  cat("sample ", i , "of",nsample, "with size n =",nsize,"\n")
  ergmat[i,]<-itempar.dicho(sx,sortdif=FALSE, pot=pot)
  }  
  erglist<-as.list(data.frame((ergmat)))
  erg<-sapply(erglist,sd,na.rm=TRUE)
  uoSigma<-itempar.dicho(daten,sortdif=FALSE, pot=pot) # berechnen von Sigma zur Sortierung

##### aufbereitung der ergebnisse und ausgabe ----------------
Itemnames<-colnames(daten)
names(erg)<-Itemnames
names(uoSigma)<-Itemnames

if(sortdif==TRUE){SE<-erg[order(uoSigma)] ; Sigma<-sort(uoSigma)  }
if(sortdif==FALSE){SE<-erg ; Sigma<-uoSigma}

itpSE<-list(Sigma=Sigma, SE=SE)
class(itpSE) <- c("ipSE","list")

return(itpSE)
# summary.ipSE(itpSE)
}
