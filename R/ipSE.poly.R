#' @title Item Parameter calculation with Standard Errors for polytomous Partial Credit  Model
#' @export ipSE.poly
#' @description Calculation of the item threshold, the difficulty and their standard errors (SE) respectively for polytomous items according to the Partial Credit Model.
#' All parameters are calculatetd using a generalization of the pairwise comparison algorithm (Choppin, 1968, 1985).
#' Missing values up to an high amount in data matrix are allowed, as long as items are proper linked together.
#'
#' 
#'@details Parameter calculation is based on the construction of a paired comparison matrix M\emph{nicjc} with entries f\emph{icjc}, representing the number of respondents who answered to item \emph{i} in category \emph{c} and to item \emph{j} in category \emph{c-1} widening Choppin's (1968, 1985) conditional pairwise algorithm to polytomous item response formats. 
#' This algorithm is simply realized by matrix multiplication.
#'  
#'Estimation of standard errors is done by repeated calculation of item parameters for subsamples of the given data. 
#'
#' To avoid numerical problems with off diagonal zeros when constructing the pairwise comparison matrix M\emph{nicjc}, powers of the M\emph{nicjc} matrix, can be used (Choppin, 1968, 1985). Using powers \emph{k} of M\emph{nicjc}, argument \code{pot=TRUE} (default), replaces the results of the direct comparisons between \emph{i} and \emph{j} with the sum of the indirect comparisons of \emph{i} and \emph{j} through an intermediate \emph{k}.
#' 
#'In general, it is recommended to use the argument with default value \code{pot=TRUE}.
#'
#'@section A Note on Standard Errors: Estimation of standard errors is done by repeated calculation of item parameters for subsamples of the given data. This procedure is mainly controlled by the arguments \code{nsample} and \code{size} (see arguments). With regard to calculation time, the argument \code{nsample} is the 'time killer'. On the other hand, things (estimation of standard errors) will not necessarily get better when choosing large values for \code{nsample}. For example choosing \code{nsample=400} will only result in minimal change for standard error estimation in comparison to (\code{nsample=30}) which is the default setting (see examples). 
#'    
#' @param daten a data matrix with optionaly named colums (names of items) or a data.frame, potentially with missing values, comprising polytomous responses of respondents (rows) on some items (colums) coded starting with 0 for lowest category to \emph{m}-1 for highest category, with \emph{m} beeing the number of categories for all items.
#' @param m number of response categories for all items - by default \emph{m} is defined as \code{m = max(daten,na.rm=TRUE)+1}.
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
#' @param verbose logical, if verbose = TRUE (default) a message about subsampling whe calculation standrderrors is sent to console.
#' 
#' @param ... additional parameters passed through.
#' 
#' @return A (list) object of class ippwpoSE containing the item category thresholds, difficulties sigma and their standrd errors.
#' @exportClass ippwpoSE
#' @references Choppin, B. (1968). Item Bank using Samplefree Calibration. \emph{Nature, 219}(5156), 870-872.
#' @references Choppin, B. (1985). A fully conditional estimation procedure for Rasch model parameters. \emph{Evaluation in Education, 9}(1), 29-42.
#' 
#' @examples data(bfiN) # loading example data set
#'
#' # calculating itemparameters and their SE for 5 neuroticism items with 6 answer categories (0-5).
#' neuro_itempar<-ipSE.poly(daten = bfiN, m = 6) 
#' 
#' summary(neuro_itempar) # summary for result
#' 
#' # plotting item thresholds with with their CI = 95% 
#' plot(neuro_itempar)
#' 
#' ###### example from details section 'Some Notes on Standard Errors' ########
#' # neuro_itempar_400<-ipSE.poly(daten = bfiN, m = 6,nsample=400)
#' # plot(neuro_itempar) 
#' # plot(neuro_itempar_400) 
#'    
############## funktions beginn ########################################################
ipSE.poly<-function(daten, m=max(daten,na.rm=TRUE)+1, sortdif=TRUE, nsample=30, size=0.50, seed="no", pot=TRUE, verbose=TRUE, ...){
# Berechnung von Standardfehlern nach dem Bootstrap Verfahren 

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

##### berechnung der ergebnisse SE----------------  
## für SE   
  N<-dim(daten)[1] # anzahl personen
  k<-dim(daten)[2] # anzahl items
  if(mode(size)=="character") {if(size=="jack"){nsize<-N-1 ; nsample=N } }
  if(mode(size)=="numeric") {if(size >= 1 | size <= 0 ) stop("size should have values between 0 and 1")  ;nsize<-round(N*size)}
  if(mode(seed)=="numeric") {set.seed(seed)}
  
  ergli<-vector("list", length=nsample)                  #list( , ncol=k ,nrow=nsample)
  for (i in 1:nsample){
  sx<-daten[sample(1:dim(daten)[1],nsize),] # ziehen einer stichprobe mit größe size aus daten  
  if(verbose==TRUE){cat("sample ", i , "of",nsample, "with size n =",nsize,"\n")}
  ####################################################################
  ergli[[i]]<-itempar.poly(sx, m = m, sortdif=FALSE, ...) ###
  ####################################################################
  }  
# diverse umsortierungen der berechnungsergebnisse SE   
temp1<-t(data.frame((ergli)))
sorter<-unlist(lapply((c(1:m)),function(x){seq(from=x, by=m  ,length.out=nsample)} ))
temp2<-temp1[sorter, ]
temp3<-vector("list", length=m)
for(i in 1:m){
  von<-(((i-1)*nsample)+1); 
  bis<-(i*nsample)
  temp3[[i]]<-temp2[von:bis,]
}
SEerg<-sapply(temp3,function(x){apply(x, 2, sd,na.rm=TRUE)})
colnames(SEerg)<-c(paste("threshold.",1:(m-1),sep=""),"sigma")
##### berechnung der ergebnisse parameter---------------- 
parametererg<-as.matrix(data.frame(itempar.poly(daten, m = m, sortdif=FALSE, ...)))

##### aufbereitung der ergebnisse und ausgabe ----------------
sortsig<-order(parametererg[,dim(parametererg)[2]]) # reihenfolge nach sigma parameter
if(sortdif==TRUE){SE<-SEerg[sortsig,] ; parameter<-parametererg[sortsig,]  }
if(sortdif==FALSE){SE<-SEerg ; parameter<-parametererg}

itppSE<-list(parameter=parameter, SE=SE)
class(itppSE) <- c("ippwpoSE","list")

return(itppSE)
# summary.ippwpoSE(itpSE)
}
