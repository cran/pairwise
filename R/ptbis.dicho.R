#' @title Point Biserial Correlations dichotomous 1PL
#' @export ptbis.dicho
#' @description Calculation of the point biserial correlations for dichotomous item categories with total scale parameter (person parameter).
#'  
#' @details no details in the moment.
#'     
#' @param daten a data matrix, potentially with missing values, comprising dichotomous items (columns). 
#' @param scale an integer vector as an result of any scaling approach (WLE, MLE, Rawscore, etc. ) relating to the Items (columns) in \code{daten}.
#' @return An object of class "list" and "itanaldicho" containing item statistics.
#' @exportClass itanaldicho
#' @examples ######################
#' ##### compute person ML estimates first ########
#' data(cog);data(cogBOOKLET) # loading reponse and allocation data
#' d<-(cog[cog$BOOKID!=14,]) # skip persons which got booklet No.14.
#' inc<-make.incidenz(tab=cogBOOKLET, bookid=d$BOOKID) # make just the incidenz matrix
#' result<-ppML.dicho(daten=d[,4:34], SIGMA=itempar.dicho(d[,4:34]),incidenz = inc )
#' MLscale<-summary(result,FALSE)[,1] # return just the ML person estimates and their standard errors.
#' ##### now compute item statistics ########
#' ptbis.dicho(daten=d[,4:34], scale=MLscale)

############## funktions beginn ########################################################
ptbis.dicho<-function(daten,scale){
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
  Nsc<-length(scale) # anzahl personen (parameter)
  stopifnot(N==Nsc)
  # nur verwenden derjenigen zeilen it gültigem wert auf scale
  pindex <- is.finite(scale) & !is.na(scale)
  X<-daten[pindex,]
  v<-scale[pindex]
  ### hilfs-funktion biseriale korrelation mit einem dicho und einem intervall ...
  ptb<-function(x,v){
    # v[which(x==1)];    v[which(x==0)]
    ScaleAvg_1<-(mean(v[which(x==1)]))
    ScaleAvg_0<-(mean(v[which(x==0)]))
    ScaleSD_1<-(sd(v[which(x==1)],na.rm=TRUE))
    ScaleSD_0<-(sd(v[which(x==0)],na.rm=TRUE))
    Count_1<-sum(x,na.rm=TRUE)
    Count_0<-sum(!x,na.rm=TRUE)
    n<-length(x)
    x1v<-(ScaleAvg_1 - ScaleAvg_0 / (sd(v))) * sqrt( (Count_1*Count_0) / ( n* (n-1) )   ) 
    x0v<-(ScaleAvg_0 - ScaleAvg_1 / (sd(v))) * sqrt( (Count_0*Count_1) / ( n* (n-1) )   )
    # erg<-c(x1v,x0v); names(erg)<-c("1","0")
    erg<-cbind( c(1,0),c(Count_1,Count_0), c(x1v,x0v), c(ScaleAvg_1,ScaleAvg_0) ,c(ScaleSD_1,ScaleSD_0))
    colnames(erg)<-c("Score","Count","Pt_Bis","ScaleAvg","ScaleSD")
    rownames(erg)<-c("cat_1","cat_2")
    return(erg)  
  }
  ### ENDE hilfs-funktion biseriale korrelation mit einem dicho und einem intervall ...  
  res<-lapply(as.list(data.frame(X)),ptb,v)
  class(res)<-c("list","itanaldicho")
  return(res)
  # return (apply(X,2,ptb,v))
  
}  




