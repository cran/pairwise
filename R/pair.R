pair <- function(daten, m=NULL, w=NULL, pot=TRUE, zerocor=TRUE, ccf=FALSE, ...){

  options(stringsAsFactors = FALSE) # added 14-12-2017
  
  fuargs <- list(daten=daten, m=m, pot=pot, zerocor=zerocor, ccf=ccf, ...=...)
  #### some internal functions ------
  # dataprep1<-function(X){
  # X<-as.matrix(X)
  # if(length(colnames(X))==0){
  #   Iname<-nchar(paste(dim(X)[2]))
  #   colnames(X)<-paste("I",formatC(1:dim(X)[2], width = Iname, format = "d", flag = "0"),sep="") 
  #   cat("no item names found in data" ,"\n", "items are named", colnames(X)[1], "(first item) to",  colnames(X)[dim(X)[2]],"(last item)","\n")
  # }
  # if(length(rownames(X))==0){
  #   Pname<-nchar(paste(dim(X)[1]))
  #   rownames(X)<-paste("P",formatC(1:dim(X)[1], width = Pname, format = "d", flag = "0"),sep="")  
  #   cat("no person names (IDs) found in data" ,"\n", "persons are named", rownames(X)[1], "(first row) to",  rownames(X)[dim(X)[1]],"(last row)","\n")
  # }
  # return(X)
  # }
  
  mat.mult<-function(A,B,na.rm=TRUE){
  # new version based on remarks by A. Robitzsch
  A<-as.matrix(A);B<-as.matrix(B)
  if(dim(A)[2]!=dim(B)[1]){stop("not conformable Matrices A B")}
  A[ is.na(A) ] <- 0
  B[ is.na(B) ] <- 0
  erg <- A %*% B
  return(erg)
  }
  
  katdat<-function(X,mVector, INFO=FALSE){
  #if(length(mVector)==0){mVector<-mV(X)}
  nitem<-dim(X)[2]
  XList<-as.list(data.frame((X)))
  foo<-function(q,w){ lapply(q,function(x){ (x==w)*1 })    }
  katdatList<-mapply(foo, (lapply(as.list(mVector-1),function(x){c(0:x[1])})), XList, SIMPLIFY = F )
  katdat<-do.call(cbind, lapply(katdatList,function(x){(do.call(cbind,x))}))
  colnames(katdat)<-unlist(mapply(paste, mapply(rep,colnames(X),mVector,SIMPLIFY = F), ( lapply(as.list(mVector-1),function(x){c(0:x[1])})  ) , MoreArgs=list(sep = "."), SIMPLIFY = F))
  rownames(katdat)<-rownames(X)
  if(INFO==FALSE){return(katdat)}
  if(INFO==TRUE){
    erg<-list(katdat=katdat, mVector=mVector)
    class(erg)<-"katdat"
    return(erg)
  }
  }
    
  Fmat<-function(X,INFO=FALSE,ipsative=TRUE, w=NULL){ # new extended function 20-03-2018: case weighths
    # der normale Fall X ist ein objekt der Klasse "katdat"
    stopifnot(class(X)=="katdat")
    mVector<-X$mVector; X<-X$katdat
    if(is.null(w)){W <- matrix(1,nrow = nrow(X),ncol = ncol(X))} # added 20-03-2018: case weighths
    if(!(is.null(w))){W <- matrix(rep(w,times = ncol(X)),nrow = nrow(X),byrow=FALSE)} # added 20-03-2018: case weighths
    #condf <- mat.mult(t(X),(X)) # old backup
    condf <- mat.mult(t(X),(X*W))# added 20-03-2018: case weighths
    spaltenraus <- cumsum(mVector)
    zeilenraus <- cumsum(mVector)-((mVector)-1)
    fmat <- condf[-zeilenraus,-spaltenraus]
    ### setzten der ipsativen item vergleiche auf 0
    if(ipsative==FALSE){
      bisnull <- cumsum((mVector)-1)
      vonnull <- (cumsum((mVector)-1) ) - ((mVector)-2)
      y<-do.call(c,mapply(function(x,y){rep(x,each=y) },mapply(seq,vonnull,bisnull,SIMPLIFY = FALSE),mVector-1,SIMPLIFY = FALSE))
      x<-do.call(c,mapply(function(x,y){rep(x,times=y) },mapply(seq,vonnull,bisnull,SIMPLIFY = FALSE),mVector-1,SIMPLIFY = FALSE))
      for (i in 1:length(y)){fmat[(y[i]),(x[i])]<-0}
    }
    ####
    if(INFO==FALSE){return(fmat)}
    if(INFO==TRUE){
      erg<-list(fmat=fmat, mVector=mVector)
      class(erg)<-"Fmat"
      return(erg)
    }
  }
  
#   cube<-function(M){
#   Mp<-M%*%M%*%M
#   return(Mp)
#   }
  
  cube<-function(M, POTT=3){
    Mp <- M
    for (i in 1:(POTT-1)){
      Mp<-Mp%*%M
    }
    return(Mp)
  }
    
  Dmat<-function(X){
  dmat <- t(X) / X
  return (dmat) 
  }
#### compute arbitrary powers of matrix 
pow <- function (x, k) 
{  n <- nrow(x)
   # return(pow(solve(x), -k))
   x.k <- x
   i <- 2
   while (i <= k) {
     x.k <- x.k %*% x
     i <- i + 1
   }
   return(x.k)
}
  #### END internal functions ------
  
  #### start data preparation ------
if( any(class(daten)=="list") ){
  Ldaten <- daten # back up user data
  rt <- length(daten)
  d <- dataprep1(daten) # dataprep1 checks if daten is a "list"
  Lr <- lapply(1:rt, function(x){c(d[((((nrow(d))/rt)*(x-1))+1):(((nrow(d))/rt)*x),])})  
  Mrpc <- matrix(mapply(FUN = function(x,y){ sum(Lr[[x]]>Lr[[y]]) },x=rep(1:rt,each=rt),y=rep(1:rt,times=rt),SIMPLIFY = T),ncol = rt)
  }

if( any(class(daten)!="list") ){
  d<-dataprep1(daten)
  rt <- 1
  Mrpc <- matrix(1,1,1)
}
  
  ### some category checks for m ----
  if(length(m)==0){if (all(apply(d,2,function(x){min(x,na.rm=TRUE)})== 0) == FALSE){stop("item categories must start with 0") }}
  if (length(m)==0){m<-apply(d,2,function(x){max(x,na.rm=TRUE)+1}); comment(m) <- "estimated from data"}
  if (length(m)==1){m<-rep(m,dim(d)[2]); comment(m) <- "user defined (recycled)"}else{comment(m) <- "user defined"} 
  if (any (m < apply(d,2,function(x){max(x,na.rm=TRUE)+1}))){stop("some items in data have more categories than defined in m","\n","max item categories in data are: ",paste(apply(d,2,function(x){max(x,na.rm=TRUE)+1}),collapse=" , "),"\n", "but m was defined: ",paste(m,collapse=" , ")  )}

  #### start itemparameter calculation ------
  if (ccf==FALSE){
    
  f <- Fmat(katdat(d, mVector=m, INFO=T),INFO=T,ipsative=FALSE,w = w)
  mVector<-f$mVector
  
  # new zero correction as an option --> conf. R. Alexandrowicz(2011) p.373 
  # modified 04-29-2016 :
#   fm <- f$fmat
#   fm[fm==0]<-NA
#   min(fm,na.rm = T)
#   max(fm,na.rm = T)
#   zkor <- max(fm,na.rm = T) / min(fm,na.rm = T)
#   zkor <- 1
#   if(zerocor==TRUE){f$fmat[f$fmat==0]<-zkor} ## this now in v0.3.2 befor powering
  
  

  # powers of nij ?
  if(class(pot)=="logical"){
  if(pot==FALSE){nij <- f$fmat}
  if(pot==TRUE){nij <- cube(f$fmat); Mrpc <- cube(Mrpc) }
  }
  if(class(pot)=="numeric"){
    if(pot<2){stop("pot must be >= than 2")}
    else{
      nij <- pow(f$fmat,pot); Mrpc <- pow(Mrpc,pot)
    }
  }
   
  # new zero correction as an option --> conf. R. Alexandrowicz(2011) p.373 
  if(zerocor==TRUE){nij[nij==0]<-1; Mrpc[Mrpc==0]<-1}
  
  # calculation thresholds / dificulties
  logD <- log(Dmat(nij)) # new 04-29-2016
  logDr <- log(Dmat(Mrpc)) # new 22-06-2020 Rater severity / time drift 
  logD_ <- logD # new 04-29-2016
  #logD_[logD_==0]<-colMeans(logD_, na.rm = TRUE) # new 04-29-2016
  # logD_[logD_==0]<-NA # new 04-29-2016
  # logD_[is.na(logD_)]<-rowMeans(logD_, na.rm = TRUE) # new 04-29-2016
  tau<-rowMeans(logD_, na.rm = TRUE)
  taur <- rowMeans(logDr)
  # formatieren der ausgabe
  vo <- (cumsum((mVector)-1) ) - ((mVector)-2)
  bi <- cumsum((mVector)-1)
  er1<-mapply(function(v,b){ tau[v:b]},vo,bi,SIMPLIFY = FALSE)
  names(er1) <- colnames(d)

### ungleiche oder gleiche kategoriezahl -- sb als liste, thresholds ggf mit NA als matrix
  sigma <- sapply(er1, mean)
  threshold <- er1
  ### berechnung der sb; sb als liste!!!!
  sb<-(lapply(threshold,function(x){cumsum(x)})) # richtig: umrechnung threshold (tau) in sb !!!!!!!!!
  if(length(unique(mVector))!=1){
    maxLen <- max(sapply(threshold, length))
    # create a new list with elements padded out with NAs
    newthreshold <- lapply(threshold, function(.ele){c(.ele, rep(NA, maxLen))[1:maxLen]})
    threshold <- do.call(rbind, newthreshold)
    colnames(threshold) <- c(1:dim(threshold)[2])
  }
  if(length(unique(mVector))==1){
    threshold<-do.call(rbind, threshold)
    colnames(threshold) <- c(1:(unique(mVector)-1))
    # rownames(threshold) <- colnames(d)
  }

  erg<-list(threshold=threshold,sigma=sigma,sb=sb,resp=d, fuargs=fuargs, m=m, taur=taur)
  class(erg) <- c("pair","list")
  
  #invisible(erg)
   return(erg)

  }

  if (ccf==TRUE){
  
  f <- Fmat(katdat(d, mVector=m, INFO=T),INFO=T,ipsative=FALSE,w = w)
  mVector<-f$mVector
  # powers of nij ?
  if(class(pot)=="logical"){
    if(pot==FALSE){nij <- f$fmat}
    if(pot==TRUE){nij <- cube(f$fmat); Mrpc <- cube(Mrpc) }
  }
  if(class(pot)=="numeric"){
    if(pot<2){stop("pot must be >= than 2")}
    else {nij <- cube(f$fmat, POTT=pot); Mrpc <- pow(Mrpc,pot)}
    }
  
  # new zero correction as an option --> conf. R. Alexandrowicz(2011) p.373 
  if(zerocor==TRUE){nij[nij==0]<-1; Mrpc[Mrpc==0]<-1}
  print(Mrpc)
  return(nij)
  }

}
