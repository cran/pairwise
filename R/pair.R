#' @title Item Parameter Calculation
#' @export pair
#' @description This is the (new) main function for calculation of the item parameter for the dichotomous Rasch Model (Rasch, 1960) and its extension for polytomous items (thurstonian thresholds) according to the Partial Credit Model (Masters, 1982).
#' 
#' The function implements a generalization (see Heine & Tarnai, 2015) of the pairwise comparison approach, that is based on the principle for item calibration introduced by Choppin (1968, 1985) -- see also (Wright & Masters, 1982). The number of (response) categories may vary across items.
#' 
#' Missing values up to an high amount in data are allowed, as long as items are proper linked together.
#'
#' @details Parameter calculation is based on the construction of a paired comparison matrix M\emph{nicjc} with entries f\emph{icjc} representing the number of respondents who answered to item \emph{i} in category \emph{c} and to item \emph{j} in category \emph{c-1} widening Choppin's (1968, 1985) conditional pairwise algorithm to polytomous item response formats. This algorithm is simply realized by matrix multiplication.
#' 
#' To avoid numerical problems with off diagonal zero's when constructing the pairwise comparison matrix M\emph{nij}, powers of the M\emph{nicjc} matrix, can be used (Choppin, 1968, 1985). Using powers \emph{k} of M\emph{nicjc} - argument \code{pot=TRUE} (default), replaces the results of the direct comparisons between \emph{i} and \emph{j} with the sum of the indirect comparisons of \emph{i} and \emph{j} through an intermediate \emph{k}.
#' In general, it is recommended to use the argument with default value \code{pot=TRUE}.
#' 
#' If a list object is assigned to the argument \code{data}, the list entries (matrix or data.frame) must all have the same dimensionality. The individual list entries represent either \emph{r} measurement times or raters. If such a list object is used, first the item parameters are calculated across all \emph{r} measurement points or raters and additionally a threshold parameter is given for each of the \emph{r} measurement points or raters (e.g. rater severity or overal item shift).
#' 
#' For a graphic representation of the item 'estimates' the plotting S3 method \code{\link{plot.pair}} is available. For plotting the item category probabilities the function \code{\link{catprob}} can be used.
#'
#' @param daten a single \code{data.frame} or \code{matrix} or a \code{list} with every list entry holding a \code{data.frame} or \code{matrix}. Row and column names may represent the names of items (columns) and persons (rows). Missing values (\code{NA}) are allowed. Item responses can be polytomous or dichotomous (or a mixture of a varying number of categories across all items). Responses of \code{n} respondents (rows) on \code{k} items (colums) must be coded (scored) starting with 0 for lowest category to \emph{m}-1 for highest category. If a list is assigned to this argument each \code{data.frame} or \code{matrix} as respective list entry must have the same dimensionality. See details and examples.
#' @param m an integer (will be recycled to a vector of length k) or a vector giving the number of response categories for all items - by default \code{(m = NULL)}, \code{m} is calculated from data, assuming that every response category is at least once present in the data. For \emph{'sparse' data} it is \emph{strongly recomended} to explicitly \emph{define the number of categories} by defining this argument.
#' @param w an optional vector of case weights.
#' @param pot either a logical or an integer  >= 2 defining the power to compute of the pairwise comparison matrix. If TRUE (default) a power of three of the pairwise comparison matrix is used for further calculations. If FALSE no powers are computed.
#' @param zerocor either a logical or an numeric value between >0 and <=1. If (in case of a logical) zerocor is set to TRUE (default) unobserved combinations (1-0, 0-1) in the data for each pair of items are given a frequency of one conf. proposal by Alexandrowicz (2011, p.373). As an alternative option a numeric value between >0 and <=1 can be assigned to unobserved combinations (1-0, 0-1) in the data for each pair of items (conf. to personal communication with A. Robitzsch; 29-03-2021).  
#' @param ccf logical with default \code{ccf=FALSE} to perform normal item parameter calculation, if set to \code{ccf=TRUE} just the conditional item (category) frequencies are returned.
#' @param likelihood either NULL (default) or a a character expression defining a likelihood estimation approach based on the pairwise comparison matrix. Currently only the so called MINCHI approach as described in Fischer (2006) is implemented , which can be selected by setting \code{likelihood="minchi"}.
#' @param pot2 ignored when \code{likelihood=NULL} (default).
#' @param delta ignored when \code{likelihood=NULL} (default).
#' @param conv ignored when \code{likelihood=NULL} (default).
#' @param maxiter ignored when \code{likelihood=NULL} (default).
#' @param progress ignored when \code{likelihood=NULL} (default).
#' @param init ignored when \code{likelihood=NULL} (default).
#' @param zerosum ignored when \code{likelihood=NULL} (default).
#' @param ... additional parameters passed through.
#' 
#' @return A (list) object of class \code{"pair"} containing the item category thresholds and difficulties sigma, also called item location.
#' 
#' @references Choppin, B. (1968). Item Bank using Sample-free Calibration. \emph{Nature, 219}(5156), 870-872.
#' @references Choppin, B. (1985). A fully conditional estimation procedure for Rasch model parameters. \emph{Evaluation in Education, 9}(1), 29-42.
#' @references Heine, J. H. & Tarnai, Ch. (2015). Pairwise Rasch model item parameter recovery under sparse data conditions. \emph{Psychological Test and Assessment Modeling, 57}(1), 3–36.
#' @references  Alexandrowicz, R. W. (2011). 'GANZ RASCH': A Free Software for Categorical Data Analysis. \emph{Social Science Computer Review, 30}(3), 369-379.
#' @references Masters, G. (1982). A Rasch model for partial credit scoring. \emph{Psychometrika, 47}(2), 149–174.
#' @references Rasch, G. (1960). \emph{Probabilistic models for some intelligence and attainment tests.} Copenhagen: Danmarks pædagogiske Institut.
#' @references Wright, B. D., & Masters, G. N. (1982). \emph{Rating Scale Analysis.} Chicago: MESA Press.
#' @references Fischer, Gerhard H. 2006. "Rasch Models". Pp. 515–85 in Handbook of statistics (26): Psychometrics. Vol. 26, edited by C. R. Rao and S. Sinharay. Amsterdam: Elsevier.

#' @examples data(bfiN) # loading example data set
#' # calculating itemparameters for 5 neuroticism items with 6 answer categories (0-5).
#' neuro_itempar<-pair(daten = bfiN, m = 6) 
#' summary(neuro_itempar) 
#' summary(neuro_itempar, sortdif=TRUE) # ordered by difficulty 
#' # plotting threshold profiles for 5 neuroticism items.
#' plot(neuro_itempar) 
#' plot(neuro_itempar, sortdif=TRUE) # plotting ordered by difficulty 
#' ################ with unequal number of categories 
#' data(sim200x3)
#' res<-pair(sim200x3)
#' summary(res)
#' plot(res)

############## function begins #################################################
## Version 0.6.0-0 (with iterative option) # m=NULL; w=NULL; pot=TRUE; zerocor=TRUE; ccf=FALSE;likelihood=NULL; pot2=2; delta=TRUE; conv=0.0001; maxiter=3000; progress=TRUE; init=NULL; zerosum=TRUE

pair <- function(daten, m=NULL, w=NULL, pot=TRUE, zerocor=TRUE, ccf=FALSE, likelihood=NULL, pot2=2, delta=TRUE, conv=0.0001, maxiter=3000, progress=TRUE, init=NULL, zerosum=TRUE, ...){
  
  options(stringsAsFactors = FALSE) # added 14-12-2017
  
  fuargs <- list(daten=deparse(substitute(daten)), m=m, pot=pot, zerocor=zerocor, ccf=ccf, likelihood=likelihood, delta=delta, conv=conv, maxiter=maxiter, progress=progress, init=init, zerosum=zerosum, ...=...)
  #### some internal functions ------
  # mat.mult -------------------------------------------------------------------
  mat.mult<-function(A,B,na.rm=TRUE){
    # new version based on remarks by A. Robitzsch
    A<-as.matrix(A);B<-as.matrix(B)
    if(dim(A)[2]!=dim(B)[1]){stop("not conformable Matrices A B")}
    A[ is.na(A) ] <- 0
    B[ is.na(B) ] <- 0
    erg <- A %*% B
    return(erg)
  }
  # katdat ---------------------------------------------------------------------
  katdat<-function(X,mVector, INFO=FALSE){
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
  # Fmat -----------------------------------------------------------------------
  Fmat<-function(X,INFO=FALSE,ipsative=TRUE, w=NULL){ # new extended function 20-03-2018: case weights
    # the normal case X is an object of class "katdat"
    stopifnot(class(X)=="katdat")
    mVector<-X$mVector; X<-X$katdat
    if(is.null(w)){W <- matrix(1,nrow = nrow(X),ncol = ncol(X))} # added 20-03-2018: case weights
    if(!(is.null(w))){W <- matrix(rep(w,times = ncol(X)),nrow = nrow(X),byrow=FALSE)} # added 20-03-2018: case weights
    # condf <- mat.mult(t(X),(X)) # old backup
    condf <- mat.mult(t(X),(X*W))# added 20-03-2018: case weights
    spaltenraus <- cumsum(mVector)
    zeilenraus <- cumsum(mVector)-((mVector)-1)
    fmat <- condf[-zeilenraus,-spaltenraus]
    ### set the ipsative item comparisons to 0
    if(ipsative==FALSE){
      bisnull <- cumsum((mVector)-1)
      vonnull <- (cumsum((mVector)-1) ) - ((mVector)-2)
      y<-do.call(c,mapply(function(x,y){rep(x,each=y) },mapply(seq,vonnull,bisnull,SIMPLIFY = FALSE),mVector-1,SIMPLIFY = FALSE))
      x<-do.call(c,mapply(function(x,y){rep(x,times=y) },mapply(seq,vonnull,bisnull,SIMPLIFY = FALSE),mVector-1,SIMPLIFY = FALSE))
      for (i in 1:length(y)){fmat[(y[i]),(x[i])]<-0}
    }
    if(INFO==FALSE){return(fmat)}
    if(INFO==TRUE){
      erg<-list(fmat=fmat, mVector=mVector)
      class(erg)<-"Fmat"
      return(erg)
    }
  }
  # cube -----------------------------------------------------------------------
  cube <- function(M, POTT=3){
    Mp <- M
    for (i in 1:(POTT-1)){
      Mp<-Mp%*%M
    }
    return(Mp)
  }
  # Dmat -----------------------------------------------------------------------
  Dmat<-function(X){
    dmat <- t(X) / X
    return (dmat) 
  }
  # pow ---- compute arbitrary powers of matrix --------------------------------
  pow <- function (x, k){
    n <- nrow(x)
    # return(pow(solve(x), -k))
    x.k <- x
    i <- 2
    while(i <= k){
      x.k <- x.k %*% x
      i <- i + 1
    }
    return(x.k)
  }
  #### END internal functions --------------------------------------------------
  
#### start data preparation --------------------------------------------------
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

#### some category checks for m calculated from data (revised on 29-03-2021)----
if(length(m)==0){if(all(apply(d,2,function(x){min(x,na.rm=TRUE)})== 0) == FALSE){stop("item categories must start with 0 ... some items may be constant") }}
if(length(m)==0){m<-apply(d,2,function(x){max(x,na.rm=TRUE)+1}); comment(m) <- "estimated from data"}
if(length(m)==1){m<-rep(m,dim(d)[2]); comment(m) <- "user defined (recycled)"}else{comment(m) <- "user defined"} 
if(any(m < apply(d,2,function(x){max(x,na.rm=TRUE)+1}))){stop("some items in data have more categories than defined in m","\n","max item categories in data are: ",paste(apply(d,2,function(x){max(x,na.rm=TRUE)+1}),collapse=" , "),"\n", "but m was defined: ",paste(m,collapse=" , ")  )}
if(any(m<2)){stop("some items are constant ... have only one category")}

#### start regular item parameter calculation ----------------------------------
if (ccf==FALSE){
  kd <- katdat(d, mVector=m, INFO=T)  
  f <- Fmat(kd,INFO=T,ipsative=FALSE,w = w)
  mVector<-f$mVector
  # powers of nij ?
  if(is.logical(pot)){
    if(pot==FALSE){nij <- f$fmat}
    if(pot==TRUE){nij <- cube(f$fmat); Mrpc <- cube(Mrpc) }
  }
  if(is.numeric(pot)){
    if(pot<2){stop("pot must be >= than 2")}
    else{
      nij <- pow(f$fmat,pot); Mrpc <- pow(Mrpc,pot)
    }
  }
    
  # new zero correction as an option --> conf. R. Alexandrowicz(2011) p.373 
  if(zerocor==TRUE){nij[nij==0]<-1; Mrpc[Mrpc==0]<-1}
  # new zero correction as an option --> conf. pers. com. A. Robitzsch 29-03-2021
    if(is.numeric(zerocor)){
    if(zerocor<=0 | zerocor>1){stop("zerocor must be >0 and <=1")};
      nij[nij==0]<-zerocor; Mrpc[Mrpc==0]<-zerocor}
    
    # calculation thresholds / difficulties
    logD <- log(Dmat(nij)) # new 04-29-2016
    logDr <- log(Dmat(Mrpc)) # new 22-06-2020 Rater severity / time drift 
    logD_ <- logD # new 04-29-2016
    tau<-rowMeans(logD_, na.rm = TRUE)
    taur <- rowMeans(logDr)
    
    # new option (29-03-2021) which item pairs should occur in likelihood based estimation procedure?
    if(delta==TRUE){delta.ij <- 1 * ( f$fmat + t(f$fmat) > 0 )}# currently only used when likelihood=="minchi"
    if(delta==FALSE){delta.ij <- 1 * ( f$fmat + t(f$fmat) >= 0 )}# currently only used when likelihood=="minchi"
    
    if(!is.null(likelihood)){ 
      # possibly add here some more PMLM algorithms ...
      
      # if(likelihood=="some more PMLM algorithms"){
      
      #}
      
      if(likelihood=="minchi"){ # added (29-03-2021)
        # pairwise likelihood method, so called MINCHI method -- cf. G. Fischer: p. 544 ff in Handbook of Statistics Vol. 26 (Psychometrics)
        # initial values -- b.init -- for parameter estimation
        if(is.null(init) ){ 
          est <- tau # est --- beta
        } else {
          est <- init ## work on this for polytomous data 
        }
        # calculate y_{ij} values
        # powers of nij ?
        if(is.logical(pot2)){
          if(pot==FALSE){nij.y <- f$fmat}
          if(pot==TRUE){nij.y <- cube(f$fmat) }#; Mrpc <- cube(Mrpc)
        }
        if(is.numeric(pot2)){
          if(pot2<2){stop("pot2 must be >= than 2")}
          else{
            nij.y <- pow(f$fmat,pot2)#; Mrpc <- pow(Mrpc,pot2
          }
        }
        y.ij <- nij.y / ( f$fmat + t(f$fmat) ) #JHH: 'nij.y' might have powers (pot2); 'f$fmat' is the pairwise frequencies
        y.ij[ delta.ij==0 ] <- 0
        y.ji <- t( y.ij ) # not used yet
        eps <- exp(-est)
        change <- 1
        iter <- 0
        e <- -log(eps)
        
        # start estimation algorithm
        while( change > conv & iter < maxiter ){
          eps0 <- eps
          e0 <- e
          eps <- sqrt( rowSums( y.ij * eps * delta.ij ) / colSums( y.ij / eps ) )
          if (zerosum){
            b1 <- - log(eps)
            b2 <- b1 - mean(b1)
            eps <- exp(-b2)
          }
          e <- -log(eps)
          change <- max( abs( eps0 - eps ) )
          change_e <- max(abs( -e0 + e))
          iter <- iter + 1
          if (progress == TRUE){
            cat(likelihood, "Iteration-", iter, ": max. parm. change=", round( change_e, 6 ), "\n")
            # utils::flush.console()
          }
        } # end estimation algorithm
        tau_pairwise <- tau
        tau <- e
      }
    } ### end of likelihood option
    
    
    # formatting results
    vo <- (cumsum((mVector)-1) ) - ((mVector)-2)
    bi <- cumsum((mVector)-1)
    er1<-mapply(function(v,b){ tau[v:b]},vo,bi,SIMPLIFY = FALSE)
    names(er1) <- colnames(d)
    
    # unequal or equal category number -- sb as list, thresholds if necessary with NA as matrix
    sigma <- sapply(er1, mean)
    threshold <- er1
    ### calculation of sb; sb as list!!!
    sb<-(lapply(threshold,function(x){cumsum(x)})) # correct: conversion threshold (tau) to sb !!!
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
    }
    
    erg<-list(threshold=threshold,sigma=sigma,sb=sb,resp=d, fuargs=fuargs, m=m, taur=taur)
    class(erg) <- c("pair","list")
    
    return(erg)
  }
  #### end regular item parameter calculation ----------------------------------
  
  #### start regular item parameter calculation --------------------------------
  if (ccf==TRUE){
    # just return nij matrix ...
    f <- Fmat(katdat(d, mVector=m, INFO=T),INFO=T,ipsative=FALSE,w = w)
    mVector<-f$mVector
    # powers of nij ?
    if(is.logical(pot)){
      if(pot==FALSE){nij <- f$fmat}
      if(pot==TRUE){nij <- cube(f$fmat); Mrpc <- cube(Mrpc) }
    }
    if(is.numeric(pot)){
      if(pot<2){stop("pot must be >= than 2")}
      else {nij <- pow(f$fmat,pot); Mrpc <- pow(Mrpc,pot)}
    }
    
    # new zero correction as an option --> conf. R. Alexandrowicz(2011) p.373 
    if(zerocor==TRUE){nij[nij==0]<-1; Mrpc[Mrpc==0]<-1}
    # new zero correction as an option --> conf. pers. com. A. Robitzsch 29-03-2021
    if(is.numeric(zerocor)){
      if(zerocor<=0 | zerocor>1){stop("zerocor must be >0 and <=1")};
      nij[nij==0]<-zerocor; Mrpc[Mrpc==0]<-zerocor}
    
    print(Mrpc)
    return(nij)
  }
}

############## old version function begins #####################################

# pair <- function(daten, m=NULL, w=NULL, pot=TRUE, zerocor=TRUE, ccf=FALSE, ...){
# 
#   options(stringsAsFactors = FALSE) # added 14-12-2017
#   
#   fuargs <- list(daten=daten, m=m, pot=pot, zerocor=zerocor, ccf=ccf, ...=...)
#   #### some internal functions ------
#   # dataprep1<-function(X){
#   # X<-as.matrix(X)
#   # if(length(colnames(X))==0){
#   #   Iname<-nchar(paste(dim(X)[2]))
#   #   colnames(X)<-paste("I",formatC(1:dim(X)[2], width = Iname, format = "d", flag = "0"),sep="") 
#   #   cat("no item names found in data" ,"\n", "items are named", colnames(X)[1], "(first item) to",  colnames(X)[dim(X)[2]],"(last item)","\n")
#   # }
#   # if(length(rownames(X))==0){
#   #   Pname<-nchar(paste(dim(X)[1]))
#   #   rownames(X)<-paste("P",formatC(1:dim(X)[1], width = Pname, format = "d", flag = "0"),sep="")  
#   #   cat("no person names (IDs) found in data" ,"\n", "persons are named", rownames(X)[1], "(first row) to",  rownames(X)[dim(X)[1]],"(last row)","\n")
#   # }
#   # return(X)
#   # }
#   
#   mat.mult<-function(A,B,na.rm=TRUE){
#   # new version based on remarks by A. Robitzsch
#   A<-as.matrix(A);B<-as.matrix(B)
#   if(dim(A)[2]!=dim(B)[1]){stop("not conformable Matrices A B")}
#   A[ is.na(A) ] <- 0
#   B[ is.na(B) ] <- 0
#   erg <- A %*% B
#   return(erg)
#   }
#   
#   katdat<-function(X,mVector, INFO=FALSE){
#   #if(length(mVector)==0){mVector<-mV(X)}
#   nitem<-dim(X)[2]
#   XList<-as.list(data.frame((X)))
#   foo<-function(q,w){ lapply(q,function(x){ (x==w)*1 })    }
#   katdatList<-mapply(foo, (lapply(as.list(mVector-1),function(x){c(0:x[1])})), XList, SIMPLIFY = F )
#   katdat<-do.call(cbind, lapply(katdatList,function(x){(do.call(cbind,x))}))
#   colnames(katdat)<-unlist(mapply(paste, mapply(rep,colnames(X),mVector,SIMPLIFY = F), ( lapply(as.list(mVector-1),function(x){c(0:x[1])})  ) , MoreArgs=list(sep = "."), SIMPLIFY = F))
#   rownames(katdat)<-rownames(X)
#   if(INFO==FALSE){return(katdat)}
#   if(INFO==TRUE){
#     erg<-list(katdat=katdat, mVector=mVector)
#     class(erg)<-"katdat"
#     return(erg)
#   }
#   }
#     
#   Fmat<-function(X,INFO=FALSE,ipsative=TRUE, w=NULL){ # new extended function 20-03-2018: case weighths
#     # der normale Fall X ist ein objekt der Klasse "katdat"
#     stopifnot(class(X)=="katdat")
#     mVector<-X$mVector; X<-X$katdat
#     if(is.null(w)){W <- matrix(1,nrow = nrow(X),ncol = ncol(X))} # added 20-03-2018: case weighths
#     if(!(is.null(w))){W <- matrix(rep(w,times = ncol(X)),nrow = nrow(X),byrow=FALSE)} # added 20-03-2018: case weighths
#     #condf <- mat.mult(t(X),(X)) # old backup
#     condf <- mat.mult(t(X),(X*W))# added 20-03-2018: case weighths
#     spaltenraus <- cumsum(mVector)
#     zeilenraus <- cumsum(mVector)-((mVector)-1)
#     fmat <- condf[-zeilenraus,-spaltenraus]
#     ### setzten der ipsativen item vergleiche auf 0
#     if(ipsative==FALSE){
#       bisnull <- cumsum((mVector)-1)
#       vonnull <- (cumsum((mVector)-1) ) - ((mVector)-2)
#       y<-do.call(c,mapply(function(x,y){rep(x,each=y) },mapply(seq,vonnull,bisnull,SIMPLIFY = FALSE),mVector-1,SIMPLIFY = FALSE))
#       x<-do.call(c,mapply(function(x,y){rep(x,times=y) },mapply(seq,vonnull,bisnull,SIMPLIFY = FALSE),mVector-1,SIMPLIFY = FALSE))
#       for (i in 1:length(y)){fmat[(y[i]),(x[i])]<-0}
#     }
#     ####
#     if(INFO==FALSE){return(fmat)}
#     if(INFO==TRUE){
#       erg<-list(fmat=fmat, mVector=mVector)
#       class(erg)<-"Fmat"
#       return(erg)
#     }
#   }
#   
# #   cube<-function(M){
# #   Mp<-M%*%M%*%M
# #   return(Mp)
# #   }
#   
#   cube<-function(M, POTT=3){
#     Mp <- M
#     for (i in 1:(POTT-1)){
#       Mp<-Mp%*%M
#     }
#     return(Mp)
#   }
#     
#   Dmat<-function(X){
#   dmat <- t(X) / X
#   return (dmat) 
#   }
# #### compute arbitrary powers of matrix 
# pow <- function (x, k) 
# {  n <- nrow(x)
#    # return(pow(solve(x), -k))
#    x.k <- x
#    i <- 2
#    while (i <= k) {
#      x.k <- x.k %*% x
#      i <- i + 1
#    }
#    return(x.k)
# }
#   #### END internal functions ------
#   
#   #### start data preparation ------
# if( any(class(daten)=="list") ){
#   Ldaten <- daten # back up user data
#   rt <- length(daten)
#   d <- dataprep1(daten) # dataprep1 checks if daten is a "list"
#   Lr <- lapply(1:rt, function(x){c(d[((((nrow(d))/rt)*(x-1))+1):(((nrow(d))/rt)*x),])})  
#   Mrpc <- matrix(mapply(FUN = function(x,y){ sum(Lr[[x]]>Lr[[y]]) },x=rep(1:rt,each=rt),y=rep(1:rt,times=rt),SIMPLIFY = T),ncol = rt)
#   }
# 
# if( any(class(daten)!="list") ){
#   d<-dataprep1(daten)
#   rt <- 1
#   Mrpc <- matrix(1,1,1)
# }
#   
#   ### some category checks for m ----
#   if(length(m)==0){if (all(apply(d,2,function(x){min(x,na.rm=TRUE)})== 0) == FALSE){stop("item categories must start with 0") }}
#   if (length(m)==0){m<-apply(d,2,function(x){max(x,na.rm=TRUE)+1}); comment(m) <- "estimated from data"}
#   if (length(m)==1){m<-rep(m,dim(d)[2]); comment(m) <- "user defined (recycled)"}else{comment(m) <- "user defined"} 
#   if (any (m < apply(d,2,function(x){max(x,na.rm=TRUE)+1}))){stop("some items in data have more categories than defined in m","\n","max item categories in data are: ",paste(apply(d,2,function(x){max(x,na.rm=TRUE)+1}),collapse=" , "),"\n", "but m was defined: ",paste(m,collapse=" , ")  )}
# 
#   #### start itemparameter calculation ------
#   if (ccf==FALSE){
#     
#   f <- Fmat(katdat(d, mVector=m, INFO=T),INFO=T,ipsative=FALSE,w = w)
#   mVector<-f$mVector
#   
#   # new zero correction as an option --> conf. R. Alexandrowicz(2011) p.373 
#   # modified 04-29-2016 :
# #   fm <- f$fmat
# #   fm[fm==0]<-NA
# #   min(fm,na.rm = T)
# #   max(fm,na.rm = T)
# #   zkor <- max(fm,na.rm = T) / min(fm,na.rm = T)
# #   zkor <- 1
# #   if(zerocor==TRUE){f$fmat[f$fmat==0]<-zkor} ## this now in v0.3.2 befor powering
#   
#   
# 
#   # powers of nij ?
#   if(class(pot)=="logical"){
#   if(pot==FALSE){nij <- f$fmat}
#   if(pot==TRUE){nij <- cube(f$fmat); Mrpc <- cube(Mrpc) }
#   }
#   if(class(pot)=="numeric"){
#     if(pot<2){stop("pot must be >= than 2")}
#     else{
#       nij <- pow(f$fmat,pot); Mrpc <- pow(Mrpc,pot)
#     }
#   }
#    
#   # new zero correction as an option --> conf. R. Alexandrowicz(2011) p.373 
#   if(zerocor==TRUE){nij[nij==0]<-1; Mrpc[Mrpc==0]<-1}
#   
#   # calculation thresholds / dificulties
#   logD <- log(Dmat(nij)) # new 04-29-2016
#   logDr <- log(Dmat(Mrpc)) # new 22-06-2020 Rater severity / time drift 
#   logD_ <- logD # new 04-29-2016
#   #logD_[logD_==0]<-colMeans(logD_, na.rm = TRUE) # new 04-29-2016
#   # logD_[logD_==0]<-NA # new 04-29-2016
#   # logD_[is.na(logD_)]<-rowMeans(logD_, na.rm = TRUE) # new 04-29-2016
#   tau<-rowMeans(logD_, na.rm = TRUE)
#   taur <- rowMeans(logDr)
#   # formatieren der ausgabe
#   vo <- (cumsum((mVector)-1) ) - ((mVector)-2)
#   bi <- cumsum((mVector)-1)
#   er1<-mapply(function(v,b){ tau[v:b]},vo,bi,SIMPLIFY = FALSE)
#   names(er1) <- colnames(d)
# 
# ### ungleiche oder gleiche kategoriezahl -- sb als liste, thresholds ggf mit NA als matrix
#   sigma <- sapply(er1, mean)
#   threshold <- er1
#   ### berechnung der sb; sb als liste!!!!
#   sb<-(lapply(threshold,function(x){cumsum(x)})) # richtig: umrechnung threshold (tau) in sb !!!!!!!!!
#   if(length(unique(mVector))!=1){
#     maxLen <- max(sapply(threshold, length))
#     # create a new list with elements padded out with NAs
#     newthreshold <- lapply(threshold, function(.ele){c(.ele, rep(NA, maxLen))[1:maxLen]})
#     threshold <- do.call(rbind, newthreshold)
#     colnames(threshold) <- c(1:dim(threshold)[2])
#   }
#   if(length(unique(mVector))==1){
#     threshold<-do.call(rbind, threshold)
#     colnames(threshold) <- c(1:(unique(mVector)-1))
#     # rownames(threshold) <- colnames(d)
#   }
# 
#   erg<-list(threshold=threshold,sigma=sigma,sb=sb,resp=d, fuargs=fuargs, m=m, taur=taur)
#   class(erg) <- c("pair","list")
#   
#   #invisible(erg)
#    return(erg)
# 
#   }
# 
#   if (ccf==TRUE){
#   
#   f <- Fmat(katdat(d, mVector=m, INFO=T),INFO=T,ipsative=FALSE,w = w)
#   mVector<-f$mVector
#   # powers of nij ?
#   if(class(pot)=="logical"){
#     if(pot==FALSE){nij <- f$fmat}
#     if(pot==TRUE){nij <- cube(f$fmat); Mrpc <- cube(Mrpc) }
#   }
#   if(class(pot)=="numeric"){
#     if(pot<2){stop("pot must be >= than 2")}
#     else {nij <- cube(f$fmat, POTT=pot); Mrpc <- pow(Mrpc,pot)}
#     }
#   
#   # new zero correction as an option --> conf. R. Alexandrowicz(2011) p.373 
#   if(zerocor==TRUE){nij[nij==0]<-1; Mrpc[Mrpc==0]<-1}
#   print(Mrpc)
#   return(nij)
#   }
#}
