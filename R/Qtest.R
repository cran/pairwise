#' @title Model Fit based on Person Fit Index Q
#' @export Qtest
#' @keywords model person fit
#' @description function for performing a model fit test based on calculating the person fit index \code{\link{Q}} for sub samples. Persons are ranked according their fit and in turn a Kruskal-Wallis rank sum test (see \code{\link{kruskal.test}}) is performed to test if the parameters of the distribution of Q are the same in each group (sample).
#' @param obj an object of class \code{"pers"} or class \code{"pair"}as a result from function \code{\link{pers}} or \code{\link{pair}} respectively.
#' @param data optional response data when object of class \code{"pers"} or class \code{"pair"} is not provided.
#' @param threshold optional in case that object of class \code{"pers"} or class \code{"pair"} is not provided. Threshold values as matrix with row and columnnames !! -- items as rows and thresholds as columns. Thresholds should be ordered from left to right, some items may have less thresholds than the others, in this case the respective row/column is filled with an NA value - see examples.
#' @param split Specifies the splitting criterion. Basically there are three different options available - each with several modes - which are controlled by passing the corresponding character expression to the argument. 
#' 
#' 1) Using the rawscore for splitting into subsamples with the following modes: \code{split = "median"} median raw score split - high score group and low score group; \code{split = "mean"} mean raw score split - high score group and low score group.
#' Finaly \code{split = "score"} that is splitting \code{daten} into as many subsamples as there are raw score groups - discarding min and max (theoretical) score group - which matches the concept proposed by Andersen (1973).
#' 
#' 2) Dividing the persons in \code{daten} into subsamples with equal size by random allocation with the following modes: \code{split = "random"} (which is equivalent to \code{split = "random.2"}) divides persons into two subsamples with equal size. In general the number of desired subsamples must be expressed after the dot in the character expression - e.g. \code{split = "random.6"} divides persons into 6 subsamples (with equal size) by random allocation etc. 
#' 
#' 3) The third option is using a manifest variable as a splitting criterion. In this case a vector with the same length as number of cases in \code{daten} must be passed to the argument grouping the data into subsamples. This vector should be coded as \code{"factor"} or a \code{"numeric"} integer vector with min = 1.
#' 
#' @param splitseed numeric, used for \code{set.seed(splitseed)} for random splitting - see argument \code{split}.

#' @param ... not used so far.
#' 
#' @details The person Q-index proposed by Tarnai and Rost, (1990) is solely based on the empirical responses and the item parameters. Thus the computation of person parameters using the function \code{\link{pers}} is not required - see examples. But for convenience return objects of both functions are accepted in function \code{Q}. This offers the opportunity to get some inference on model fit only based on item parameters.
#' @return A list with class "htest" containing the following components as returned by \code{\link{kruskal.test}}.

#' @references Tarnai, C., & Rost, J. (1990). \emph{Identifying aberrant response patterns in the Rasch model: the Q index}. Münster: ISF.
#' @references Andersen, E. B. (1973). A goodness of fit test for the rasch model. \emph{Psychometrika, 38}(1), 123–140. 

#' @examples
#' #######################
#' data(bfiN) # get some data
#' ip <- pair(daten = bfiN,m = 6) # item parameters according the partial credit model
#' Q(ip)
#' 
#' Qtest(ip)
#' 
#' ### with data an thresholds as external objects #####
#' threshold <- matrix(seq(-3,3,length.out = 9),ncol = 3)
#' dimnames(threshold) <- list(c("I1","I2","I3"),c("1","2","2"))
#' threshold
#' resp_vec <- c(3,0,2,1,2,2,2,2,1,3,0,NA,NA,0,2,3,NA,2,NA,2,1,2,NA,1,2,2,NA)
#' resp_emp <- matrix(resp_vec,ncol = 3,byrow = TRUE)
#' colnames(resp_emp) <- c("I1","I2","I3")
#' resp_emp
#' Qindex <- Q(data = resp_emp,threshold = threshold)
#' cbind(resp_emp,Qindex)
#' 
#' Qtest_res <- Qtest(data = resp_emp,threshold = threshold)
#' Qtest_res
#' 
#' #### unequal number of thresholds ###################
#' threshold <- matrix(seq(-3,3,length.out = 9),ncol = 3)
#' dimnames(threshold) <- list(c("I1","I2","I3"),c("1","2","2"))
#' threshold[2,3] <- NA
#' 
#' resp_vec <- c(3,0,2,1,2,2,2,2,1,3,0,NA,NA,0,2,3,NA,2,NA,2,1,2,NA,1,2,2,NA)
#' resp_emp <- matrix(resp_vec,ncol = 3,byrow = TRUE)
#' colnames(resp_emp) <- c("I1","I2","I3")
#' resp_emp
#' Qindex <- Q(data = resp_emp,threshold = threshold)
#' cbind(resp_emp,Qindex)
#' 
#' Qtest_res <- Qtest(data = resp_emp,threshold = threshold)
#' Qtest_res

Qtest <- function(obj=NULL, data=NULL, threshold=NULL, split="median", splitseed="no", ...){
  #### gibt's überhaupt irgendwo daten ? ... und thresholds ....
  if(all(class(obj)==c("pair","list"))){
    daten <- obj$resp # empirical responses
    threshold <- obj$threshold
    m <- obj$m
  }
  if(all(class(obj)==c("pers","list"))){
    daten <- obj$pair$resp # empirical responses
    threshold <- obj$pair$threshold
    m <- obj$pair$m
  }
  if(is.null(obj)){
    if(any(c(is.null(data),is.null(threshold)))){stop("no data or thresholds provided")}
    daten <- data # empirical responses
    if(any(sapply(dimnames(threshold),is.null))){stop("no threshold or item names provided in argument 'threshold' ")}
    threshold <- threshold
    (ncol(threshold)+1)
    m <- sapply(as.list(as.data.frame(t(threshold))), function(x){length(na.omit(x))+1})
  }
 #### ENDE gibt's überhaupt irgendwo daten ? ... und thresholds ....
  
  #### abfragen der Teilungskriterien und teiler vorbereiten
  teil <- split  # übergabe an internes argument
  if(!(length(teil) > 1)) {  
    if(teil=="no"){
      teiler<-factor(rep(1,dim(daten)[1]))
      #OK
    }
    if(teil=="random"){
      if(is.numeric(splitseed)){set.seed(splitseed)}
      teiler<-factor(as.numeric(cut(sample(1:(dim(daten)[1])),2)))
      #OK
    }
    if((substr(teil, 1, 6)=="random") && (nchar(teil)>6)){ # enhancement on 26-2-2017
      nteil<-as.numeric(unlist(strsplit(teil,".",TRUE))[2]) 
      if(is.numeric(splitseed)){set.seed(splitseed)}
      teiler<-factor(as.numeric(cut(sample(1:(dim(daten)[1])),nteil)))
      #OK
    }      
    if(teil=="mean"){
      daten<-as.matrix(daten)
      rscore<-rowSums(daten,na.rm = TRUE)
      teiler<-factor(ifelse(rscore > round(mean(rscore)),"above mean" ,"mean and below" ))
      #OK
    }
    if(teil=="median"){
      daten<-as.matrix(daten)
      rscore<-rowSums(daten,na.rm = TRUE)
      teiler<-factor(ifelse(rscore > median(rscore),"above median" ,"median and below" ))
      #OK
    }
    if(teil=="score"){
      daten<-as.matrix(daten)
      rscore<-rowSums(daten,na.rm = TRUE)
      rscore_ne <- rscore
      rscore_ne[rscore_ne==0] <- 1
      maxscore <- sum(apply(threshold,1,function(x){length(na.omit(x))})) # OK
      rscore_ne[rscore_ne==maxscore] <-maxscore-1
      #         
      teiler<-factor(rscore_ne)
      #OK
    }
    
  }
  
  if((is.integer(teil)) | (is.numeric(teil)) | (is.factor(teil))){
    #teiler<-daten[,teil]
    if( (dim(daten)[1])!=length(teil) ){stop("length of argument 'split' dose not match with 'data'")}
    teiler<-factor(teil)
    #if (class(teiler)=="factor"){teiler<-(as.numeric(teiler))}
    #if (min(teiler!=1)){stop("argument teil is not valid specified")}
    # daten<-daten[,-teil]
    #OK
  }
  #### ENDE abfragen der teilungskriterien und teiler vorbereiten  
 
  Q_fit <- Q(obj=obj, data=data, threshold=threshold, ...=...)
   
  # class(Q_erg); class(teiler)
  split <- teiler
  
  teststat <- kruskal.test(x = Q_fit, g = split)
  
  return(teststat) 
}
  