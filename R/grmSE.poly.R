#' @title Graphical Model Check for polytomous PCM
#' @export grmSE.poly
#' @description This function makes the basic calculations for the graphical model check for polytomous itemresponse formats. It is more or less a wraper function, internaly calling the function \code{\link{ipSE.poly}}. Several splitting options are available (see arguments).    
#' 
#'@details The data is splitted in two or more subsamples and then item thresholds, the parameter (Sigma) and their standard errors (SE) for polytomous items according the PCM  are calculatetd for each subsample. Additional arguments (see description of function \code{\link{ipSE.poly}}) for parameter calculation are passed through. 
#'
#'WARNING: When using data based on booklet designs with systematically missing values (by design) you have to ensure that in each of the booklet the maximum raw value to reach is equal while using the raw value as splitting criterion.
#' 
#' @section A Note on Standard Errors: Estimation of standard errors is done by repeated calculation of item parameters for subsamples of the given data. This procedure is mainly controlled by the arguments \code{nsample} and \code{size} (see arguments). With regard to calculation time, the argument \code{nsample} is the 'time killer'. On the other hand, things (estimation of standard errors) will not necessarily get better when choosing large values for \code{nsample}. For example choosing \code{nsample=400} will only result in minimal change for standard error estimation in comparison to (\code{nsample=30}) which is the default setting (see examples). 
#'
#' @param daten daten a data matrix with optionaly named colums (names of items) or a data.frame, potentially with missing values, comprising polytomous responses of respondents (rows) on some items (colums) coded starting with 0 for lowest category to \emph{m}-1 for highest category, with \emph{m} beeing the number of categories for all items.
#' @param m number of response categories for all items - by default \emph{m} is defined as \code{m = max(daten,na.rm=TRUE)+1}.
#' 
#' @param teil Specifies the splitting criterion. Basicly there are three different options available - each with several modes - which are controlled by passing the corresponding character expression to the argument. 
#' 
#' 1) Using the rawscore for splitting into subsamples with the following modes: \code{teil = "median"} median raw score split - high score group and low score group; \code{teil = "mean"} mean raw score split - high score group and low score group; \code{teil = "score"} splitting \code{daten} into as many subsamples as there are raw score groups (discarding min and max score group) 
#' 
#' 2) Dividing the persons in \code{daten} into subsamples with equal size by random allocation with the following modes: \code{teil = "random"} (which is equivalent to \code{teil = "random.2"}) divides persons into two subsamples with equal size. In general the number of desired subsamples must be expressed after the dot in the character expression - e.g. \code{teil = "random.6"} divides persons into 6 subsamples (with equal size) by random allocation etc. 
#' 
#' 3) The third option is using a manifest variable as a splitting criterion. In this case a numeric indicating the column number of the variable in \code{daten} must be passed to the argument - e.g. \code{teil = 1} indicates that the variable in the first column of \code{daten} will be used as splitting criterion - (this variable will of course be used only as splitting criterion). The variable in \code{daten} should be coded as \code{factor} or a numeric integer vector with min = 1 if \code{daten} is a matrix.    
#' 
#' @param splitseed numeric, used for \code{set.seed(splitseed)} for random splitting - see argument \code{teil}
#'   
#' @param sortdif logical, In contrast to to the default setting in the function \code{\link{ipSE.poly}} the argument \code{sortdif} here is set to \code{FALSE}, so the items will kept in original order - see description for \code{\link{ipSE.poly}}.
#' 
#' @param ... additional arguments \code{nsample}, \code{size}, \code{seed}, \code{pot} for caling \code{\link{ipSE.poly}} are passed through - see description for \code{\link{ipSE.poly}}.
#' 
#' @return A (list) object of class grmSEpo containing the item difficulty parameter sigma and their standard errors for two or more subsamples.
#' @exportClass grmSEpo
#' @references description of function \code{\link{ipSE.poly}}\code{{pairwise}}.
#' 
#' @examples data(bfiN) # loading example data set
#' 
#' # calculating itemparameters and SE for two random allocated subsamples
#' grmSEpoly<-grmSE.poly(daten=bfiN, teil = "random") 
#' 
#' # some examples for plotting options
#' # plotting item difficulties for two subsamples against each other 
#' # with elipses for a CI = 95% .
#' plot(grmSEpoly) 
#' 
#' # using triangles as plotting pattern
#' plot(grmSEpoly,pch=2) 
#' 
#' #plotting without CI ellipses
#' plot(grmSEpoly,ci=0,pch=2) 
#' 
#' # plotting with item names
#' plot(grmSEpoly,itemNames=TRUE) 
#' 
#' # Changing the size of the item names
#' plot(grmSEpoly,itemNames=TRUE, cex.names = 1.3)
#' 
#' # Changing the color of the CI ellipses
#' plot(grmSEpoly,itemNames=TRUE, cex.names = .8, col.error="green")
#' 
#' ###### example from details section 'Some Notes on Standard Errors' ########
#' # grmSEpoly<-grmSE.poly(daten=bfiN, teil = "random",splitseed=13)
#' # plot(grmSEpoly)
#' ######
#' # grmSEpoly_400<-grmSE.poly(daten=bfiN, teil = "random", splitseed=13 ,nsample=400)
#' # plot(grmSEpoly_400) 
#' 
#' 
############## funktions beginn ########################################################

grmSE.poly<-function(daten, m=max(daten,na.rm=TRUE)+1, teil="no", splitseed="no", sortdif=FALSE, ...){ 
#### abfragen der teilungskriterien und teiler vorbereiten
  if(teil=="no"){
    teiler<-rep(1,dim(daten)[1])
    #OK
  }
  if(teil=="random"){
    if (class(splitseed)=="numeric"){set.seed(splitseed)}
    teiler<-as.numeric(cut(sample(1:(dim(daten)[1])),2))
    #OK
    }
  if(nchar(teil)>6){
    nteil<-as.numeric(unlist(strsplit(teil,".",TRUE))[2]) 
    if (class(splitseed)=="numeric"){set.seed(splitseed)}
    teiler<-as.numeric(cut(sample(1:(dim(daten)[1])),nteil))
    #OK
  }     
  if(teil=="mean"){
    daten<-as.matrix(daten)
    rscore<-rowSums(daten,na.rm = TRUE)
    teiler<-ifelse(rscore >= mean(rscore),1 ,2 )
    #OK
  }
  if(teil=="median"){
    daten<-as.matrix(daten)
    rscore<-rowSums(daten,na.rm = TRUE)
    teiler<-ifelse(rscore <= median(rscore),1 ,2 )
    #OK
  }
  if(class(teil)=="numeric"){
    teiler<-daten[,teil]
    if (class(teiler)=="factor"){teiler<-(as.numeric(teiler))}
    if (min(teiler!=1)){stop("argument teil is not valid specified")}
    daten<-daten[,-teil]
    #OK
  }
#### ENDE abfragen der teilungskriterien und teiler vorbereiten  
# vorbereiten des objektes datalist anhand des vectors teiler
datalist<-vector("list",length=length(as.numeric(names(table(teiler))))) #vorber. leere datalist   
 for (i in 1:length(datalist)){
   datalist[[i]]<-daten[which(teiler==i),]  #hier die zuordnung der subsamples aus daten
 }      
     
     grmSEpo <- lapply(datalist, ipSE.poly, sortdif=sortdif, ...) 
     class(grmSEpo) <- c("grmSEpo","list")

  return(grmSEpo)
# summary.grmSEpo(grmSEpo)
}
