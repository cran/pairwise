#' @title The Fischer-Scheiblechner Statistic S on item level (Wald like Test) 
#' @export pairwise.S
#' @exportClass pair_S
#' @description This function calculates the S-statistic on item level proposed by Fischer and Scheiblechner (1970) on item level for dicho- or polytomous item response formats by splitting the data into two subsamples. For polytomous Items the test is performed on item category level. Several splitting options are available (see arguments). The S-statistic is also mentioned in van den Wollenberg, (1982) -- an article in Psychometrika, which might be available more easily (see details).
#' 
#'@details The data is splitted in two subsamples and then item thresholds, the parameter (Sigma) and their standard errors (SE) for the items according the PCM (or RM in case of dichotonimies) are calculated for each subsample. This function internaly calls the function \code{\link{pairSE}}. Additional arguments (see description of function \code{\link{pairSE}}) for parameter calculation are passed through.
#' This item fit statistic is also (perhaps misleadingly) namend as 'Wald test' in other R-packages. The S-statistic, as implemented in \code{pairwise}, is defined according to Fischer and Scheiblechner (1970); see also equation (3) in van den Wollenberg, (1982), p. 124 in the following equation:
#'  \deqn{ { S }_{ i }=\frac { { \hat { \sigma  }  }^{ (1) }_{ i }-{ \hat { \sigma  }  }^{ (2) }_{ i } }{  \sqrt { { \left( { { S } }^{ (1) }_{ \hat { \sigma  } _{ i } } \right)  }^{ 2 }+{ \left( { { S } }^{ (2) }_{ \hat { \sigma  } _{ i } } \right)  }^{ 2 }   } }  }
#'where \eqn{{\hat { \sigma  }  }^{ (1) }_{ i }} is the estimate of the item parameter of subsample 1, \eqn{{\hat { \sigma  }  }^{ (2) }_{ i }} is the estimate of the item parameter of subsample 2 and \eqn{ { S }^{ (1) }_{ \hat { \sigma  } _{ i } }} and \eqn{ { S }^{ (2) }_{ \hat { \sigma  } _{ i } }} are the respective standard errors.
#'In Fischer (1974), p. 297, the resulting test statistic (as defined above) is labeled with \eqn{Z_i}, as it is asymtotically normally distributed. Contrary to the 'Wald-type' test statistic \eqn{W_i}, which was drived by Glas and Verhelst (2005) from the (general) \eqn{\chi^2} distributed test of statistical hypotheses concerning several parameters, which was introduced by Wald (1943).
#'
#' @section A note on standard errors: Estimation of standard errors is done by repeated calculation of item parameters for subsamples of the given data. This procedure is mainly controlled by the arguments \code{nsample} and \code{size} (see arguments in \code{\link{pairSE}}). With regard to calculation time, the argument \code{nsample} is the 'time killer'. On the other hand, things (estimation of standard errors) will not necessarily get better when choosing large values for \code{nsample}. For example choosing \code{nsample=400} will only result in minimal change for standard error estimation in comparison to (\code{nsample=30}) which is the default setting (see examples). 
#'
#' @param daten a data.frame or matrix with optionaly named colums (names of items), potentially with missing values, comprising polytomous or dichotomous (or mixed category numbers) responses of \code{n} respondents (rows) on \code{k} items (colums) coded starting with 0 for lowest category to \emph{m}-1 for highest category, with \emph{m} beeing a vector (with length k) with the number of categories for the respective item.
#' @param m an integer (will be recycled to a vector of length k) or a vector giving the number of response categories for all items - by default \code{m = NULL}, \code{m} is calculated from data, assuming that every response category is at least once present in data. For sparse data it is strongly recomended to explicitly define the number of categories by defining this argument.
#' 
#' @param split Specifies the splitting criterion. Basically there are three different options available - each with several modes - which are controlled by passing the corresponding character expression to the argument. 
#' 
#' 1) Using the rawscore for splitting into subsamples with the following modes: \code{split = "median"} median raw score split - high score group and low score group; \code{split = "mean"} mean raw score split - high score group and low score group.
#' 
#' 2) Dividing the persons in \code{daten} into subsamples with equal size by random allocation with the following modes: \code{split = "random"} (which is equivalent to \code{split = "random.2"}) divides persons into two subsamples with equal size. In general the number of desired subsamples must be expressed after the dot in the character expression - e.g. \code{split = "random.6"} divides persons into 6 subsamples (with equal size) by random allocation etc.
#' 
#' 3) The third option is using a manifest variable as a splitting criterion. In this case a vector with the same length as number of cases in \code{daten} must be passed to the argument grouping the data into subsamples. This vector should be coded as \code{"factor"} or a \code{"numeric"} integer vector with min = 1.
#' 
#' @param splitseed numeric, used for \code{set.seed(splitseed)} for random splitting - see argument \code{split}.
#' 
#' @param verbose logical, if \code{verbose = TRUE} (default) a message about subsampling is sent to console when calculating standard errors.
#' @param ... additional arguments \code{nsample}, \code{size}, \code{seed}, \code{pot} for caling \code{\link{pairSE}} are passed through - see description for \code{\link{pairSE}}.
#' 
#' @return A (list) object of class \code{"pair.S"} containing the test statistic and item difficulty parameter sigma and their standard errors for the two or more subsamples.
#' @exportClass pair.S
#' @references description of function \code{\link{pairSE}}\code{{pairwise}}.
#' @references Fischer, G. H., & Scheiblechner, H. (1970). Algorithmen und Programme fuer das probabilistische Testmodell von Rasch. \emph{Psychologische Beitrage}, (12), 23–51.
#' @references van den Wollenberg, A. (1982). Two new test statistics for the rasch model. \emph{Psychometrika, 47}(2), 123–140. https://doi.org/10.1007/BF02296270
#' @references Glas, C. A. W., & Verhelst, N. D. (1995). \emph{Testing the Rasch Model}. In G. Fischer & I. Molenaar (Eds.), Rasch models: Foundations, recent developments, and applications. New York: Springer.
#' @references Wald, A. (1943). Tests of statistical hypotheses concerning several parameters when the number of observations is large. \emph{Transactions of the American Mathematical Society, 54}(3), 426–482. https://doi.org/10.1090/S0002-9947-1943-0012401-3
#' @references Fischer, G. H. (1974). \emph{Einführung in die Theorie psychologischer Tests}. Bern: Huber.



#' @examples ##########
#' data("kft5")
#' S_ran_kft <- pairwise.S(daten = kft5,m = 2,split = "random")
#' summary(S_ran_kft)
#' summary(S_ran_kft,thres = FALSE)
#' #### polytomous examples
#' data(bfiN) # loading example data set
#' data(bfi_cov) # loading covariates to bfiN data set
#' 
#' # calculating itemparameters and SE for two subsamples by gender
#' S_gen <- pairwise.S(daten=bfiN, split = bfi_cov$gender)
#' summary(S_gen)
#' summary(S_gen,thres = FALSE)
#' 
#' # other splitting criteria
#' \dontrun{
#' S_med <- pairwise.S(daten=bfiN, split = "median")
#' summary(S_med)
#' 
#' S_ran<-pairwise.S(daten=bfiN, split = "random")
#' summary(S_ran)
#' 
#' S_ran.4<-pairwise.S(daten=bfiN, split = "random.4")
#' summary(S_ran.4) # currently not displayed
#' 
#' ###### example from details section 'Some Notes on Standard Errors' ########
#' S_def<-pairwise.S(daten=bfiN, split = "random",splitseed=13)
#' summary(S_def)
#' ######
#' S_400<-pairwise.S(daten=bfiN, split = "random", splitseed=13 ,nsample=400)
#' summary(S_400) 
#' }
#' 
############## funktions beginn ########################################################

pairwise.S<-function(daten, m=NULL, split="random", splitseed="no", verbose=FALSE, ...){ 
  call <- match.call()
#### abfragen der teilungskriterien und teiler vorbereiten
  teil <- split  # übergabe an internes argument
if(!(length(teil) > 1)) {  
  if(teil=="no"){
    teiler<-rep(1,dim(daten)[1])
    #OK
  }
  if(teil=="random"){
    if (class(splitseed)=="numeric"){set.seed(splitseed)}
    teiler<-as.numeric(cut(sample(1:(dim(daten)[1])),2))
    #OK
    }
  if((substr(teil, 1, 6)=="random") && (nchar(teil)>6)){
    nteil<-as.numeric(unlist(strsplit(teil,".",TRUE))[2]) 
    if (class(splitseed)=="numeric"){set.seed(splitseed)}
    teiler<-as.numeric(cut(sample(1:(dim(daten)[1])),nteil))
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
  
}
  
  if((class(teil)=="integer") | (class(teil)=="numeric") | (class(teil)=="factor")){
    #teiler<-daten[,teil]
    if( (dim(daten)[1])!=length(teil) ){stop("length of argument 'split' dose not match with 'data'")}
    teiler<-teil
    #if (class(teiler)=="factor"){teiler<-(as.numeric(teiler))}
    #if (min(teiler!=1)){stop("argument teil is not valid specified")}
    # daten<-daten[,-teil]
    #OK
  }
#### ENDE abfragen der teilungskriterien und teiler vorbereiten  
# vorbereiten des objektes datalist anhand des vectors teiler
subsamp <- names(table(teiler))

datalist<-vector("list",length=length(subsamp)) #vorber. leere datalist   
 for (i in 1:length(datalist)){
   datalist[[i]]<-daten[which(teiler==subsamp[i]),]  #hier die zuordnung der subsamples aus daten
 }
names(datalist) <- paste(subsamp,"sample")
estimate_list <- lapply(datalist, pairSE, m=m, verbose=verbose, ...) 
combi_index_list <- as.list(data.frame(combn(x = length(datalist),m = 2)))
# combi_index_list[[1]][2]
# com_ind_vec <- combi_index_list[[1]]
# est_li <- estimate_list
# names(est_li)

waldi <- function(est_li,com_ind_vec){
  zaehler_thr <- est_li[[com_ind_vec[1]]]$threshold -   est_li[[com_ind_vec[2]]]$threshold
  nenner_thr <- sqrt(est_li[[com_ind_vec[1]]]$SE^2 + est_li[[com_ind_vec[2]]]$SE^2)
  waldi_erg1_thr <- zaehler_thr/nenner_thr
  pwert_thr <- (1-pnorm(abs(waldi_erg1_thr)))*2
  zaehler_sig <- est_li[[com_ind_vec[1]]]$sigma -   est_li[[com_ind_vec[2]]]$sigma
  nenner_sig <- sqrt(est_li[[com_ind_vec[1]]]$SEsigma^2 + est_li[[com_ind_vec[2]]]$SEsigma^2)
  waldi_erg1_sig <- zaehler_sig/nenner_sig
  pwert_sig <- (1-pnorm(abs(waldi_erg1_sig)))*2
  waldcoef <- list(threshold=list(z=waldi_erg1_thr, p=pwert_thr), sigma=list(z=waldi_erg1_sig, p=pwert_sig) )
  return(waldcoef)
  }

waldtest <- lapply(X = combi_index_list ,FUN = function(x){ waldi(est_li = estimate_list,com_ind_vec = x) })

vergl_namen <- sapply(X = combi_index_list,FUN = function(x){paste(names(estimate_list[x]),collapse = " - ")})

names(waldtest) <- vergl_namen

erg <- list(S=waldtest,call=call)

class(erg) <- c("pair.S","list")

return(erg)

}
