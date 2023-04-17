#' @title Person Fit Index Q
#' @export Q
#' @description function for calculating the person fit index Q, which was proposed by Tarnai and Rost (1990).
#' @param obj an object of class \code{"pers"} or class \code{"pair"}as a result from function \code{\link{pers}} or \code{\link{pair}} respectively.
#' @param data optional response data when object of class \code{"pers"} or class \code{"pair"} is not provided.
#' @param threshold optional in case that object of class \code{"pers"} or class \code{"pair"} is not provided. Threshold values as matrix with row and columnnames !! -- items as rows and thresholds as columns. Thresholds should be ordered from left to right, some items may have less thresholds than the others, in this case the respective row/column is filled with an NA value - see examples.
#' @param ... not used so far.
#' 
#' @details The person Q-index proposed by Tarnai and Rost, (1990) is solely based on the empirical responses and the item parameters. Thus the computation of person parameters using the function \code{\link{pers}} is not required - see examples. But for convenience return objects of both functions are accepted in function \code{Q}.  
#' @return a vector holding the Q-index for every person.
#' @references Tarnai, C., & Rost, J. (1990). \emph{Identifying aberrant response patterns in the Rasch model: the Q index}. Münster: ISF.

#' @examples
#' #######################
#' data(bfiN) # get some data
#' ip <- pair(daten = bfiN,m = 6) # item parameters according the partial credit model
#' Q(ip)
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




Q <- function(obj=NULL, data=NULL, threshold=NULL, ...){
  if(all(class(obj)==c("pair","list"))){
    resp_emp <- obj$resp # empirical responses
    threshold <- obj$threshold
    m <- obj$m
    # sigma <- obj$sigma
    # N <- dim(obj$resp)[1] # number of persons
  }
  if(all(class(obj)==c("pers","list"))){
    resp_emp <- obj$pair$resp # empirical responses
    threshold <- obj$pair$threshold
    m <- obj$pair$m
    # sigma <- obj$pair$sigma
    # N <- dim(obj$pair$resp)[1] # number of persons
  }
  if(is.null(obj)){
    if(any(c(is.null(data),is.null(threshold)))){stop("no data or thresholds provided")}
    resp_emp <- data # empirical responses
    if(any(sapply(dimnames(threshold),is.null))){stop("no threshold or item names provided in argument 'thresholds' ")}
    threshold <- threshold
    (ncol(threshold)+1)
    m <- sapply(as.list(as.data.frame(t(threshold))), function(x){length(na.omit(x))+1})
  }
  
  # names of indikator super matrix --------------------------------------------
  supernames <- do.call(c,mapply(FUN = function(v,t){paste(v,t,sep = ".")},v=dimnames(threshold)[[1]],t=lapply(X = m-1,function(x){1:x}),SIMPLIFY = FALSE))
  # threshold values as vector -------------------------------------------------
  thrv <- (do.call(c,lapply(X = as.list(as.data.frame(t(threshold))),FUN = na.omit)))
  thrv_ordered <- sort(thrv)
  # produce guttmann pattern (polytomous) by threshold order ------------------- 
  SMgutt <- matrix(data = 0,nrow = sum(m-1)+1,ncol = sum(m-1))
  SMgutt[lower.tri(SMgutt)] <- 1
  colnames(SMgutt) <- supernames[order(thrv)]
  SMgutt <- SMgutt[,supernames]
  SMgutt
  # GM <- matrix(data = 0,nrow = sum(m-1)+1,ncol = length(m))
  # colnames(GM) <- colnames(resp_emp)
  superindexgutt <- lapply(X = colnames(resp_emp) ,FUN = function(x){grep(pattern = x,x = colnames(SMgutt))})
  GM <- sapply(X = superindexgutt,FUN = function(x){ 
    if(length(x)>1){re <- rowSums(SMgutt[,x])};
    if(length(x)==1){re <- (SMgutt[,x])};re})
  colnames(GM) <- colnames(resp_emp)
  GM ## Guttmann pattern (by threshold difficulty)
  
  # multiply Guttmann supermatrix by thrv --------------------------------------
    r_sigma_Lgutt<- as.list((apply(X = SMgutt,1,FUN = function(x){sum(x*thrv)})))
  names(r_sigma_Lgutt) <- 0:(nrow(SMgutt)-1)
  
  # produce Anti guttmann pattern (polytomous) by threshold order --------------
  SMantigutt <- matrix(data = 1,nrow = sum(m-1)+1,ncol = sum(m-1))
  SMantigutt[lower.tri(SMgutt)] <- 0
  colnames(SMantigutt) <- supernames[order(thrv)]
  SMantigutt <- SMantigutt[,supernames]
  SMantigutt
  # GM <- matrix(data = 0,nrow = sum(m-1)+1,ncol = length(m))
  # colnames(GM) <- colnames(resp_emp)
  superindexgutt <- lapply(X = colnames(resp_emp) ,FUN = function(x){grep(pattern = x,x = colnames(SMantigutt))})
  antiGM <- sapply(X = superindexgutt,FUN = function(x){ 
    if(length(x)>1){re <- rowSums(SMantigutt[,x])};
    if(length(x)==1){re <- (SMantigutt[,x])};re})
  colnames(antiGM) <- colnames(resp_emp)
  antiGM ## Anti-Guttmann Pattern (by threshold difficulty)
  
  # multiply anti Guttmann supermatrix by thrv ---------------------------------
  r_sigma_Lagutt <- as.list((apply(X = SMantigutt,1,FUN = function(x){sum(x*thrv)})))
  names(r_sigma_Lagutt) <- 0:(nrow(SMantigutt)-1)
  
  # response data as supermatrix -----------------------------------------------
  resp_emp_list<-as.list(data.frame((resp_emp)))
  m_list <- as.list(m)
  # mm <- m_list[[2]]
  # vv <- resp_emp_list[[2]]
  resp_emp_tr_list <- mapply(FUN = function(mm, vv){sapply(X = 1:(mm-1),FUN = function(x){(vv>=x)*1})},   mm = m_list,  vv <- resp_emp_list, SIMPLIFY = FALSE)
  resp_emp_tr <- do.call(cbind, resp_emp_tr_list)
  rownames(resp_emp_tr) <- rownames(resp_emp) 
  colnames(resp_emp_tr) <- supernames
  
  # multiply empirical supermatrix by thrv ------------------------------------
  r_sigma_emp <- ((apply(X = resp_emp_tr,1,FUN = function(x){sum(x*thrv,na.rm = TRUE)}))) # JHH(20-10-2017): added na.rm=TRUE

  # all in vectors -------------------------------------------------------------
  # evtl. check na.rm=TRUE !!!
  AG <- unlist(r_sigma_Lagutt[as.character(rowSums(resp_emp,na.rm = TRUE))])
  GP <- unlist(r_sigma_Lgutt[as.character(rowSums(resp_emp,na.rm = TRUE))])
  
  # compute Q with vectors of summated sigmas ----------------------------------
  Q_ <- (r_sigma_emp-GP) / (AG-GP)
  test <- cbind(resp_emp_tr[,order(thrv)],resp_emp, Q_) # just internal test (not returned)
  
  ## just return the Q ---------------------------------------------------------
  res <- Q_
  
  return(res)
}