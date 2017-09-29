#' @title Person Fit Index Q
#' @export Q
#' @description function for calculating the person fit index Q, which was proposed by Tarnai and Rost (1990).
#' @param obj an object of class \code{"pers"} or class \code{"pair"}as a result from function \code{\link{pers}} or \code{\link{pair}} respectively.
#' @param ... not used so far.
#' @details The person Q-index proposed by Tarnai and Rost, (1990) is solely based on the empirical responses and the item parameters. Thus the computation of person parameters using the function \code{\link{pers}} is not required - see examples. But for convenience return objects of both functions are accepted in function \code{Q}.  
#' @return a vector holding the Q-index for every person.
#' @references Tarnai, C., & Rost, J. (1990). \emph{Identifying aberrant response patterns in the Rasch model: the Q index}. Münster: ISF.

#' @examples
#' #######################
#' data(bfiN) # get some data
#' ip <- pair(daten = bfiN,m = 6) # item parameters according the partial credit model
#' Q(ip)

Q <- function(obj, ...){
  if(all(class(obj)==c("pair","list"))){
    resp_emp <- obj$resp # empirical responses
    threshold <- obj$threshold
    m <- obj$m
    sigma <- obj$sigma
    N <- dim(obj$resp)[1] # number of persons
  }
  if(all(class(obj)==c("pers","list"))){
    resp_emp <- obj$pair$resp # empirical responses
    threshold <- obj$pair$threshold
    m <- obj$pair$m
    sigma <- obj$pair$sigma
    N <- dim(obj$pair$resp)[1] # number of persons
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
  r_sigma_emp <- ((apply(X = resp_emp_tr,1,FUN = function(x){sum(x*thrv)})))

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