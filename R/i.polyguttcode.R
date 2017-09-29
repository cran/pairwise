polyguttcode <- function(X,m_v){
  #guttmann dummy coding of boundaries for polytomous items
  ## helper function for columnvector -------------------
  polybincode <- function(x, m){
    # function assumes numerix vector 'x' with coding 0 to (m-1)
    xl <- as.list(data.frame(matrix(data = rep(x,times=(m-1)),nrow = length(x) ,ncol = (m-1) )))
    erg <- mapply(FUN = function(b,d){(d>=b)*1},b=1:(m-1),d=xl,USE.NAMES=TRUE)
    # class(erg)
    # head(cbind(x,erg))
    return(erg)
  }
  ## main functionality for matrix inputs -----------------  
  ## 'X' polytomous data matrix with coding 0 to (m-1)
  ## 'm' numeric vector with number of categories for each variable (column) in X
  e1 <- mapply(FUN=function(x, m){polybincode(x,m)}, x=as.data.frame(X), m=m_v, SIMPLIFY = FALSE)
  # dim(polybincode(x,m))
  # length(e1)
  # lapply(e1,dim)
  # lapply(e1,class)
  e2 <- do.call(cbind,e1)
  colnames(e2) <- c(mapply(FUN = function(nam,m){paste(nam, 1:(m-1), sep = ".")} ,nam = colnames(X), m=m_v, SIMPLIFY = TRUE))
  rownames(e2) <- rownames(X) 
  return(e2)
}