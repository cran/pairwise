### ----------- dataprep1 ---------------------
# dataprep1<-function(X){
#   X<-as.matrix(X)
#   if(length(colnames(X))==0){
#     Iname<-nchar(paste(dim(X)[2]))
#     colnames(X)<-paste("I",formatC(1:dim(X)[2], width = Iname, format = "d", flag = "0"),sep="") 
#     # cat("no item names found in data" ,"\n", "items are named", colnames(X)[1], "(first item) to",  colnames(X)[dim(X)[2]],"(last item)","\n")
#   }
#   #if(length(rownames(X))==0){  # personen werden immer neu nummeriert
#   Pname<-nchar(paste(dim(X)[1]))
#   rownames(X)<-paste("P",formatC(1:dim(X)[1], width = Pname, format = "d", flag = "0"),sep="")  
#   # cat("no person names (IDs) found in data" ,"\n", "persons are named", rownames(X)[1], "(first row) to",  rownames(X)[dim(X)[1]],"(last row)","\n")
#   #}
#   return(X)
# }


dataprep1<-function(X){
  options(stringsAsFactors = FALSE)# added 22. juni 2020
  
  if( any(class(X)=="list") ){
    LX <- X # back up user data
    if(length(unique(c(sapply(X, dim))))>2){stop("dimensions of your data from different Measurement sources do not match -- check your data")} # prüfen ob all daten gleich groß sind
    rt <- length(LX)# anzahl der Messpunkte (rater / zeiten)
    X <- do.call(rbind,LX)
    X <-as.matrix(X)
    
    if(length(colnames(X))==0){
      Iname<-nchar(paste(dim(X)[2]))
      colnames(X)<-paste("I",formatC(1:dim(X)[2], width = Iname, format = "d", flag = "0"),sep="") 
      cat("no item names found in data" ,"\n", "items are named", colnames(X)[1], "(first item) to",  colnames(X)[dim(X)[2]],"(last item)","\n")
    }
    
    if(all(sapply(LX,function(x){length(rownames(x))}) == 0)){ # wenn nirgends zeilennamen vorh. sind
      Pname<-nchar(paste( dim(X)[1]/rt ))
      Rname<-nchar(paste(rt))
      rownames(X)<-paste(rep(paste("R",formatC(1:rt, width = Rname, format = "d", flag = "0"),sep = ""),each=nrow(LX[[1]])),rep(paste("P",formatC(1:(dim(X)[1]/rt), width = Pname, format = "d", flag = "0"),sep=""),times=rt),sep="")  
      cat("no person names (IDs) found in data" ,"\n", "persons are named", rownames(X)[1], "(first row) to",  rownames(X)[dim(X)[1]],"(last row)","\n")
    }
    if(any(sapply(LX,function(x){length(rownames(x))}) != 0)){ # wenn irgendwo zeilennamen vorh. sind
      usr_rownames <- rownames(LX[[which(sapply(LX,function(x){length(rownames(x))}) != 0)]])
      if(is.null(names(LX))){
        Rname<-nchar(paste(rt))
        rownames(X)<-paste(rep(paste("R",formatC(1:rt, width = Rname, format = "d", flag = "0"),sep = ""),each=nrow(LX[[1]])),rep(usr_rownames,times=rt),sep="")
      }
      if(!is.null(names(LX))){
        rownames(X)<-paste(rep(names(LX),each=nrow(LX[[1]])),rep(usr_rownames,times=rt),sep="")
      }
    }
  return(X)
  }# new if condition for multi rater / repeated measures 

  
if( any(class(X)!="list") ){  X<-as.matrix(X)
  if(length(colnames(X))==0){
    Iname<-nchar(paste(dim(X)[2]))
    colnames(X)<-paste("I",formatC(1:dim(X)[2], width = Iname, format = "d", flag = "0"),sep="") 
    cat("no item names found in data" ,"\n", "items are named", colnames(X)[1], "(first item) to",  colnames(X)[dim(X)[2]],"(last item)","\n")
  }
  if(length(rownames(X))==0){
    Pname<-nchar(paste(dim(X)[1]))
    rownames(X)<-paste("P",formatC(1:dim(X)[1], width = Pname, format = "d", flag = "0"),sep="")  
    cat("no person names (IDs) found in data" ,"\n", "persons are named", rownames(X)[1], "(first row) to",  rownames(X)[dim(X)[1]],"(last row)","\n")
  }
return(X)
}
}