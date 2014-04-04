#' @method summary grm
########################### hier die summary method #class grm #######################
summary.grm<-function(object,ci=2,...){
  fu1<-function(x,ci){ cbind(x$parameter[,"sigma"]-(x$SE[,"sigma"]*ci) ,  x$parameter[,"sigma"]+(x$SE[,"sigma"]*ci))   }
  erg<-lapply(object,fu1,ci) 
  dimnames(erg[[1]])[[2]]<-c("samp1_lower", "samp1_upper")
  dimnames(erg[[2]])[[2]]<-c("samp2_lower", "samp2_upper")
  erg1<-cbind(erg[[1]],erg[[2]])
  u1<-erg1[,1]
  o1<-erg1[,2]
  u2<-erg1[,3]
  o2<-erg1[,4]
  
  OK <- ( (u1<u2 & o1>u2) | (u1<o2 & o1>o2) ) | ( (u1<u2 & o1>o2) | (u1>u2 & o1<o2) )
  
  #OK <- ((erg1[,3] > erg1[,1]) | (erg1[,4] > erg1[,1]) ) | ((erg1[,3] > erg1[,2]) | (erg1[,4] > erg1[,2]) )
  
  erg2<-cbind(erg1,OK)
  
  return(erg2)
}