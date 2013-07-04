#' @method plot
########################### hier die summary method #############################
plot.ippw<-function(x, type="b",xlab="",las=3,cex.axis = 0.5,...){
  Sigma<-matrix(x,ncol=1,dimnames = list(names(x), "Sigma"))
  plot(Sigma,type=type,xlab=xlab,xaxt="n",...)
  axis(1, 1:dim(Sigma)[1], labels=rownames(Sigma),,las=las,cex.axis=cex.axis)
}