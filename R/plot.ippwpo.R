#' @method plot
########################### hier die summary method #############################
plot.ippwpo<-function(x, type="b",xlab="",pch=c(1:dim(x$threshold)[2]),las=3,cex.axis = 0.8,...){
  thresholds<-x$threshold
  matplot(thresholds,pch=pch,type=type,xlab=xlab,xaxt="n",...)
  axis(1, 1:dim(thresholds)[1], labels=rownames(thresholds),las=las,cex.axis=cex.axis,...)
}