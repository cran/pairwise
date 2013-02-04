#' @method plot
########################### hier die summary method #############################
plot.ippwpose<-function(x, ci=2, col.error=1:6, type="b",xlab="",pch=c(1:(dim(x$parameter)[2]-1)),las=3,cex.axis = 0.8,...){
  thresholds<-x$parameter[,1:(dim(x$parameter)[2]-1)]
  SEthresholds<-x$SE[,1:(dim(x$SE)[2]-1)]
  
  ##### plotingrange festlegen mit leerplot
  if(!is.na(sum(SEthresholds))) {
    Item_thresholds<-rep( c((max(thresholds,na.rm=TRUE)+max(SEthresholds,na.rm=TRUE)*2),  (min(thresholds,na.rm=TRUE)-max(SEthresholds,na.rm=TRUE)*2)), length.out=((dim(x$parameter)[1])))
  }
  if(is.na(sum(SEthresholds))) {Item_thresholds <- SEthresholds }
  # empty plot
  plot(x=1:((dim(x$parameter)[1])), y=Item_thresholds,type="n",pch=pch,xlab=xlab,xaxt="n",...)#,...
  
  matplot(thresholds,add=TRUE,pch=pch,type=type,xlab=xlab,xaxt="n",...)#,...
  
  if(is.na(sum(SEthresholds))) {cat("no plot for stdandard error")}
  if(!is.na(sum(SEthresholds))) {
    
    for (i in 1:(dim(x$parameter)[2]-1)){
    segments( 1:(dim(x$parameter)[1]), thresholds[,i]+SEthresholds[,i]*ci, 1:(dim(x$parameter)[1]), thresholds[,i]-SEthresholds[,i]*ci ,col=col.error[i],... )#,... 
    segments( 1:(dim(x$parameter)[1])-.3, thresholds[,i]+SEthresholds[,i]*ci, 1:(dim(x$parameter)[1])+.3, thresholds[,i]+SEthresholds[,i]*ci ,col=col.error[i],...  )#,... 
    segments( 1:(dim(x$parameter)[1])-.3, thresholds[,i]-SEthresholds[,i]*ci, 1:(dim(x$parameter)[1])+.3, thresholds[,i]-SEthresholds[,i]*ci ,col=col.error[i],...  )#,...  
    
    }
    
    }
  
  
  axis(1, 1:dim(thresholds)[1], labels=rownames(thresholds),las=las,cex.axis=cex.axis,...)#,...
}