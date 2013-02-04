#' @method plot
########################### hier die summary method #############################
plot.ipSE<-function(x, ci=2,col.error="blue",type="b",xlab="",pch=43,las=3,cex.axis = 0.5,...){   
  Sigma<-x$Sigma
  SE<-x$SE

  ##### plotingrange festlegen mit leerplot
  if(!is.na(sum(SE))) {
  Item_Sigma<-rep( c((max(Sigma,na.rm=TRUE)+max(SE,na.rm=TRUE)*2),  (min(Sigma,na.rm=TRUE)-max(SE,na.rm=TRUE)*2)), length.out=length(Sigma))
  }
  if(is.na(sum(SE))) {Item_Sigma <- Sigma }
  # empty plot
  plot(x=1:length(Sigma), y=Item_Sigma,type="n",pch=pch,xlab=xlab,xaxt="n",...)
  ##### start plotting
  points(x=1:length(Sigma), y=Sigma, type=type,pch=pch,...)
  
  if(is.na(sum(SE))) {cat("no plot for stdandard error")}
  if(!is.na(sum(SE))) {
  segments( 1:length(Sigma), Sigma+SE*ci, 1:length(Sigma), Sigma-SE*ci ,col=col.error,...  )
  segments( 1:length(Sigma)-.3, Sigma+SE*ci, 1:length(Sigma)+.3, Sigma+SE*ci ,col=col.error,...  )
  segments( 1:length(Sigma)-.3, Sigma-SE*ci, 1:length(Sigma)+.3, Sigma-SE*ci ,col=col.error,...  ) 
  }
  axis(1, 1:length(Sigma), labels=names(Sigma),las=las,cex.axis=cex.axis,...)
}