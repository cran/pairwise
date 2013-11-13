#' @method plot
############# hier die plot method dichotom mit SE #######################
plot.ipSE<-function(x, ci=2,col.error="blue",type="b",xlab="",pch=43,las=3,cex.axis = 0.5,bereich="auto",main=deparse(substitute(x)), ...){   
  Sigma<-x$Sigma
  SE<-x$SE
  
  ##### plotingrange festlegen mit leerplot
  ## automatische y achsen skalierung
if((bereich)[1]=="auto"){   
   if(!is.na(sum(SE))) {
  Item_Sigma<-rep( c((max(Sigma,na.rm=TRUE)+max(SE,na.rm=TRUE)*2),  (min(Sigma,na.rm=TRUE)-max(SE,na.rm=TRUE)*2)), length.out=length(Sigma))
  }
  if(is.na(sum(SE))) {Item_Sigma <- Sigma }
} 
  ## feste vorgegebene y achsen skalierung
  if(class(bereich)=="numeric"){
    Item_Sigma<-rep( bereich, length.out=((length(x$Sigma))))   
  }  
  
  # empty plot
  plot(x=1:length(Sigma), y=Item_Sigma,type="n",pch=pch,xlab=xlab,xaxt="n", main=main, ...)
  ##### start plotting
  points(x=1:length(Sigma), y=Sigma, type=type,pch=pch,...)
  
  if(is.na(sum(SE))) {cat("no plot for stdandard error")}
  if(!is.na(sum(SE))) {
  segments( 1:length(Sigma), Sigma+SE*ci, 1:length(Sigma), Sigma-SE*ci ,col=col.error,...  )
  segments( 1:length(Sigma)-((.3*(SE!=0))*ci), Sigma+SE*ci, 1:length(Sigma)+((.3*(SE!=0))*ci), Sigma+SE*ci ,col=col.error,...  )
  segments( 1:length(Sigma)-((.3*(SE!=0))*ci), Sigma-SE*ci, 1:length(Sigma)+((.3*(SE!=0))*ci), Sigma-SE*ci ,col=col.error,...  ) 
  }
  axis(1, 1:length(Sigma), labels=names(Sigma),las=las,cex.axis=cex.axis,...)
}