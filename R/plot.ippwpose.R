#' @method plot
########################### hier die summary method #############################
plot.ippwpoSE<-function(x, ci=2, col.error=1:6, col.lines=1:6,type="b",xlab="",pch=c(1:(dim(x$parameter)[2]-1)),las=3,cex.axis = 0.8,bereich="auto",main=deparse(substitute(x)), ...){
  thresholds<-x$parameter[,1:(dim(x$parameter)[2]-1)]
  SEthresholds<-x$SE[,1:(dim(x$SE)[2]-1)]
  
  ##### plotingrange festlegen mit leerplot
  ## automatische y achsen skalierung
  if((bereich)[1]=="auto"){ 
  if(!is.na(sum(SEthresholds))) {
    Item_thresholds<-rep( c((max(thresholds,na.rm=TRUE)+max(SEthresholds,na.rm=TRUE)*2),  (min(thresholds,na.rm=TRUE)-max(SEthresholds,na.rm=TRUE)*2)), length.out=((dim(x$parameter)[1])))
  }
  if(is.na(sum(SEthresholds))) {Item_thresholds <- SEthresholds }
  }
  ## feste vorgegebene y achsen skalierung
  if(class(bereich)=="numeric"){
    Item_thresholds<-rep( bereich, length.out=((dim(x$parameter)[1])))   
  }
  
  # empty plot
  plot(x=1:((dim(x$parameter)[1])), y=Item_thresholds,type="n",pch=pch,xlab=xlab,xaxt="n",main=main, ...)#,...
  
  matplot(thresholds,add=TRUE,pch=pch,type=type,xlab=xlab,xaxt="n",col = col.lines,...)#,...
  
  if(is.na(sum(SEthresholds))) {cat("no plot for stdandard error")}
  if(!is.na(sum(SEthresholds))) {
    
    for (i in 1:(dim(x$parameter)[2]-1)){
    segments( 1:(dim(x$parameter)[1]), thresholds[,i]+SEthresholds[,i]*ci, 1:(dim(x$parameter)[1]), thresholds[,i]-SEthresholds[,i]*ci ,col=col.error[i],... )#,... 
    segments( 1:(dim(x$parameter)[1])-((.3*(SEthresholds[,i]!=0))*ci), thresholds[,i]+SEthresholds[,i]*ci, 1:(dim(x$parameter)[1])+((.3*(SEthresholds[,i]!=0))*ci), thresholds[,i]+SEthresholds[,i]*ci ,col=col.error[i],...  )#,... 
    segments( 1:(dim(x$parameter)[1])-((.3*(SEthresholds[,i]!=0))*ci), thresholds[,i]-SEthresholds[,i]*ci, 1:(dim(x$parameter)[1])+((.3*(SEthresholds[,i]!=0))*ci), thresholds[,i]-SEthresholds[,i]*ci ,col=col.error[i],...  )#,...  
    
    }
    
    }
  
  
  axis(1, 1:dim(thresholds)[1], labels=rownames(thresholds),las=las,cex.axis=cex.axis,...)#,...
}