#' @method plot
########################### hier die summary method #############################
plot.grmSEpo<-function(x,itemNames=FALSE, cex.names=.8,ci=2,col.error="blue",type="b",xlab="",pch=43,las=3,cex.axis = 0.5,...){   
  #sonderfall 2 subsamples
  if (length(x)==2){
    Itemnames<-names(x[[1]]$parameter[,dim(x[[2]]$parameter)[2]])
    X<-x[[1]]$parameter[,dim(x[[2]]$parameter)[2]]
    Y<-x[[2]]$parameter[,dim(x[[1]]$parameter)[2]]
    XS<-x[[1]]$SE[,dim(x[[2]]$SE)[2]]
    YS<-x[[2]]$SE[,dim(x[[1]]$SE)[2]]

    ##### plotingrange festlegen mit leerplot
    xymax<-max(c(max(X),max(Y))) + 3*(max(c(max(XS),max(YS))))
    xymin<-min(c(min(X),min(Y))) - 3*(max(c(max(XS),max(YS))))
    Sample_1<-c(xymin,xymax); Sample_2<-c(xymin,xymax)
    plot(Sample_1,Sample_2,type="n",bty="n",...)
    # hilfsfunktion elipse
    eli<-function(x.cent,y.cent,xb,yh, ...){
      # plotten einer elipse
      nseg=360
      xx <- x.cent + xb*cos( seq(0,2*pi, length.out=nseg) )
      yy <- y.cent + yh*sin( seq(0,2*pi, length.out=nseg) )
      lines(xx,yy,col=col.error,...) 
    }
    ##############
    ##### plotten der grafik    
    if (itemNames==TRUE){pch=""}
    if (itemNames==TRUE){text(X,Y,Itemnames,cex=cex.names,...)}
    points(X,Y,pch=pch,...) 
    abline(0,1,col="red",...) 
    for (i in 1: length(X)){ eli(X[i],Y[i],XS[i]*ci,YS[i]*ci) }
}
  
  if (length(x)!=2){cat("actualy no plotting method for", length(x) ,"subsample(s) available","\n")  }
  
}
  