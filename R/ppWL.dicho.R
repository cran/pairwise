#' @title WL Person Parameter dichotomous 1PL
#' @export ppWL.dicho
#' @description Calculation of the person parameters for dichotomous items, given the item parameters and and the datamatrix (argument \code{daten}) containing the person respose vectors (rows), according the 1PL Rasch Model using an WL approach.
#' @details no detail in the moment.
#'
#' @param daten a matrix (or data.frame) containing (numeric) responses of the persons (rows) coded in the (0,1) manner.
#' @param SIGMA numeric vector with item (difficulty) parameters.
#' @param incidenz This is only relevant when items are assigned to different booklets. For such a booklet-design a matrix should be assigned to this argument with the same dimensions like \code{daten} containig 0 and 1 codes giving the information (for every person) if the respective item was in the booklet given to the person.    
#' @param na_treat numeric defining the type of treatment to missing responses in the argument \code{daten}. If set to \code{na_treat=0} (default) missing responses are treated as wrong answers. An option is to set \code{na_treat} to any value between 0 and 1 e.g.: \code{na_treat=0.5}  
#' @param limit numeric giving the limit at which accuracy the WL-algorithm stops.
#' @param iter numeric giving the maximum numer of iteration to perform.
#' @param tecout logical default set to\code{FALSE}. If set to \code{TRUE} the result will be a (very) long list with estimation details for every case in \code{daten}. In case of a booklet-design the list entrys will be divided by "booklet".
#' @return An object of class "ppWLdicho" or "list" (depending on \code{techout}) containing the person parameters.
#' @exportClass ppWLdicho
#' @examples ############
#' data(cog);data(cogBOOKLET) # loading reponse and allocation data
#' d<-(cog[cog$BOOKID!=14,]) # skip persons which got booklet No.14.
#' inc<-make.incidenz(tab=cogBOOKLET, bookid=d$BOOKID) # make just the incidenz matrix
#' result<-ppWL.dicho(daten=d[,4:34], SIGMA=itempar.dicho(d[,4:34]),incidenz = inc )
#' summary(result)
#' summary(result,FALSE) # return just the WL person estimates and their standard errors.


ppWL.dicho<-function(daten,SIGMA,incidenz=NULL,na_treat=0,limit=0.00001,iter=20,tecout=FALSE){ 
# deactivated argument: v_theta_init=NULL
# param v_theta_init optionaly a numeric vector giving the startvalues (for each person - row in \code{daten}) for estimation of the respective person parameter. Default is \code{log()} of the persons rawscore. 
  
##### check der Daten ---------------------
lowest_category<-(range(daten,na.rm=TRUE))[1]
higest_category<-(range(daten,na.rm=TRUE))[2]
stopifnot(lowest_category==0, higest_category==1)

##### aufbereiten der daten --------------- 
daten<-as.matrix(daten)
if(length(rownames(daten))==0){
  lname<-nchar(paste(dim(daten)[1]))
  rownames(daten)<-paste("P",formatC(1:dim(daten)[1], width = lname, format = "d", flag = "0"),sep="")  
  cat("no person IDs (names) found in data" ,"\n", "persons are named", rownames(daten)[1], "(first row) to",  rownames(daten)[dim(daten)[1]],"(last row)")
}  
N<-dim(daten)[1] # anzahl personen
k<-dim(daten)[2] # anzahl items

caseORDER<-(rownames(daten))

##### check der Argumente ---------------------
# if(length(v_theta_init)==0){v_theta_init<-log(rowSums(daten,na.rm=TRUE))}
# if(N!=length(v_theta_init)){
#  cat("wrong definition of start values","\n", "length of v_theta_init must match the number of persons","\n","natuaral logarithm of rawscores are used as startvalues","\n")
#  v_theta_init<-log(rowSums(daten,na.rm=TRUE)) }
if(k!=length(SIGMA)){cat("length of SIGMA dose not match with number of items","\n");stop }
if (length(incidenz)!=0){
  if (any(range(incidenz)!=c(0,1))) {cat("wrong definition of incidenz matrix","\n");stop   }
}
# einfacher Fall - kein bookletdesign
if (length(incidenz)==0){
object<-(apply(daten,1,thetaWL.dicho, sigma=SIGMA, na_treat=na_treat, limit=limit,iter=iter))

if (tecout==FALSE){ # the normal output   
pattern<- apply((sapply(object,function(x){(x$response)})), 2, function(x){paste(x,collapse="")})
theta<-sapply(object,function(x){x$theta})
SE<-sapply(object,function(x){x$SE})
raw<-sapply(object,function(x){sum(x$response,na.rm=TRUE)})
converge<-sapply(object,function(x){x$converge})
iterations<-sapply(object,function(x){x$iterations})
res<-data.frame(pattern,theta,SE,raw,converge,iterations)
class(res) <- c("ppWLdicho","data.frame")
}
if (tecout==TRUE){ # the tecnical output 
res<-object
class(res) <- c("list")
}
}
# ENDE einfacher Fall - kein bookletdesign

# komplexer Fall - bookletdesign
if (length(incidenz)!=0){
 all_incpat<-(do.call("paste",c(as.data.frame(incidenz), sep = ""))) # zeilen der incidenz als pattern
 uni_incpat<-unique(do.call("paste",c(as.data.frame(incidenz), sep = ""))) # unique davon
 
 ## aufteilen der daten nach booklets in liste mit dem entspr. sigmas ----
 daten_booklet_list<-list()
 for (i in 1:length(uni_incpat)){
   item_index<-which(as.numeric(unlist(strsplit(uni_incpat[i],"")))==1)
   person_index<-all_incpat %in% uni_incpat[i]
   sig_tmp<-SIGMA[item_index] # ergänzung (daten nur 1 Item)
   dat_tmp<-as.matrix(daten[person_index,item_index])# ergänzung (daten nur 1 Item)
   if(dim(dat_tmp)[2]==1){colnames(dat_tmp)<-names(sig_tmp) }# ergänzung (daten nur 1 Item)
   daten_booklet_list[[i]] <- list("sigma"=sig_tmp ,"daten"=dat_tmp)# änderung (daten nur 1 Item)
 }
 names(daten_booklet_list)<-uni_incpat
 
 object <- lapply(daten_booklet_list, function(x){apply(x$daten,1,thetaWL.dicho, sigma=x$sigma, na_treat=na_treat, limit=limit,iter=iter) }  )
  
 if (tecout==FALSE){ # the normal output
   resL<-list()   
   for (i in 1:length(object)){
     pattern<- apply(( as.matrix(sapply(object[[i]],function(x){(x$response)}))   ), 2, function(x){paste(x,collapse="")})
     theta<-sapply(object[[i]],function(x){x$theta})
     SE<-sapply(object[[i]],function(x){x$SE})
     raw<-sapply(object[[i]],function(x){sum(x$response,na.rm=TRUE)})
     converge<-sapply(object[[i]],function(x){x$converge})
     iterations<-sapply(object[[i]],function(x){x$iterations}) 
     resL[[i]]<-data.frame(pattern,theta,SE,raw,converge,iterations)
   }
   res <- do.call(rbind, resL)
   res<-res[caseORDER, ]
   class(res) <- c("ppWLdicho","data.frame")
 }
 if (tecout==TRUE){ # the tecnical output
   res<-object
   class(res) <- c("list")
 }
}
# ENDE komplexer Fall - bookletdesign

return(res)
summary(res,TRUE)

}