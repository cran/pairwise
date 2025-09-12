#' @title S3 Summary for Person Separation Reliability
#' @exportS3Method summary pairwiseSepRel
#' @keywords methods
#' @method summary pairwiseSepRel
#' @description S3 summary method for object of class\code{"pairwiseSepRel"}
#' @param object object of class\code{"pairwiseSepRel"}
#' @param ... other parameters passed trough
########################### hier die summary method ############################
summary.pairwiseSepRel <- function(object, ...){
  txt1 <- format(c(
    "Separation Reliability: ",
    "Observed Variance: ",
    "Mean Square Measurement Error: "
  ), justify = "right")
  
  txt2 <- c(
    " (Squared Standard Deviation)",
    " (Model Error Variance)"
  )
  
  if(interactive()) writeLines("")
  writeLines(paste0(txt1[1L], round(object$sep.rel, 4L)))
  writeLines("")
  writeLines(paste0(txt1[2L], round(object$SSD.PS, 4L), txt2[1L]))
  writeLines(paste0(txt1[3L], round(object$MSE, 4L), txt2[2L]))
  if(interactive()) writeLines("")
}
