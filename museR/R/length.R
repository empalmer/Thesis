#==============================================================
#' length
#' 
#' 

length_measures <- function(piece){
  measure_col <- grep("measure", colnames(piece),value = T)
  measure <-  piece[,measure_col]
  max_m <- max(measure)
  max_m
}

