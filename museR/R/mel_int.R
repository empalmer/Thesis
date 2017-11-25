#' Takes a spline and converts creates data frame with numeric note value (NNV) and descriptive notevalue (DNV) 
#' 
#' @param spline .krn file 
#' @return dataframe of stuff
#' 
#' @examples 
#' 
#'

remove_rest_nas <- function(piece){
  
}

#' Takes a spline and converts creates data frame with numeric note value (NNV) and descriptive notevalue (DNV) 
#' 
#' @param spline .krn file 
#' @return dataframe of stuff
#' 
#' @examples 
#' 
#'

### NOTE ONLY CONSIDERING DIM AND MINOR - NO AGUMENTED #####
tot_mel_int <- function(spline){
  sum(is.na(spline))
}
is_mel_int <- function(n1,nn2, int){ 
  ifelse(n1$V-n2$V == int,T,F)