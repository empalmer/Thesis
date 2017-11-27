#' 
#' @param spline .krn file 
#' @return dataframe of stuff
#' 
#' @examples 
#' 
#'

remove_rest_nas <- function(piece){
  
}

#' How many melodic intervals each spline has
#' 
#' @param spline .krn file - grouped by spline number
#' @return how many melodic intervals each spline has
#' 
#' @examples 
#' 
#'

### NOTE ONLY CONSIDERING DIM AND MINOR - NO AGUMENTED #####
tot_mel_int <- function(spline){
  sum(is.na(spline))
}

#' Checks if two notes is a specified melodic interval
#' 
#' @param n1 one note
#' @param n2 second note
#' @param int what interval we are chekcing for
#' @return T or F 
#' 
#' @examples 
#' 
#'

is_mel_int <- function(n1,n2, int){ 
  ifelse(n1$V-n2$V == int,T,F)
}  
  
  
  
  











  
  
  
  