
###############################################################
#' measure_density
#' 
#' @param piece in raw data frame
#' @return density for each measure
#'

measure_density <- function(piece){
  piece %>%
    group_by(measure)
}