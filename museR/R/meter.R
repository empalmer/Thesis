#==============================================================
#' Meter
#'
#' @param piece
#' @return Time signature of the piece
#'

meter <- function(piece){
  m <- piece[1,2]
  met <- regmatches(m, regexpr("M.{,2}",m))
  met <- sub(met, "", "M")
  met
}



