#==============================================================
#' beat_density
#' 
#' @param piece in raw data frame
#' @return density for each measure
#'

beat_density <- function(piece){
  note_cols <- grep("measure|n\\.n", colnames(piece) ,value = T)
  note_df <- piece[,note_cols]
  onote_df <- note_df[,-1]
  d <- vector()
  for(i in 1:nrow(onote_df)){
    d[i] <- sum(!is.na(onote_df[i,]))
  }
  d <- d[which(d != 0)]
  c(mean(d),sd(d))
}

#==============================================================
#' note_duration
#' 
#' @param piece in raw data frame
#' @return density for each measure
#'

note_duration <- function(piece,inst){
  rhy_cols <- grep("measure|r\\.v_1", colnames(piece) ,value = T)
  rhy_df <- piece[,note_cols]
  onote_df <- note_df[,-1]
  rhy_no_na <- as.numeric(as.vector(na.omit()))
}






