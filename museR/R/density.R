library(dplyr)
library(purrr)
library(purrrlyr)
###############################################################
#' measure_density
#' 
#' @param piece in raw data frame
#' @return density for each measure
#'

beat_density <- function(piece){
  note_cols <- grep("measure|n\\.v", colnames(piece) ,value = T)
  note_df <- piece[,note_cols]
  onote_df <- note_df[,-1]
  d <- vector()
  for(i in 1:nrow(onote_df)){
    d[i] <- sum(!is.na(onote_df[i,]))
  }
  d <- d[which(d != 0)]
  c(mean(d),sd(d))
}

map(felix_df,beat_density)
map(fanny_pieces,beat_density)


