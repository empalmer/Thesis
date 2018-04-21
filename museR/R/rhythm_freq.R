#==============================================================
#' rythem freq
#'
#' @param piece
#' @return Vector of frequencies for all rhythmic values
#'

rhythm_freq <- function(piece){
  r_cols <- measure_col <- grep("r\\.v", colnames(piece),value = T)
  r_df <-  piece[,r_cols]
  tot <- rep(0,10)
  names(tot) <- c("2","2.","4","4.","8","8.","16","16.","32")
  a <- list()
  for(i in 1:ncol(r_df)){
    a <- table(r_df[,i])
    b <- c(tot,a)
    tot <- tapply(b,names(b),sum)
  }
  freqs <- tot/sum(!is.na(r_df))
  freqs
}

#==============================================================
#' dot freq
#'
#' @param piece
#' @return Frequency of dotted rhythms in the piece
#'

dot_freq <- function(piece){
  f <- rhythm_freq(piece)
  s <- f["16."] + f["2."] + f["8."]
  s
}

#============================================================
#' topline rhythem freqs
#'
#' @param piece
#' @return Frequencies
#'

top_rhythm_freq <- function(piece){
  r_cols <- measure_col <- grep("r\\.v", colnames(piece),value = T)
  r_df <-  piece[,r_cols]
  tot <- rep(0,10)
  names(tot) <- c("2","2.","4","4.","8","8.","16","16.","32")
  a <- table(r_df[,1])
  freqs <- a/sum(!is.na(r_df))
  freqs
}

#==============================================================
#' top dot freq
#'
#' @param piece
#' @return Frequency of dotted rhythms in the piece
#'

top_dot_freq <- function(piece){
  f <- top_rhythm_freq(piece)
  s <- f["16."] + f["2."] + f["8."]
  s
}
