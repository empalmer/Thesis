#==============================================================
#' rhythm entropy
#'
#' @param piece
#' @return Time signature of the piece
#'
rhy_entropy <- function(piece){
  r_cols <- measure_col <- grep("r\\.v", colnames(piece),value = T)
  r_df <-  piece[,r_cols]
  changes <- 0
  for(j in 1:ncol(r_df)){
    rv <- as.numeric(as.vector(na.omit(r_df[,j])))
    changes_j <- 0
    for(i in 2:length(rv)){
      c <- rv[i]-rv[i-1]
      changes_j[i-1] <-ifelse(c ==0,0,1)
    }
    changes[j] <- mean(changes_j)
  }
  changes <- mean(changes,na.rm = T)
  changes
}




