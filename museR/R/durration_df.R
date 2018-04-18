
#==============================================================
#' Duration
#' 
#' @param piece
#' @return piece that has duration included
#' 

durration_df <- function(piece){
  cols <- grep("n\\.n|n\\.o|r\\.v", colnames(piece),value = T)
  for(j in 1:length(cols)){
    for(i in 2:nrow(piece)){
      if(!is.na(piece[i,cols[j]])){
        if(piece[i,cols[j]] == "." ){
          piece[i,cols[j]] <- piece[i-1,cols[j]]
        }
      }
    }
  }
  piece
}



