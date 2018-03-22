
#==============================================================
#' scale_degree_freq
#' @param piece 
#' @return the frequency of occurance of all the scale degrees
#' 
scale_degree_freq <- function(piece){
  note_cols <- grep("n\\.n", colnames(piece),value = T)
  note_df <-  piece[,note_cols]
  key <- Major_minor(piece)[1]
  scale_degrees <- scales[,key] %>% as.vector()
  degrees_count <- rep(0,length(scale_degrees))
  tot <- 0
  for(i in 1:ncol(note_df)){
    a <- table(note_df[1])
    tot <- tot + sum(a, na.rm = T)
    scale_deg <- a[scale_degrees] %>% as.vector
    scale_deg[is.na(scale_deg)]<- 0
    degrees_count <- degrees_count + scale_deg
  }
  freq <- degrees_count/tot
  data.frame(scale_degrees,degrees_count,freq)
}

#==============================================================

