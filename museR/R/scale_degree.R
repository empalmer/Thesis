#==============================================================
#' scale_degree_freq
#' @param piece
#' @return the frequency of occurance of all the scale degrees
#'
scale_degree_freq <- function(piece){
  note_cols <- grep("n\\.n", colnames(piece),value = T)
  note_df <-  piece[,note_cols]
  keyx <- Major_minor(piece)[1]
  scale_degrees <- scales[,keyx] %>% as.vector()
  degrees_count <- rep(0,length(scale_degrees))
  tot <- 0
  for(i in 1:ncol(note_df)){
    a <- table(note_df[,1]) # is this really a better useage?
    tot <- tot + sum(a, na.rm = T)
    scale_deg <- a[scale_degrees] %>% as.vector
    scale_deg[is.na(scale_deg)]<- 0
    degrees_count <- degrees_count + scale_deg
  }
  freq <- degrees_count/tot
  data.frame(scale_degrees,degrees_count,freq)
}

#==============================================================
#==============================================================
#' scale_degree_freq
#' @param piece
#' @return the scale degrees for each note
#'

assign_sd <- function(piece){
  key2 <- Major_minor(piece)[1]
  scalez <- c("G#","A-","A","A#","B-","B","B#","C-","C","C#",
             "D-","D","D#","E-","E","E#","F-","F","F#","G-","G")
  scalez <- c(scalez,scalez)
  start <- min(which(scalez == key2))
  scale_deg_s <- scalez[start:(start+20)]
  scdv <- c(0,0,1,2,2,3,4,3,4,5,5,6,7,7,8,9,8,9,10,10,11) + 1
  scdv <- c(scdv,scdv+scdv[start])
  scdv_key <- scdv[start:(start+20)]
  scdv_key <- scdv_key - (scdv_key[1]-1)

  scale_deg_values <- rbind(scale_deg_s,scdv_key)
  scale_deg_values <- cbind(scale_deg_values,c(".",NA),c("rest",NA))
  colnames(scale_deg_values) <- c(scale_deg_s,".","rest")

  note_name_cols <- grep("n\\.n", colnames(piece),value = T)
  note_value_cols <- grep("n\\.v", colnames(piece),value = T)
  for(j in 1:length(note_name_cols)){
    for(i in 1:nrow(piece)){
      if(is.na(piece[i,note_name_cols[j]])){
        sd <- NA
      }else{
        note <- piece[i,note_name_cols[j]] %>% unname() %>% as.character()
        sd <- scale_deg_values[2,note]
      }
      piece[i,note_value_cols[j]] <- sd
    }
  }
  piece
}

