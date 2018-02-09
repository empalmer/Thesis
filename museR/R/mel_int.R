
###############################################################
#' remove_rest_nas
#' 
#' @param spline .krn file 
#' @return dataframe of stuff
#' 

remove_rest_nas <- function(piece){
}

###############################################################
#' tot_mel_int
#' How many melodic intervals each spline has
#' 
#' @param spline .krn file - grouped by spline number
#' @return how many melodic intervals each spline has
#' 
#' @examples 
#' 
#'
# NOTE ONLY CONSIDERING DIM AND MINOR - NO AGUMENTED
tot_mel_int <- function(spline){
  sum(is.na(spline))
}

###############################################################
#' is_mel_int
#' Checks if two notes is a specified melodic interval
#' 
#' @param n1 one note
#' @param n2 second note
#' @param int what interval we are chekcing for
#' @return T or F 
#' 

is_mel_int <- function(n1,n2, int){ 
  ifelse(n1$V-n2$V == int,T,F)
}  
  
#############################################################  

#####################################################
#' voice_mel_ints
#' 
#' @param piece One instrument 
#' @param col name 
#' @return vector of counts for melodic intervals
#' 

voice_mel_ints <- function(piece,col){
  mel <- piece[,col]
  mel <- as.numeric(as.vector(na.omit(mel)))
  mel_dif <- c()
  for(i in 1:length(mel)-2){
    mel_dif[i] <- abs(mel[i]-mel[i+1])
  }
  m <- rep(0,12)
  names(m) <- c("unison","m2", "M2","m3", "M3","p4","tt",
                "p5", "m6","M6","m7","M7")
  for(i in 1:length(mel_dif)){
    int <- mel_dif[i]
    m[int] <- m[int] + 1
  }
  m <- m/sum(m)
}
 
 












  
  
  
  


top_line <- function(piece,inst){
  inst_cols <- grep(inst,colnames(piece),value = T)
  note_cols <- grep("n\\.v", inst_cols,value = T)
  n1 <- grep("1" ,note_cols,value = T)
  n2 <- grep("3" ,note_cols,value = T)
  n3 <- grep("5" ,note_cols,value = T)
  notes <- c()
  for(i in 1:nrow(piece)){
    if(is.na(piece[i,n3])){ # is there a note in the fifth of the chord?
      if(is.na(piece[i,n2])){ # is there a note on 2nd?
        if(!is.na(piece[i,n1])){ # is the row empty?, if not
          notes[i] <- piece[i,n1]
        }
      } else notes[i] <- piece[i,n2]
    }else notes[i] <- piece[i,n3]
  }
  notes
}

