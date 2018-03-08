library(bazar)
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
#' top_line
#' 
#' @param piece 
#' @param col one instrument name name 
#' @return gives the top melodic line of a instrument
#' 

top_line <- function(piece,inst){
  inst_cols <- grep(inst,colnames(piece),value = T)
  note_cols <- grep("n\\.v", inst_cols,value = T)
  n1 <- grep("1" ,note_cols,value = T)
  n2 <- grep("3" ,note_cols,value = T)
  n3 <- grep("5" ,note_cols,value = T)
  notes <- vector()
  for(i in 1:nrow(piece)){
    if(!is.empty(n3)){
      if(is.na(piece[i,n3])){ # is there a note in the fifth of the chord?
        if(is.na(piece[i,n2])){ # is there a note on 2nd?
          if(!is.na(piece[i,n1])){ # is the row empty?, if not
            notes[i] <- piece[i,n1]
          }
        } else notes[i] <- piece[i,n2]
      }else notes[i] <- piece[i,n3]
    }
    else if(!is.empty(n2)){
      if(is.na(piece[i,n2])){ # is there a note on 2nd?
        if(!is.na(piece[i,n1])){ # is the row empty?, if not
          notes[i] <- piece[i,n1]
        }
      } else notes[i] <- piece[i,n2]
    } else notes[i] <- piece[i,n1]
  }
  notes
}

#####################################################
#' voice_mel_ints
#' 
#' @param piece One instrument 
#' @param col name 
#' @return vector of counts for melodic intervals
#' 

mel_ints <- function(piece,col){
  mel <- top_line(piece,col)
  mel <- as.numeric(as.vector(na.omit(mel)))
  mel_dif <- c()
  for(i in 1:length(mel)-1){
    mel_dif[i] <- min(max(mel[i],mel[i+1]) - min(mel[i],mel[i+1]),
                      min(mel[i],mel[i+1]) + 12 - max(mel[i],mel[i+1]))
  }
  # Change indexing to start at 1
  mel_dif <- mel_dif + 1
  ints <- c("unison","m2", "M2","m3", "M3","p4","tt",
            "p5", "m6","M6","m7","M7")
  mel_fac <- factor(ints[mel_dif], levels = ints, ordered = T)
  m <- table(mel_fac)/sum(table(mel_fac))
  m
}