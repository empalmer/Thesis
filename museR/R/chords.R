
# for piano left hand
select_chords <- function(piece,inst){
  inst_cols <- grep(inst,colnames(piece),value = T)
  note_cols <- grep("n\\.v", inst_cols,value = T)
  n1 <- grep("1" ,note_cols,value = T)
  n2 <- grep("3" ,note_cols,value = T)
  n3 <- grep("5" ,note_cols,value = T)
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


identify_chord <- function(chord){
  
}



