#' Takes a spline and converts creates data frame with numeric note value (NNV) and descriptive notevalue (DNV) 
#' 
#' @param spline .krn file 
#' @return dataframe of stuff
#' 
#' @examples 
#' 
#'


note_value <- data.frame(colnames(c("base","third","fifth","val_base","val_third","val_fifth")))
for(i in 1:nrow(piece)){
  note_value[i,1] <- notes_to_val(piece[i,3])[1] 
  note_value[i,2] <- notes_to_val(piece[i,5])[1]
  note_value[i,3] <- notes_to_val(piece[i,7])[1]
  note_value[i,4] <- notes_to_val(piece[i,3])[2] 
  note_value[i,5] <- notes_to_val(piece[i,5])[2]
  note_value[i,6] <- notes_to_val(piece[i,7])[2]
}


notes_to_val <- function(note){
  if (is.na(note)){
    v <- NA
    val <- NA
  } else if(str_detect(note, "[Aa]-")){
    v <- "A flat"
    val <- 1
  } else if(str_detect(note, "[Aa]$")){
    v <- "A"
    val <- 2
  } else if (str_detect(note, "[Aa]#")){
    v <- "A sharp"
    val <- 3
  } else if (str_detect(note, "[Bb]-")){
    v <- "B flat"
    val <- 3
  } else if (str_detect(note, "[Bb]$")){
    v <- "B"
    val <- 4
  }else if (str_detect(note, "[Bb]#")){
    v <- "B sharp"
    val <- 5
  } else if (str_detect(note, "[Cc]-")){
    v <- "C flat"
    val <- 4
  }  else if (str_detect(note, "[Cc]$")){
    v <- "C"
    val <- 5
  } else if (str_detect(note, "[Cc]#")){
    v <- "C sharp"
    val <- 6
  } else if (str_detect(note, "[Dd]-")){
    v <- "D flat"
    val <- 6
  } else if (str_detect(note, "[Dd]$")){
    v <- "D"
    val <- 7
  } else if (str_detect(note, "[Dd]#")){
    v <- "D sharp"
    val <- 8
  } else if (str_detect(note, "[Ee]-")){
    v <- "E flat"
    val <- 8
  } else if (str_detect(note, "[Ee]$")){
    v <- "E"
    val <- 9
  } else if (str_detect(note, "[Ee]#")){
    v <- "E sharp"
    val <- 10
  } else if (str_detect(note, "[Ff]-")){
    v <- "F flat"
    val <- 9
  } else if (str_detect(note, "[Ff]$")){
    v <- "F"
    val <- 10
  } else if (str_detect(note, "[Ff]#")){
    v <- "F sharp"
    val <- 11
  } else if (str_detect(note, "[Gg]-")){
    v <- "G flat"
    val <- 11
  } else if (str_detect(note, "[Gg]$")){
    v <- "G"
    val <- 12
  } else if (str_detect(note, "[Gg]#")){
    v <- "G sharp"
    val <- 1
  } else if (str_detect(note, "r")){
    v <- "rest"
    val <- NA
  }else {
    v <- note
    val <- NA
  }
  r <- c(v,val)
  return(r)
}
