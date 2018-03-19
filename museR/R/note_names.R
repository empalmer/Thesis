#### n.v_n.n #### 
#' Identifies the note and note value of a .krn note
#'  
#' @param note .krn file 
#' @return NNV and DNV
#' 

n.v_n.n <- function(note){
  if (is.na(note)){
    v <- NA
    val <- NA
  } else if(str_detect(note, "[Aa]-")){
    v <- "Ab"
    val <- 1
  } else if(str_detect(note, "[Aa]$")){
    v <- "A"
    val <- 2
  } else if (str_detect(note, "[Aa]#")){
    v <- "A#"
    val <- 3
  } else if (str_detect(note, "[Bb]-")){
    v <- "Bb"
    val <- 3
  } else if (str_detect(note, "[Bb]$")){
    v <- "B"
    val <- 4
  }else if (str_detect(note, "[Bb]#")){
    v <- "B#"
    val <- 5
  } else if (str_detect(note, "[Cc]-")){
    v <- "Cb"
    val <- 4
  }  else if (str_detect(note, "[Cc]$")){
    v <- "C"
    val <- 5
  } else if (str_detect(note, "[Cc]#")){
    v <- "C#"
    val <- 6
  } else if (str_detect(note, "[Dd]-")){
    v <- "Db"
    val <- 6
  } else if (str_detect(note, "[Dd]$")){
    v <- "D"
    val <- 7
  } else if (str_detect(note, "[Dd]#")){
    v <- "D#"
    val <- 8
  } else if (str_detect(note, "[Ee]-")){
    v <- "Eb"
    val <- 8
  } else if (str_detect(note, "[Ee]$")){
    v <- "E"
    val <- 9
  } else if (str_detect(note, "[Ee]#")){
    v <- "E#"
    val <- 10
  } else if (str_detect(note, "[Ff]-")){
    v <- "Fb"
    val <- 9
  } else if (str_detect(note, "[Ff]$")){
    v <- "F"
    val <- 10
  } else if (str_detect(note, "[Ff]#")){
    v <- "F#"
    val <- 11
  } else if (str_detect(note, "[Gg]-")){
    v <- "Gb"
    val <- 11
  } else if (str_detect(note, "[Gg]$")){
    v <- "G"
    val <- 12
  } else if (str_detect(note, "[Gg]#")){
    v <- "G#"
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
#==============================================================
#### note_value #### 
#' Adds columns with more easily analyzable note names
#'  
#' @param notes one note line for one instrument
#' @return data frame with orriginal piece and NNV and DNV
#' 

add_n.v_n.n <- function(notes){
  df <- data.frame(colnames(c("n.n","n.v")))
  for(i in 1:length(notes)){
    df[i,1] <- n.v_n.n(notes[i])[1] 
    df[i,2] <- n.v_n.n(notes[i])[2] 
  }
  df
  #df <- data.frame(colnames(c("n.n_1","n.v_1","n.n_2","n.v_2","n.n_3","n.v_3")))
  #for(i in 1:nrow(spline)){
  #  df[i,1] <- n.v_n.n(spline[i,5])[1] 
  #  df[i,2] <- n.v_n.n(spline[i,5])[2] 
  #  df[i,3] <- n.v_n.n(spline[i,6])[1]
  #  df[i,4] <- n.v_n.n(spline[i,6])[2]
  #  df[i,5] <- n.v_n.n(spline[i,7])[1]
  #  df[i,6] <- n.v_n.n(spline[i,7])[2]
  #}
  #df
}  


#==============================================================
#' Gives the name of a given rhythm value
#'
#' @param note
#' @return rhythm name
#'
#'
r.n <- function(note){
  if(is.na(note)){
    n <- NA
  }else if(note == 4){
    n <- "Quarter note" 
  }else{
    n <- "Tbd"
  }
  return(n)
}
#==============================================================
#' Takes result of kern_2_df and changes
#' rhythms to name values (ie changes 4 to quarter note)
#'
#' @param ri individual note vector
#' @return columns with rhythm names
#'
#'
add_r.n <- function(ri){
  v <- vector()
  for(i in 1:length(ri)){
   v[i] <- r.n(ri[i]) 
  }
  v
  #df <- data.frame(colnames(c("r.n.1","r.n.2","r.n.3")))
  #for(i in 1:nrow(spline)){
  #  df[i,1] <- r.n(spline[i,2])
  #  df[i,2] <- r.n(spline[i,3])
  #  df[i,3] <- r.n(spline[i,4])
  #}
  #df
}
  
  

  
  