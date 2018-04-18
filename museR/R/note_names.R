#### n.v_n.n #### 
#' Identifies the note and note value of a .krn note
#'  
#' @param note .krn file 
#' @return NNV and DNV
#' 

n.v_n.n <- function(note){
  if(is.na(note)){
    v <- NA
    val <- NA
  } else if(stringr::str_detect(note, "[Aa]-")){
    v <- "A-"
    val <- 1
  } else if(stringr::str_detect(note, "[Aa](?!#|-)[kTp;n\\)_]*$")){
    v <- "A"
    val <- 2
  } else if(stringr::str_detect(note, "[Aa]#")){
    v <- "A#"
    val <- 3
  } else if(stringr::str_detect(note, "[Bb]-")){
    v <- "B-"
    val <- 3
  } else if (stringr::str_detect(note, "[Bb](?!#|-)[kTp;n\\)_]*$")){
    v <- "B"
    val <- 4
  }else if (stringr::str_detect(note, "[Bb]#")){
    v <- "B#"
    val <- 5
  } else if (stringr::str_detect(note, "[Cc]-")){
    v <- "C-"
    val <- 4
  }  else if (stringr::str_detect(note, "[Cc](?!#|-)[kTp;n\\)_]*$")){
    v <- "C"
    val <- 5
  } else if (stringr::str_detect(note, "[Cc]#")){
    v <- "C#"
    val <- 6
  } else if (stringr::str_detect(note, "[Dd]-")){
    v <- "D-"
    val <- 6
  } else if (stringr::str_detect(note, "[Dd](?!#|-)[kTp;n\\)_]*$")){
    v <- "D"
    val <- 7
  } else if (stringr::str_detect(note, "[Dd]#")){
    v <- "D#"
    val <- 8
  } else if (stringr::str_detect(note, "[Ee]-")){
    v <- "E-"
    val <- 8
  } else if (stringr::str_detect(note, "[Ee](?!#|-)[kTp;n\\)_]*$")){
    v <- "E"
    val <- 9
  } else if (stringr::str_detect(note, "[Ee]#")){
    v <- "E#"
    val <- 10
  } else if (stringr::str_detect(note, "[Ff]-")){
    v <- "F-"
    val <- 9
  } else if (stringr::str_detect(note, "[Ff](?!#|-)[kTp;n\\)_]*$")){
    v <- "F"
    val <- 10
  } else if (stringr::str_detect(note, "[Ff]#")){
    v <- "F#"
    val <- 11
  } else if (stringr::str_detect(note, "[Gg]-")){
    v <- "G-"
    val <- 11
  } else if (stringr::str_detect(note, "[Gg](?!#|-)[kTp;n\\)_]*$")){
    v <- "G"
    val <- 12
  } else if (stringr::str_detect(note, "[Gg]#")){
    v <- "G#"
    val <- 1
  } else if (stringr::str_detect(note, "r")){
    v <- "rest"
    val <- NA
  } else if (stringr::str_detect(note,"\\.")){
    v <- "."
    val <- "."
  }else {
    v <- note
    val <- NA
  }
  r <- c(v,val)
  return(r)
}
#==============================================================
#### note_value #### 
#' add_n.v_n.n
#' Add note value note name
#' Adds columns with more easily analyzable note names
#'  
#' @param notes one note line for one instrument
#' @return data frame with orriginal piece and NNV and DNV
#' 

add_n.v_n.n <- function(notez){
  df <- data.frame(colnames(c("n.n","n.v")))
  #v <- vector()
  for(i in 1:length(notez)){
    #v[i] <- n.v_n.n(notes[i])
    df[i,1] <- n.v_n.n(notez[i])[1]
    df[i,2] <- n.v_n.n(notez[i])[2] 
  }
  df
}  


#==============================================================
#' r.n
#' 
#' Gives the name of a given rhythm value
#'
#' @param note one note
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
}
  
  

  
  