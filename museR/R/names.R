

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

#### note_value #### 
#' Adds columns with more easily analyzable note names
#'  
#' @param spline .krn file 
#' @return data frame with orriginal piece and NNV and DNV
#' 

# should be run on indifidual instruments... 
add_n.v_n.n <- function(spline){
  df <- data.frame(colnames(c("n.n_1","n.n_2","n.n_3","n.v_1","n.v_2","n.v_3")))
  for(i in 1:nrow(spline)){
    df[i,1] <- n.v_n.n(spline[i,3])[1] 
    df[i,2] <- n.v_n.n(spline[i,5])[1]
    df[i,3] <- n.v_n.n(spline[i,7])[1]
    df[i,4] <- n.v_n.n(spline[i,3])[2] 
    df[i,5] <- n.v_n.n(spline[i,5])[2]
    df[i,6] <- n.v_n.n(spline[i,7])[2]
  }
  df
}  



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

#' Takes result of kern_2_df and changes
#' rhythms to name values (ie changes 4 to quarter note)
#'
#' @param piece_data_frame
#' @return columns with rhythm names
#'
#'
add_r.n <- function(spline){
  df <- data.frame(colnames(c("r1","r2","r3")))
  for(i in 1:nrow(spline)){
    df[i,1] <- r.n(spline[i,2])
    df[i,2] <- r.n(spline[i,4])
    df[i,3] <- r.n(spline[i,6])
  }
  df
}
  
  
  
  