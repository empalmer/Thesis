
library(stringr)
### kern2df ####

#' Takes a .krn spline and converts into data frame
#'
#' @param spline .krn file
#' @return dataframe of stuff
#'
#'

kern2df <- function(spline){ # takes a spline(".krn") as input
  data <- readLines(spline)
  data <- data[-grep("!!linebreak", data)] #removes all linebreak text
  data <- data[-grep("\\*", data)] # removes ... 

  measures <- grep( "="   ,data, value = F) # measures in .krn start wtih = 1
  measure_numbers <- 1:length(measures) # how many measures there are
  list_notes <- grep("^\\(*[0-9]|\\[|\\.", data) # makes a list of notes and . s
  val_list_notes <- grep("^\\(*[0-9]|\\[|\\.", data, value = T) # match ( maybe once,  [0-9 maybe once])
  measure_column <- vector() # make a vector of which row each is in.
  for( i in 2:length(measures)){
    len <- as.numeric(measures[i]-measures[(i-1)])-1
    val <- measure_numbers[i-1]
    measure_column <- c(measure_column, rep(val, len))
  }
  sep_chord <- list() #creates a list that seperates 4E 8n into two
  for(i in 1:length(list_notes)){ # if not, seperate the notes
    sep_chord[[i]] <- unlist(strsplit(val_list_notes[i],"\\/|\\\\"))
  }
  n <- data.frame() # seperates chords from list to data frame
  for( j in 1:3){
    for( i in 1:length(list_notes)){
      n[i,j]<-sep_chord[[i]][j]
    }
  }

  piece <- data.frame(measure = measure_column, n1 = n[,1], n2 = n[,2], n3 = n[,3])
  piece <- as.data.frame(lapply(piece, function(y) gsub("L|J", "", y)))
  piece <- as.data.frame(lapply(piece, function(y) gsub("\\[|\\]|\\\\|\\/", "", y)))

  r1 <- str_extract(piece[,2],"[0-9]{1,2}(\\.*)")
  note_base <- str_extract(piece[,2],"[A-z]{1,2}.*")
  r2 <- str_extract(piece[,3],"[0-9]{1,2}(\\.*)")
  note_third <- str_extract(piece[,3],"[A-z]{1,2}.*")
  r3 <- str_extract(piece[,4],"[0-9]{1,2}(\\.*)")
  note_fifth <- str_extract(piece[,4],"[A-z]{1,2}.*")

  spline_df <- data.frame(measure = measure_column,
                      rhythm_val_base = r1,
                      note_val_base = note_base,
                      rhy_val_third = r2,
                      note_val_third = note_third,
                      rhy_val_fifth = r3,
                      note_val_fifth = note_fifth)
  spline_df
}


#######  piece_df #############
#' Takes several splines in .krn that are part of a piece 
#' and calls kern2df to create one df for entire piece
#'
#' @param a first spline .krn file
#' @param b second spline .krn file
#' @param c third spline .krn file
#' @return dataframe for entire piece - combined splines

piece_df <- function(a,b,c){
# if want splines listed vertically
#  len <- nrow(df_a)
#  len_b <- nrow(df_b)
#  len_c <- nrow(df_c)
#  spline_number <- c(rep(1,len),rep(2,len),rep(3,len)) 
#indicates which spline everything belongs to
#  piece <- rbind(df_a,df_b,df_c)
#  piece <- cbind(spline_number, piece)
#  piece
  A <- kern2df(a)
  B <- kern2df(b)
  C <- kern2df(c)
  
  measure_col <- A[,1]
  A <- A[,-1] # dont need repeats of the measure number. Should be consistant. 
  B <- B[,-1]
  C <- C[,-1]
    
  piece <- cbind(measure_col,A,B,C) #combine splines vertically
  
  colnames(piece) <- c("measure","voice_r_val_1", "voice_n_val_1","voice_r_val_3",
                       "voice_n_val_3","voice_r_val_5","voice_n_val_5","pR_r_val_1", 
                       "pR_n_val_1","pR_r_val_3","pR_n_val_3",
                       "pR_r_val_5","pR_n_val_5","pL_r_val_1", 
                       "pL_n_val_1","pL_r_val_3",
                       "pL_n_val_3","pL_r_val_5",
                       "pL_n_val_5") # rename columns (voice, piano right hand, piano left hand)
  tf <- 0
  for(i in 1:ncol(piece)){   # Get rid of collumns with all NA's
    tf[i] <- all(is.na(piece[,i]))
  }
  piece <- piece[,-which(as.logical(tf))]
  piece
}

###all_info_piece

#' Takes a piece in three seperate .krn files and combines into
#'
#' @param a .krn file spline 1
#' @param b .krn file spline 2
#' @param c .krn file spline 3
#' @return list of df of notes, key, time base etc.
#'
#'

all_info_piece <- function(a,b,c){ # returns list that includes key signature, etc
  notes <- piece_df(a,b,c)
  a <- readLines(a)
  b <- readLines(b)
  c <- readLines(c)
  staff_a <- grep("staff[0-9]", a, value = T) # which staff for first spline?
  staff_b <- grep("staff[0-9]", b, value = T) # which staff are we talking about?
  staff_c <- grep("staff[0-9]", c, value = T) # which staff are we talking about?
  key <- grep( "\\[[A-z]"  ,a, value = T)     # what key is piece in?(same for all splines?)
  meter <- grep( "M"  ,a, value = T)     # what meter is piece in?
  time_base <- grep( "tb"  ,a, value = T)     # what time_base?(rhythem is dependent)

  l <- list(notes, staff_a, staff_b, staff_c, key, meter, time_base) # returns list
  l
}



### tb_2_rhythm_value##

#' Takes result of all_info_piece to change rhythms to name values (ie changes 4 to quarter note)
#'
#' @param list_of_pieces
#' @return same list, but with added columns with rhythm names
#'
#'
tb_2_rhythm_value <- function(piece_list){
  tb_string <- piece_list[[7]]
  tb <- as.numeric(str_extract(tb_string,"[0-9]+"))
  p_df <- piece_list[[1]]
  r_val_name_base <- p_df$rhythm_val_base/tb
  r_val_name_third <- p_df$rhythm_val_third/tb
  r_val_name_fifth <- p_df$rhythm_val_third/tb

}


convert_folder <- function(list_of_pieces){ # given a folder with music, listed as spline a,b,c, return list with converted into list for each piece

}








