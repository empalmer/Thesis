library(stringr)
source("~/Desktop/Thesis/museR/R/names.R")

###############################################################
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

  measures <- grep("=",data, value = F) # measures in .krn start wtih = 1
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
                      r.v.1 = r1,
                      r.v.2 = r2,
                      r.v.3 = r3,
                      n.v.1 = note_base,
                      n.v.2 = note_third,
                      n.v.3 = note_fifth)
  spline_df
}

###############################################################
#' Takes several splines in .krn that are part of a piece 
#' and calls kern2df to create one df for entire piece
#'
#' @param a first spline .krn file
#' @param b second spline .krn file
#' @param c third spline .krn file
#' @return dataframe for entire piece - combined splines

piece_df <- function(a,b,c){
  A <- kern2df(a)
  B <- kern2df(b)
  C <- kern2df(c)
  measure_col <- A[,1]
  A.n <- add_n.v_n.n(A) # add note names and int values
  B.n <- add_n.v_n.n(B)
  C.n <- add_n.v_n.n(C)
  A.r <- add_r.n(A) # Add rhythm names
  B.r <- add_r.n(B)
  C.r <- add_r.n(C)
  # The following is for better organization of data frame
  Ap <- cbind(A[,2],A.r[,1],A[,5],A.n[,1:2], # 1st note
              A[,3],A.r[,2],A[,6],A.n[,3:4], # 2nd note
              A[,4],A.r[,3],A[,7],A.n[5:6])  # 2rd note
  Bp <- cbind(B[,2],B.r[,1],B[,5],B.n[,1:2], # 1st note
              B[,3],B.r[,2],B[,6],B.n[,3:4], # 2nd note
              B[,4],B.r[,3],B[,7],B.n[5:6])  # 2rd note
  Cp <- cbind(C[,2],C.r[,1],C[,5],C.n[,1:2], # 1st note
           C[,3],C.r[,2],C[,6],C.n[,3:4], # 2nd note
           C[,4],C.r[,3],C[,7],C.n[5:6])  # 2rd note
  # Create the data frame
  piece <- cbind(measure_col, Ap, Bp, Cp) 
  # Name the columns, voice, piano right hand, piano left hand
  colnames(piece) <- c("measure","V_r.v_1", "V_r.n_1", "V_n.o_1",
                        "V_n.n_1","V_n.v_1","V_r.v_2", "V_r.n_2", 
                       "V_n.o_2","V_n.n_2","V_n.v_2","V_r.v_3", 
                       "V_r.n_3", "V_n.o_3","V_n.n_3","V_n.v_3",
                       "pR_r.v_1", "pR_r.n_1", "pR_n.o_1",
                       "pR_n.n_1","pR_n.v_1","pR_r.v_2", "pR_r.n_2", 
                       "pR_n.o_2","pR_n.n_2","pR_n.v_2","pR_r.v_3", 
                       "pR_r.n_3", "pR_n.o_3","pR_n.n_3","pR_n.v_3",
                       "pL_r.v_1", "pL_r.n_1", "pL_n.o_1",
                       "pL_n.n_1","pL_n.v_1","pL_r.v_2", "pL_r.n_2", 
                       "pL_n.o_2","pL_n.n_2","pL_n.v_2","pL_r.v_3", 
                       "pL_r.n_3", "pL_n.o_3","pL_n.n_3","pL_n.v_3"
                       ) 
  # Get rid of columns with all NA's
  j <- 0
  for(i in 1:ncol(piece)){   
    j[i] <- all(is.na(piece[,i]))
  }
  piece <- piece[,-which(as.logical(j))]
  # Return piece
  piece
}

###############################################################
#' Takes a piece_df from piece_df()
#'
#' @param piece_df
#' @return list of df of notes, key, time base etc.
#'
#'

all_info_piece <- function(piece_df){ # returns list that includes key signature, etc
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


###############################################################
#' Takes result of all_info_piece to change 
#' rhythms to name values (ie changes 4 to quarter note)
#'
#' @param piece_data_frame
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


###############################################################
convert_folder <- function(list_of_pieces){ # given a folder with music, listed as spline a,b,c, return list with converted into list for each piece

}











