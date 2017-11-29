

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
  staff <- grep("staff[0-9]", data, value = T) # which staff are we talking about?
  key <- grep( "\\[[A-z]"  ,data, value = T) # what is the key sig of the piece?
  data <- data[-grep("\\*", data)]
  measures <- grep( "="  ,data, value = F)
  measure_numbers <- 1:length(measures) # how many measures there are
  
  
  list_notes <- grep("^[0-9]|\\[|\\.", data) # makes a list of notes and . 's 
  
  val_list_notes <- grep("^[0-9]|\\[|\\.", data, value = T)
  
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
  
  
  n <- data.frame()
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



#' Takes several splines in .krn that are part of a piece and calls kern2df to create one df for entire piece
#' 
#' @param a first spline .krn file 
#' @param b second spline .krn file
#' @param c third spline .krn file
#' @return dataframe for entire piece - combined splines

piece_df <- function(a,b,c){
  df_a <- kern2df(a)
  df_b <- kern2df(b)
  df_c <- kern2df(c)
  len <- nrow(df_a)
  len_b <- nrow(df_b)
  len_c <- nrow(df_c)
#  if(len != len_b & len != len_c){ # make sure everything matches up
 
  spline_number <- c(rep(1,len),rep(2,len),rep(3,len)) #indicates which spline everything belongs to
  
  piece <- rbind(df_a,df_b,df_c)
  
  piece <- cbind(spline_number, piece)
  piece

}  
  
  
  
  
  
  
  






