

###############################################################
#' Takes a .krn spline and converts into data frame
#'
#' @param spline .krn file
#' @return dataframe of stuff

kern2df <- function(spline){ # takes a spline(".krn") as input
  data <- readLines(spline)
  key <- grep(":$",data,value = T)
  if(identical(key,character(0))){
    key <- grep("\\*{1}k\\[",data,value = T)
  }
  meter <- grep("\\*{1}M[0-9]",data,value = T)
  data <- data[-grep("^!|\\*", data)] #removes extra text and info
  measures <- grep("=",data, value = F) # measures in .krn start wtih = 1
  val_list_notes <- data[-measures]
  if(data[1] != "=1-"){measures <- c(0,measures)}
  measure_numbers <- 0:(length(measures)) # how many measures there are
  measure_column <- vector() # make a vector of which row each is in.
  for(i in 2:length(measures)){
    len <- as.numeric(measures[i]-measures[(i-1)])-1
    val <- measure_numbers[i]
    measure_column <- c(measure_column, rep(val, len))
  }
  sep_chord <- list() #creates a list that seperates 4E 8n into two
  for(i in 1:length(val_list_notes)){
    sep_chord[[i]] <- unlist(strsplit(val_list_notes[i],"\\s"))
  }
  max_notes <- max(lengths(sep_chord))
  n <- data.frame()
  for( j in 1:max_notes){
    for( i in 1:length(val_list_notes)){
      n[i,j]<-sep_chord[[i]][j]
    }
  }
  piece <- cbind(measure_column, n)
  piece <- as.data.frame(lapply(piece, function(y) gsub("L|J", "", y)))
  piece <- as.data.frame(lapply(piece, function(y) gsub("\\[|\\]|\\\\|\\/", "", y)))

  spline_df <- data.frame(rep(key,nrow(piece)),
                          rep(meter,nrow(piece)),
                          measure_column)
  for(i in 2:(max_notes+1)){
    ri <- str_extract(piece[,i],"[0-9]{1,2}(\\.*)") # rhythem value
    rin <- add_r.n(ri) # rhythem name
    notei <- str_extract(piece[,i],"[A-z]{1,2}.*")
    notei_nv <- add_n.v_n.n(notei)
    spline_df <- cbind(spline_df,ri,rin,notei,notei_nv)
  }
  spline_df
} 

#==============================================================
#' Takes splines in .krn and calls kern2df to create one df for entire piece
#'
#' @param v vector with splines .krn file strings
#' @param insts vector of same lenght as v, including instrument names
#' for each spline
#' @return dataframe for entire piece - combined splines
 
piece_df2 <- function(v,insts){
  len <- length(v)
  c <- vector()
  first <- T
  for(i in 1:len){
    Ai <- kern2df(v[i])
    if(first){
      piece <- Ai
    }
    if(!first){
      piece <- cbind(piece,Ai[,-(1:3)])
    }
    max_notes <- (ncol(Ai)-3)/5
    for(k in 1:max_notes){
      if(first){cols <- c("key","meter","measure","r.v","r.n","n.o","n.n","n.v")}
      if(!first){cols <- c("r.v","r.n","n.o","n.n","n.v")}
      c_i <- vector()
      for(j in 1:length(cols)){
        c_i[j] <- paste(insts[i],"_",cols[j],k)
      }
      c <- c(c,c_i)
      first <- F
    }
  }
  colnames(piece) <- c
  piece  
}

#==============================================================

# piece_df <- function(v){
#   A <- kern2df(v[1])
#   B <- kern2df(v[2])
#   C <- kern2df(v[3])
#   measure_col <- A[,1]
#   A.n <- add_n.v_n.n(A) # add note names and int values
#   B.n <- add_n.v_n.n(B)
#   C.n <- add_n.v_n.n(C)
#   A.r <- add_r.n(A) # Add rhythm names
#   B.r <- add_r.n(B)
#   C.r <- add_r.n(C)
#   # The following is for better organization of data frame
#   Ap <- cbind(A[,2],A.r[,1],A[,5],A.n[,1:2], # 1st note
#               A[,3],A.r[,2],A[,6],A.n[,3:4], # 2nd note
#               A[,4],A.r[,3],A[,7],A.n[5:6])  # 2rd note
#   Bp <- cbind(B[,2],B.r[,1],B[,5],B.n[,1:2], # 1st note
#               B[,3],B.r[,2],B[,6],B.n[,3:4], # 2nd note
#               B[,4],B.r[,3],B[,7],B.n[5:6])  # 2rd note
#   Cp <- cbind(C[,2],C.r[,1],C[,5],C.n[,1:2], # 1st note
#            C[,3],C.r[,2],C[,6],C.n[,3:4], # 2nd note
#            C[,4],C.r[,3],C[,7],C.n[5:6])  # 2rd note
#   # Create the data frame
#   piece <- cbind(measure_col, Ap, Bp, Cp) 
#   # Name the columns, voice, piano right hand, piano left hand
#   colnames(piece) <- c("measure","V_r.v_1", "V_r.n_1", "V_n.o_1",
#                         "V_n.n_1","V_n.v_1","V_r.v_2", "V_r.n_2", 
#                        "V_n.o_2","V_n.n_2","V_n.v_2","V_r.v_3", 
#                        "V_r.n_3", "V_n.o_3","V_n.n_3","V_n.v_3",
#                        "pR_r.v_1", "pR_r.n_1", "pR_n.o_1",
#                        "pR_n.n_1","pR_n.v_1","pR_r.v_2", "pR_r.n_2", 
#                        "pR_n.o_2","pR_n.n_2","pR_n.v_2","pR_r.v_3", 
#                        "pR_r.n_3", "pR_n.o_3","pR_n.n_3","pR_n.v_3",
#                        "pL_r.v_1", "pL_r.n_1", "pL_n.o_1",
#                        "pL_n.n_1","pL_n.v_1","pL_r.v_2", "pL_r.n_2", 
#                        "pL_n.o_2","pL_n.n_2","pL_n.v_2","pL_r.v_3", 
#                        "pL_r.n_3", "pL_n.o_3","pL_n.n_3","pL_n.v_3"
#                        ) 
#   # Get rid of columns with all NA's
#   j <- 0
#   for(i in 1:ncol(piece)){   
#     j[i] <- all(is.na(piece[,i]))
#   }
#   piece <- piece[,-which(as.logical(j))]
#   # Return piece
#   piece
# }

#==============================================================
#' Takes a piece_df from piece_df()
#'
#' @param piece_df
#' @return list of df of notes, key, time base etc.
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
#==============================================================

