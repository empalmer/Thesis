###############################################################
#' kern2df
#' Kern 2 data frame
#' Takes a .krn spline and converts into data frame
#'
#' @param spline .krn file
#' @return dataframe of stuff

kern2df <- function(spline){ # takes a spline(".krn") as input
  data <- readLines(spline)
  key_v <- grep("^\\*.*[^I]:$",data,value = T)
  if(identical(key_v,character(0))){
    key_v <- grep("\\*{1}k\\[",data,value = T)
  }
  key_v<- gsub("\t.*","",key_v)
  meter <- grep("\\*{1}M[0-9]",data,value = T)
  data <- data[-grep("^!|\\*", data)] #removes extra text and info
  measures <- grep("=",data, value = F) # measures in .krn start wtih = 1
  val_list_notes <- data[-measures]
  if(!grepl("=1-",data[1])){measures <- c(0,measures)}
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
  piece <- as.data.frame(lapply(piece, function(y) gsub("L|J|K", "", y)))
  piece <- as.data.frame(lapply(piece, function(y) gsub("'", "", y)))
  piece <- as.data.frame(lapply(piece, function(y) gsub("\\[|\\]|\\\\|\\/", "", y)))
  spline_df <- data.frame(rep(key_v,nrow(piece)),
                          rep(meter,nrow(piece)),
                          measure_column)
  for(i in 2:(max_notes+1)){
    #ri <- stringr::str_extract(piece[,i],"[0-9]{1,2}(\\.*)|\\.") # rhythem value
    ri <- stringr::str_extract(piece[,i],"[0-9]{1,2}(\\.*)")
    #rin <- add_r.n(ri) # rhythem name
    #notei <- stringr::str_extract(piece[,i],"[A-z]{1,2}.*|^\\.")
    notei <- stringr::str_extract(piece[,i],"[A-z]{1,2}.*")
    notei_nv <- add_n.v_n.n(notei)
    spline_df <- cbind(spline_df,ri,notei,notei_nv)
  }
  spline_df <- as.data.frame(lapply(spline_df,
                                    function(y) gsub("K", "", y)))
  spline_df
}

#==============================================================
#' piece_df
#' Takes splines in .krn and calls kern2df
#' to create one df for entire piece
#'
#' @param v vector with splines .krn file strings
#' @param insts vector of same lenght as v,
#' including instrument names for each spline
#' @return dataframe for entire piece - combined splines

piece_df <- function(v,insts){
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
    max_notes <- (ncol(Ai)-3)/4
    for(k in 1:max_notes){
      if(first){cols <- c("key","meter","measure","r.v","n.o","n.n","n.v")}
      if(!first){cols <- c("r.v","n.o","n.n","n.v")}
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



