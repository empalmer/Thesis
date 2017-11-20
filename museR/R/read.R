#' Takes a .krn file and converts into a readable format
#' 
#' @param spline .krn file 
#' @return dataframe of stuff

kern2data <- function(spline){ # takes a spline(".krn") as input
  data <- readLines(spline)
  data <- data[-grep("!!linebreak", data)] #removes all linebreak text
  staff <- grep("staff[0-9]", data, value = T) # which staff are we talking about?
  key <- grep( "\\[[A-z]"  ,data, value = T) # what is the key sig of the piece?
  data <- data[-grep("\\*", data)]
  measures <- grep( "="  ,data, value = F)
  measure_numbers <- 1:length(measures) # how many measures there are
  
  
  list_notes <- grep("^[0-9]|\\[", data)
  
  val_list_notes <- grep("^[0-9]|\\[", data, value = T)
  
  measure_column <- vector() # make a vector of which row each is in. 
  for( i in 2:length(measures)){
    len <- as.numeric(measures[i]-measures[(i-1)])-1
    val <- measure_numbers[i-1]
    measure_column <- c(measure_column, rep(val, len))
  }
  
  
  notes_list <- list()
  for(i in 1:length(list_notes)){
    notes_list[[i]] <- unlist(strsplit(val_list_notes[i],"/") )
  }
  
  n <- data.frame()
  for( j in 1:3){
    for( i in 1:length(list_notes)){
      n[i,j]<-notes_list[[i]][j]
    }
  }
  
  piece <- data.frame(measure =measure_column, n1 = n[,1], n2 = n[,2], n3=n[,3])
  piece <- as.data.frame(lapply(piece, function(y) gsub("L|J", "", y)))
  piece <- as.data.frame(lapply(piece, function(y) gsub("\\[|\\]", "", y)))
  
  r1 <- vector()
  for(i in 1:nrow(piece)){
    r1[i]<- regmatches(piece[i,2], regexpr("[0-9]{1,2}",piece[i,2]))
  }
  on1 <- vector()
  for(i in 1:nrow(piece)){
    on1[i] <- regmatches(piece[i,2], regexpr("[A-z]{1,2}.*",piece[i,2]))
  }
  r2 <- vector()
  for(i in 1:nrow(piece)){
    if(is.na(piece[i,3])){
      r2[i] <- NA}
    else if( piece[i,3]==""){
      r2[i] <- NA
    }
    else{
      r2[i]<- regmatches(piece[i,3], regexpr("[0-9]{1,2}",piece[i,3]))
    }
  }
  on2 <- vector()
  for(i in 1:nrow(piece)){
    if(is.na(piece[i,3])){
      on2[i] <- NA}
    else if( piece[i,3]==""){
      on2[i] <- NA
    }
    else{
      on2[i]<- regmatches(piece[i,3], regexpr("[A-z]{1,2}.*",piece[i,3]))
    }
  }
  r3 <- vector()
  for(i in 1:nrow(piece)){
    if(is.na(piece[i,4])){
      r3[i] <- NA}
    else if( piece[i,4]==""){
      r3[i] <- NA
    }
    else{
      r3[i]<- regmatches(piece[i,4], regexpr("[0-9]{1,2}",piece[i,4]))
    }
  }
  on3 <- vector()
  for(i in 1:nrow(piece)){
    if(is.na(piece[i,4])){
      on3[i] <- NA}
    else if( piece[i,4]==""){
      on3[i] <- NA
    }
    else{
      on3[i]<- regmatches(piece[i,4], regexpr("[A-z]{1,2}.*",piece[i,4]))
    }
  }
  
  piece <- data.frame(measure = measure_column, 
                      nv1 = r1, 
                      n1 = on1, 
                      nv2 = r2, 
                      n2 = on2, 
                      nv3 = r3,
                      n3 = on3)
  piece
}