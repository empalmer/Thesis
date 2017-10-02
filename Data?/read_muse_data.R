

data <- readLines("data.txt")
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}     

f <- function(data){
  num_rows <- length(data)
  num_col <- getmode((nchar(data)))
  
  note_letter <- numeric(num_rows)
  octave <- numeric(num_rows)
  measure_number <- numeric(num_rows)
  note_duration <- numeric(num_rows)
  
  for(i in 1:num_rows){
    if(nchar(data[i] == num_col)) {
      #v <- unlist(strsplit(data[i], "//s+"))
      v <- unlist(strsplit(data[i], " +"))
      note_letter[i] <- v[1]
      note_duration[i] <- v[2]
    }
    else{
      measure_number[i] <- as.numeric(substr(x = data[i], start = 9, stop = 9))
    }
  }
  new_data <- data.frame(note_letter,octave,measure_number, note_duration)
  new_data
}

new_data <- f(data)


