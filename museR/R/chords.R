#==============================================================
#' Extract_chord_name_beat
#' This function gives all the harmonic notes that are attacked
#' at the same time
#' @param piece A piece of music in R's piece_df
#' @return A list of all the row by voiced chords
extract_chord_name_beat <- function(piece){
  note_cols <- grep("n\\.n", colnames(piece),value = T)
  note_df <-  piece[,note_cols]
  chords <- list()
  note_df <- map_df(note_df,function(x){gsub("rest",NA,x)})
  x <- !map_df(note_df,is.na)
  for(i in 1:nrow(x)){
    chords[[i]] <- note_df[i,x[i,]] %>%unname() %>% as.character()
    }
  chords
}

#==============================================================
#==============================================================
#' extract_chord_value_beat
#' Extracts..
#' @param piece
#' @return list of all note value chords

extract_chord_value_beat <- function(piece){
  piece <- assign_sd(piece)
  note_cols <- grep("n\\.v", colnames(piece),value = T)
  note_df <-  piece[,note_cols]
  chords <- list()
  note_df <- map_df(note_df,function(x){gsub("rest",NA,x)})
  x <- !map_df(note_df,is.na)
  for(i in 1:nrow(x)){
    chords[[i]] <- note_df[i,x[i,]] %>%unname() %>% as.numeric()
  }
  return(chords)
}

#==============================================================
#==============================================================
#' one_chord_harms
#' Given a chord, this returns the intervals between chord notes.
#' @param chord A vector of chord notes
#' @return A vector of the intervals between chord notes
one_chord_harms <- function(chord){
  l <- length(chord)
  if(l < 2){return(chord)}
  c <- 0
  for(i in 2:l){
    c[i-1] <- (chord[i] - chord[i-1]) %% 12
  }
  c
}

#==============================================================
#' chord_harms
#' Given a piece, this returns a list of intervals between chord notes.
#' @param piece
#' @return A list of chords spelled out by interval
chord_harms <- function(piece){
  chords <- extract_chord_value_beat(piece)
  harm_form <- map(chords,one_chord_harms)
  harm_form
}

#==============================================================
#' harm_ints
#'
#' @param piece
#' @return

harm_ints <- function(piece){
  chords <- chord_harms(piece)
  l <- length(chords)
  ls <- map(chords,length) %>% unlist
  harms <- which(ls == 1)
  twos <- chords[harms] %>% unlist
  ints <- c("unison","m2", "M2","m3", "M3","p4","tt",
            "p5", "m6","M6","m7","M7")
  m <- table(twos)/sum(table(twos))
  names(m) <- ints
  m
}


#==============================================================
#' freq_chord_size
#'
#' @param piece
#' @param type harmonic interval = 2, triad = 3, seventh = 4
#' @return
#'
freq_chord_size <- function(piece,type){
  chords <- chord_harms(piece)
  l <- length(chords)
  ls <- map(chords,length) %>% unlist
  two <- sum(ls ==type)
  freq <- two/l
  freq
}
