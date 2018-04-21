


#==============================================================
convert_chords2_scale_degree <- function(piece){
  chords_notes <- extract_chord_value_beat(piece)
  key_sig <- Major_minor(piece)[1]
  scale <- scales[,key_sig] %>% unname() %>% as.character()
  deg <- scales[,"scale_degree_names"] %>% unname %>% as.character
  g <- function(x){deg[which(scale== x)]}
  f <- function(note){map(note,g) %>% unlist()}
  chords_deg <- map(chords_notes,f)
  chords_deg
}
#==============================================================


chord_or_harm_int <- function(chord){
  u <- unique(chord)
  l <- length(u)
  if(l== 2){
    find_harm_int(u)
  }else if(l == 3){
    block_chord(chord)
  }else if(l ==1){
    "not a chord"
  }else{
    "dont know"
  }
}

#==============================================================

find_harm_int <- function(v){ # lenghth 2
  f <- abs(v[1]-v[2] %% 12) # first(lower) note minus 2nd
  ints <- c("unison","m2", "M2","m3", "M3","p4","tt",
            "p5", "m6","M6","m7","M7")
  ints <- factor(ints, ordered = T)
  ints[f + 1]
}


harm_ints <- function(piece){
  a <- extract_chord_value_beat(piece)
  l <- map(a,length)
  harms <- a[which(l == 2)]
  ints <- map(harms, find_harm_int)
  ints
}

freq_harm_ints <- function(piece){
  harm_ints_list <- harm_ints(piece) %>% unlist()
  freq <- table(harm_ints_list)/sum(table(harm_ints_list))
  freq
}

block_chord <- function(v){
  #first_note <-
  #second_note <-
  #third_note <-
  #f1 <- v[1] - v[2] %% 12
  #f2 <- v[2] - v[3] %% 12
  #c(f1,f2)
}

#==============================================================

chords <- function(piece){
  a <- extract_chord_value_beat(piece)
  map(a,chord_or_harm_int)
}


#==============================================================
inverted_chord <- function(a,b,c){

}
