#==============================================================
key_sigs <- c("nosf","f#","f#c#","f#c#g#","f#c#g#d#","f#c#g#d#a#",
              "f#c#g#d#a#e#","f#c#g#d#a#e#b#",
              "b-e-a-d-g-c-f-","b-e-a-d-g-c-","b-e-a-d-g-",
              "b-e-a-d-","b-e-a-","b-e-","b-")
major_keys <- c("C","G","D","A","E","B","Fs","Cs",
                "Cb","Gb","Db","Ab","Eb","Bb","FM")
minor_keys <- c("a","e","b","fs","cs","gs","ds","as","ab",
                "eb","bb","f","c","g","d")
notes <- as.factor(c("A","Ab","A#","B","Bb","B#","C","Cb","C#",
                     "D","Db","D#","E","Eb","E#","F","Fb","F#",
                     "G","Gb","G#","rest"))
key <- t(data.frame(major_keys,minor_keys))
colnames(key) <- key_sigs


#==============================================================

scale_degree_names <- c("Tonic","Supertonic","Mediant","Subdominant",
                        "Dominant","Submediant","Leading Tone")
scale_degree_roman_major <- c("I","ii","iii","IV","V","vi","viid")
scale_degree_roman_minor <- c("i","iid","III","iv","V","VI","viid")

# major scales
C <- c("C","D","E","F","G","A","B")
G <- c("G","A","B","C","D","E","F#")
D <- c("D","E","F#","G","A","B","C#")
A <- c("A","B","C#","D","E","F#","G#")
E <- c("E","F#","G#","A","B","C#","D#")
B <- c("B","C#","D#","E","F#","G#","A#")
Fs <- c("F#","G#","A#","B","C#","D#","E#")
Cs <- c("C#","D#","E#","F#","G#","A#","B#")
Cb <- c("Cb","Db","Eb","Fb","Gb","Ab","Bb")
Gb <- c("Gb","Ab","Bb","Cb","Db","Eb","F")
Db <- c("Db","Eb","F","Gb","Ab","Bb","C")
Ab <- c("Ab","Bb","C","Db","Eb","F","G")
Eb <- c("Eb","F","G","Ab","Bb","C","D")
Bb <- c("Bb","C","D","Eb","F","G","A")
FM <- c("F","G","A","Bb","C","D","E")


# minor scales - natrual
a <- c("A","B","C","D","E","F","G")
e <- c("E","F#","G","A","B","C","D")
b <- c("B","C#","D","E","F#","G","A")
fs <- c("F#","G#","A","B","C#","D","E")
cs <- c("C#","D#","E","F#","G#","A","B")
gs <- c("G#","A#","B","C","D#","E","F#")
ds <- c("D#","E#","F#","G#","A#","B","C#")
as <- c("A#","B#","C#","D#","E#","F#","G#")
ab <- c("Ab","Bb","Cb","Db","Eb","Fb","Gb")
eb <- c("Eb","F","Gb","Ab","Bb","Cb","Db")
bb <- c("Bb","C","Db","Eb","F","Gb","Ab")
f <- c("F","G","Ab","Bb","C","Db","Eb")
c <- c("C","D","Eb","F","G","Ab","Bb")
g <- c("G","A","Bb","C","D","Eb","F")
d <- c("D","E","F","G","A","Bb","C")

scales <- data.frame(scale_degree_names,scale_degree_roman_major,
                     C,G,D,A,E,B,Fs,Cs,
                     Cb,Gb,Db,Ab,Eb,Bb,FM,scale_degree_roman_minor,
                     a,e,b,fs,cs,gs,ds,as,ab,eb,bb,f,c,g,d)

Major_minor <- function(piece){
  krn_key <- piece[1,1]
  krn_key <- gsub("\t.*","",krn_key)
  key_s <- gsub("\\*k","",krn_key)
  key_s <- gsub("\\[","",key_s)
  key_s <- gsub("\\]","",key_s)
  note_cols <- grep("n\\.n", colnames(piece),value = T)
  note_df <-  piece[,note_cols]
  if(key_s ==""){key_s <- "nosf"}
  m_m <- key[,key_s] %>% unname() 
  tonics <- scales[1,m_m] %>% unlist()%>% unname() %>% as.character()
  fifths <- scales[5,m_m] %>% unlist()%>% unname() %>% as.character()
  tonics_fifths_count <- rep(0,2)
  for(i in 1:ncol(note_df)){
    a <- table(note_df[,i])
    a
    mmtonic <- a[tonics] %>% as.vector
    mmtonic[is.na(mmtonic)]<- 0
    mmfifth <- a[fifths] %>% as.vector
    mmfifth[is.na(mmfifth)]<- 0
    tonics_fifths_count <- tonics_fifths_count + mmtonic + mmfifth
  }
  names(tonics_fifths_count) <- tonics
  Major <- tonics_fifths_count[1] %>% unname
  minor <- tonics_fifths_count[2] %>% unname
  if(Major >= minor){x <- c(tonics[1],"Major")
  }else{x <- c(tonics[2],"minor")}
  x
}








