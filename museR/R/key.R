#============================================================
#' Figure out the key of a piece
#' 
#' @param piece A piece of .krn music
#' @import tidyverse
#' 
Major_minor <- function(piece){
  krn_key <- piece[1,1]
  if(stringr::str_detect(krn_key,"\\:")){
    krn_key <- gsub("\\*","",krn_key)
    krn_key <- gsub("\\:","",krn_key)
    if(krn_key == toupper(krn_key)){
      c(krn_key,"Major")
    }else(c(krn_key,"minor"))
  } else{
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
}








