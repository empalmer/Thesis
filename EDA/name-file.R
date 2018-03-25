library(museR)
library(tidyverse)


#==============================================================
# fanny
wo_komst <- piece_df(c("h1.xml-a.krn","h1.xml-b.krn","h1.xml-c.krn"))
das_stille <- piece_df(c("h2.xml-a.krn","h2.xml-b.krn","h2.xml-c.krn"))
huet_in <- piece_df(c("h3v2.a.krn","h3v2.b.krn","h3v2.c.krn"))
#dieser_nact <- piece_df("h4.xml-a.krn","h4.xml-b.krn","h4.xml-c.krn")
lied_des <- piece_df(c("h5.xml-a.krn","h5.xml-b.krn","h5.xml-c.krn"))
mein_herz <- piece_df(c("h7.xml-a.krn","h7.xml-b.krn","h7.xml-c.krn"))
friiling <- piece_df(c("h8.xml-a.krn","h8.xml-b.krn","h8.xml-c.krn"))
eilig <- piece_df(c("h9.xml-a.krn","h9.xml-b.krn","h9.xml-c.krn"))
ave_maria <- piece_df(c("h11.xml-a.krn","h11.xml-b.krn","h11.xml-c.krn"))
lebewohl <- piece_df(c("h12.xml-a.krn","h12.xml-b.krn","h12.xml-c.krn"))
die_nonne <- piece_df(c("h13.xml-a.krn","h13.xml-b.krn","h13.xml-c.krn"))
wanderlied <- piece_df(c("h15.xml-a.krn","h15.xml-b.krn","h15.xml-c.krn"))
nn <- piece_df(c("h16.xml-a.krn","h16.xml-b.krn","h16.xml-c.krn"))
zchwishen  <- piece_df(c("h18.xml-a.krn","h18.xml-b.krn","h18.xml-c.krn"))
die_alof <- piece_df(c("h19.xml-a.krn","h19.xml-b.krn","h19.xml-c.krn"))
#wass_will <- piece_df("h20.xml-a.krn","h20.xml-b.krn","h20.xml-c.krn")
#am_leuct <- piece_df("h21.xml-a.krn","h21.xml-b.krn","h21.xml-c.krn")
und_wii <- piece_df(c("h22.xml-a.krn","h22.xml-b.krn","h22.xml-c.krn"))

#==============================================================
# fanny
fanny_pieces <- list(wo_komst,das_stille,lied_des,mein_herz,
                     friiling,eilig,ave_maria, lebewohl,die_nonne,nn,
                     wanderlied,zchwishen, die_alof, und_wii)

fanny <- list.files(path = "~/Desktop/Thesis/Data",
                        pattern = "^h.+xml.*krn$") %>%
  map_chr(substring,1,4) %>%
  unique() %>%
  map(grep,fanny_pieces, value = T) %>%
  map(piece_df2,c("V","pR","pL"))

#==============================================================
# felix
felix_names <- list(c("f1o8p.xml","Minelied im Mai"), 
              c("f2o8p.xml","Das Heimweh"),
              c("f4o8p.xml","Erntelied"),
              c("f5o8p.xml","Pilgerspruch"))

felix_krn <- list.files(path = "~/Desktop/Thesis/Data",
                        pattern = "^f.+krn$") %>%
  map_chr(substring,1,5) %>%
  unique() %>%
  map(grep,felix_pieces, value = T)
felix_krn <- felix_krn[-c(3,4)]
felix <- map(felix_krn, piece_df2, insts = c("V","pR","pL"))

#==============================================================
# bach

old <- getwd()
setwd("~/Desktop/Thesis/BAch")
bach <- list.files(path = "~/Desktop/Thesis/BAch") %>%
  map(piece_df, insts = "piano")
setwd(old)
 









