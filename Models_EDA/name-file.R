library(museR)
library(tidyverse)

#==============================================================
# fanny
fanny_names <- list(c("Wo_kommst_du_her","h1.krn"),
                    c("Das_stille_Fleh'n","h2.krn"),
                    c("Heut'_in_dieser_Nacht","h3.krn"),
                    c("Die_furchtsame_Trane","h4.krn"),
                    c("Lied_des_Schafers","h5.krn"),
                    c("Ohne_sie","h6.krn"),
                    c("Mein_Herz_das_ist_begraben","h7.krn"),
                    c("Fruhlingserinnerrung","h8.krn"),
                    c("Eilig zieh'n in weiter Ferne","h9.krn"),
                    c("Ihr Tone schwingt euch","h10.krn"), 
                    c("Ave Maria","h11.krn"),
                    c("Die Nonne","h12.krn"),
                    c("Lebewohl","h13.krn"),
                    c("Die Schwalbe","h14.krn"),
                    c("Wanderlied","h15.krn"),
                    c("Fruhlingsnahe","h16.krn"),
                    c("Heimweh","h17.krn"),
                    c("Zwischen Gaeta und Capua","h18.krn"),
                    c("Die Aolsharfe auf dem Schlosse zu Baden","h19.krn"),
                    c("Was will die einsame Trane","h20.krn"),
                    c("Am leuchtenden Sommermorgen","h21.krn"),
                    c("Und wubten's die Blumen","h22.krn"), 
                    c("Gerausch","h23.krn"),
                    c("Uber die Berge","h24.krn"),
                    c("Schlafe,schlaf","h25.krn"),
                    c("Zu deines","h26.krn"),
                    c("Genesungsfeier","h27.krn"),
                    c("Fruhlingslied","h28.krn"),
                    c("Der schnee","h29.krn"),
                    c("Nact","h30.krn"),
                    c("Zauberkreis","h31.krn"),
                    c("Traum","h32.krn"),
                    c("Schwanenlied","h33.krn"))
old <- getwd()
setwd("~/Desktop/Thesis/Data/Fanny_working_krn")
fanny_l <- list.files(path = "~/Desktop/Thesis/Data/Fanny_working_krn")
fanny <-  map(fanny_l,piece_df,c("piece"))
setwd(old)


#==============================================================
# felix
felix_names <- list(c("f1o8p.xml","Minelied im Mai"), 
              c("f2o8p.xml","Das Heimweh"),
              c("f4o8p.xml","Erntelied"),
              c("f5o8p.xml","Pilgerspruch"))
felix_krn <- list.files(path = "~/Desktop/Thesis/Data/Felix_working_krn") 
old <- getwd()
setwd("~/Desktop/Thesis/Data/Felix_working_krn")
felix <- map(felix_krn, piece_df, insts = c("piece"))
setwd(old)

# for(i in 1:length(felix_krn)){
#   print(felix_krn[i])
#   piece_df(felix_krn[i],"x")
# }

#==============================================================
# bach

old <- getwd()
setwd("~/Desktop/Thesis/Data/Bach")
bach <- list.files(path = "~/Desktop/Thesis/Data/Bach") %>%
  map(piece_df, insts = "piano")
setwd(old)
 
#==============================================================
# Disputed pieces
old <- getwd()
setwd("~/Desktop/Thesis/Data/Disputed_pieces")
disputed <- list.files(path = "~/Desktop/Thesis/Data/Disputed_pieces") %>%
  map(piece_df, insts = "piece")
disputed <- list(piece_df("fop8-2.krn","piece"),
                 piece_df("fop8-3.krn","piece"),
                 piece_df("f812.krn","piece"),
                 piece_df("f9_12.xml.krn","piece"))
setwd(old)



