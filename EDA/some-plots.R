library(ggplot2)
library(cowplot)
library(stringr)
source("~/Desktop/Thesis/museR/R/read.R")
source("~/Desktop/Thesis/museR/R/mel_int.R")

df <- data.frame(matrix(nrow = 13, ncol = 12))
colnames(df) <- c("unison","m2", "M2","m3",
                  "M3","p4","tt","p5", "m6","M6","m7","M7")
rownames(df) <- c("wo_komst","das_stille","lied_des","mein_herz",
                 "friiling","eilig","ave_maria", "lebewohl", 
                  "wanderlied","nn", "zchwishen", "die_alof", "und_wii")
for(i in 1:length(pieces)){
  df[i,] <- voice_mel_ints(pieces[[i]],"V_n.v_1")
}

v <- c()
for(i in 1:length(pieces)){
  v <- c(v,voice_mel_ints(pieces[[i]]))
}

df3 <- data.frame(v, rep(c("unison","m2", "M2","m3",
                           "M3","p4","tt","p5", "m6","M6","m7","M7"),13))

ggplot(df3, aes(x = factor(df3[,2],
                           levels = c("unison",
                                      "m2", "M2","m3", "M3","p4",
                                      "tt","p5", "m6","M6","m7","M7")),
                y = v)) + geom_boxplot() + labs(x = "interval", y = "freq")



###############################################
########### Density ###########################
###############################################


felix_d <- map(felix_df,beat_density)
fanny_d <- map(fanny_pieces,beat_density)
bach_d <- map(bach_df,beat_density)
bach_means <- map(bach_d,1) %>% unlist()
bach_sd <- map(bach_d,2) %>% unlist()
bach_ds <- data.frame(m = bach_means,s = bach_sd)
ggplot(bach_ds,aes(x = m)) + geom_density()
ggplot(bach_ds,aes(x = s)) + geom_density()

mean <- c(unlist(map(felix_d,1)),
          unlist(map(fanny_d,1)),
          unlist(map(bach_d,1)))
sd <- c(unlist(map(felix_d,2)),
        unlist(map(fanny_d,2)),
        unlist(map(bach_d,2)))
composer <- c(rep("felix",2), rep("fanny",14),rep("bach",36))
means <- data.frame(mean = mean, sd = sd, composer = composer)
ggplot(means,aes(x = mean,color = composer))+ geom_density()
ggplot(means,aes(x = sd,color = composer))+ geom_density()




###############################################
########### Conssonance #######################
###############################################

felix_cv <- map(felix_df,consonances,"V")
fanny_cv <- map(fanny_pieces,consonances,"V")

con <- c(unlist(map(felix_d,1)),unlist(map(fanny_d,1)))
composer <- c(rep("felix",2), rep("fanny",14))
cons <- data.frame(consonance = con, composer = composer)

ggplot(cons,aes(x =con,color = composer))+ geom_density()




