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

df2 <- cbind(t(df),c("unison","m2", "M2","m3",
                     "M3","p4","tt","p5", "m6","M6","m7","M7"))


unison <- ggplot(df, aes(x = df$unison)) + geom_density()
m2 <- ggplot(df, aes(x = df$m2)) + geom_density()
M2 <- ggplot(df, aes(x = df$M2)) + geom_density()
m3 <- ggplot(df, aes(x = df$m3)) + geom_density()
M3 <- ggplot(df, aes(x = df$M3)) + geom_density()
p4 <- ggplot(df, aes(x = df$p4)) + geom_density()
tt <- ggplot(df, aes(x = df$tt)) + geom_density()
p5 <- ggplot(df, aes(x = df$p5)) + geom_density()
m6 <- ggplot(df, aes(x = df$m6)) + geom_density()
M6 <- ggplot(df, aes(x = df$M6)) + geom_density()
m7 <- ggplot(df, aes(x = df$m7)) + geom_density()
M7 <- ggplot(df, aes(x = df$M7)) + geom_density()
plot_grid(unison,m2,M2,m3,M3,p4,tt,p5,m6,M6,m7,M7)

ggplot(df3, aes(x = factor(df3[,2],
                           levels = c("unison",
                                      "m2", "M2","m3", "M3","p4",
                                      "tt","p5", "m6","M6","m7","M7")),
                y = v)) + geom_boxplot() + labs(x = "interval", y = "freq")


