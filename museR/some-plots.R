library(ggplot2)
library(cowplot)
source("~/Desktop/Thesis/museR/R/read.R")
source("~/Desktop/Thesis/museR/R/mel_int.R")

wo_komst <- piece_df("h1.xml-a.krn","h1.xml-b.krn","h1.xml-c.krn")
das_stille <- piece_df("h2.xml-a.krn","h2.xml-b.krn","h2.xml-c.krn")
#huet_in <- piece_df("h3.xml-a.krn","h3.xml-b.krn","h3.xml-c.krn")
#dieser_nact <- piece_df("h4.xml-a.krn","h4.xml-b.krn","h4.xml-c.krn")
lied_des <- piece_df("h5.xml-a.krn","h5.xml-b.krn","h5.xml-c.krn")
mein_herz <- piece_df("h7.xml-a.krn","h7.xml-b.krn","h7.xml-c.krn")
friiling <- piece_df("h8.xml-a.krn","h8.xml-b.krn","h8.xml-c.krn")
eilig <- piece_df("h9.xml-a.krn","h9.xml-b.krn","h9.xml-c.krn")
ave_maria <- piece_df("h11.xml-a.krn","h11.xml-b.krn","h11.xml-c.krn")
lebewohl <- piece_df("h12.xml-a.krn","h12.xml-b.krn","h12.xml-c.krn")
#die_nonne <- piece_df("h13.xml-a.krn","h13.xml-b.krn","h13.xml-c.krn")
wanderlied <- piece_df("h15.xml-a.krn","h15.xml-b.krn","h15.xml-c.krn")
nn <- piece_df("h16.xml-a.krn","h16.xml-b.krn","h16.xml-c.krn")
zchwishen  <- piece_df("h18.xml-a.krn","h18.xml-b.krn","h18.xml-c.krn")
die_alof <- piece_df("h19.xml-a.krn","h19.xml-b.krn","h19.xml-c.krn")
#wass_will <- piece_df("h20.xml-a.krn","h20.xml-b.krn","h20.xml-c.krn")
#am_leuct <- piece_df("h21.xml-a.krn","h21.xml-b.krn","h21.xml-c.krn")
und_wii <- piece_df("h22.xml-a.krn","h22.xml-b.krn","h22.xml-c.krn")

pieces <- list(wo_komst,das_stille,lied_des,mein_herz,
           friiling,eilig,ave_maria, lebewohl, wanderlied,
           nn, zchwishen, die_alof, und_wii)

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


