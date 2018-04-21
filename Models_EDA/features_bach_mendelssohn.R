source("~/Desktop/Thesis/Models_EDA/name-file.R")

#==============================================================
# get all data
mendelssohn <- c(felix,fanny)
data <- c(bach,felix,fanny)
name<- c(rep("bach",length(bach)),rep("felix",length(felix)),
         rep("fanny",length(fanny)))
data <- rbind(name,data)                

composer <- c(rep("bach",length(bach)),rep("mendelssohn",length(mendelssohn)))

#==============================================================
# Compute feature: mel ints
mel_intsb <- map(bach,mel_ints,"piano") # each 
mel_intsm <- map(mendelssohn,mel_ints,"piece")

#==============================================================
#compute feature: length
lenb <- map(bach,length_measures)
lenm <- map(mendelssohn,length_measures)
len <- c(lenb,lenm) %>% unlist()
#compute feature: harm ints
#harm_intb <- map(bach, freq_harm_ints)
#harm_intm <- map(mendelssohn, freq_harm_ints)

#==============================================================
# Compute feature: connsonance
cons_b <- map(bach,consonances,"piano")
cons_m <- map(mendelssohn,consonances,"piece")

cons_perf <- c(map(cons_b,1),map(cons_m,1)) %>% unlist()
cons_imp <- c(map(cons_b,2),map(cons_m,2)) %>% unlist()
cons_dis <- c(map(cons_b,3),map(cons_m,3)) %>% unlist()

#==============================================================
# Compute features: density
dens_b <- map(bach,beat_density)
dens_m <- map(mendelssohn,beat_density) # mean then sd

dens_mean <- c(map(dens_b,1),map(dens_m,1)) %>% unlist()
dens_sd <- c(map(dens_b,2),map(dens_m,2)) %>% unlist()

#==============================================================
# Compute features: scale degree freq

sf_b <- map(bach,scale_degree_freq) %>% unname()
sf_m <- map(mendelssohn,scale_degree_freq)
sf_freqs <- c(map(sf_b,3) ,map(sf_m,3))
sf_1 <- map(sf_freqs,1) %>% unlist
sf_2 <- map(sf_freqs,2) %>% unlist
sf_3 <- map(sf_freqs,3)%>% unlist
sf_4 <- map(sf_freqs,4)%>% unlist
sf_5 <- map(sf_freqs,5)%>% unlist
sf_6 <- map(sf_freqs,6)%>% unlist
sf_7 <- map(sf_freqs,7)%>% unlist
#==============================================================
# Meter
met <- c(map(bach,meter), map(mendelssohn,meter)) %>% unlist

#==============================================================
# dotted Rhythem freqs

rfb <- map(bach,dot_freq) %>% unname()
rfm <- map(mendelssohn,dot_freq) %>% unname()

rf <- c(rfb,rfm) %>% unlist %>% unname()

#- rhtm freqs
rfb <- map(bach,rhythem_freq)
rfm <- map(mendelssohn,rhythem_freq)

rf <- c(rfb,rfm)

rf2 <- map(rf,"2") %>% unlist
rf2d <- map(rf,"2.") %>% unlist
rf4 <- map(rf,"4") %>% unlist
rf4d<- map(rf,"4.") %>% unlist
rf8<- map(rf,"8") %>% unlist
rf8d<- map(rf,"8.") %>% unlist
rf16<- map(rf,"16") %>% unlist
rf32<- map(rf,"32") %>% unlist


#================================================================
# Features Data frame

features <- data.frame(composer,
                        cons_dis,cons_imp,cons_perf,
                        dens_mean,dens_sd,
                        sf_1,sf_2,sf_3,sf_4,sf_5,sf_6,sf_7,len,rf2,
                       rf2d,rf4,rf4d,rf8,rf8d,
                       rf16,rf32)



