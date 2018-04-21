source("~/Desktop/Thesis/Models_EDA/name-file.R")

#==============================================================
# get all data

fdata <- c(fanny,felix)
fcomposer<- c(rep("fanny",length(felix)),
              rep("felix",length(fanny)))
fdata <- rbind(fcomposer,fdata)                

#==============================================================
# Compute feature: mel ints

# Felix/Fanny
mel_intsh <- map(fanny,mel_ints,"piece")
mel_intsf <- map(felix,mel_ints,"piece")
mel_intsM <- c(mel_intsh,mel_intsf)
fm_1 <- map(mel_intsM,1) %>% unlist
fm_2 <- map(mel_intsM,2) %>% unlist
fm_3 <- map(mel_intsM,3)%>% unlist
fm_4 <- map(mel_intsM,4)%>% unlist
fm_5 <- map(mel_intsM,5)%>% unlist
fm_6 <- map(mel_intsM,6)%>% unlist
fm_7 <- map(mel_intsM,7)%>% unlist
fm_8 <- map(mel_intsM,8)%>% unlist
fm_9 <- map(mel_intsM,9)%>% unlist
fm_10 <- map(mel_intsM,10)%>% unlist
fm_11 <- map(mel_intsM,11)%>% unlist
fm_12 <- map(mel_intsM,12)%>% unlist


# Disputed
mel_intsd <- map(disputed,mel_ints,"piece")
fmd1 <- map(mel_intsd,1) %>% unlist
fmd2 <- map(mel_intsd,2) %>% unlist
fmd3 <- map(mel_intsd,3)%>% unlist
fmd4 <- map(mel_intsd,4)%>% unlist
fmd5 <- map(mel_intsd,5)%>% unlist
fmd6 <- map(mel_intsd,6)%>% unlist
fmd7 <- map(mel_intsd,7)%>% unlist
fmd8 <- map(mel_intsd,8)%>% unlist
fmd9 <- map(mel_intsd,9)%>% unlist
fmd10 <- map(mel_intsd,10)%>% unlist
fmd11 <- map(mel_intsd,11)%>% unlist
fmd12 <- map(mel_intsd,12)%>% unlist

#==============================================================
#==============================================================
#compute feature: length
lenh <- map(fanny,length_measures)
lenf <- map(felix,length_measures)

lend <- map(disputed,length_measures) %>% unlist

flen <- c(lenh,lenf) %>% unlist()
#==============================================================
#==============================================================
#compute feature: harm ints
#harm_intb <- map(bach, freq_harm_ints)
#harm_intm <- map(mendelssohn, freq_harm_ints)

#==============================================================
#==============================================================
# Compute feature: connsonance
cons_h <- map(fanny,consonances,"piece")
cons_f <- map(felix,consonances,"piece")

cons_d <- map(disputed,consonances,"piece")

fcons_perf <- c(map(cons_h,1),map(cons_f,1)) %>% unlist()
fcons_imp <- c(map(cons_h,2),map(cons_f,2)) %>% unlist()
fcons_dis <- c(map(cons_h,3),map(cons_f,3)) %>% unlist()

cons_perf_d <- map(cons_d,1) %>% unlist
cons_imp_d <- map(cons_d,2) %>% unlist
cons_dis_d <- map(cons_d,3) %>% unlist

#==============================================================
#==============================================================
# Compute features: density
dens_h <- map(fanny,beat_density)
dens_f <- map(felix,beat_density) # mean then sd

dens_d <- map(disputed,beat_density)

fdens_mean <- c(map(dens_h,1),map(dens_f,1)) %>% unlist()
fdens_sd <- c(map(dens_h,2),map(dens_f,2)) %>% unlist()


dens_mean_d <- map(dens_d,1) %>% unlist
dens_sd_d <- map(dens_d,2) %>% unlist

#==============================================================
#==============================================================
# Compute features: scale degree freq

sf_h <- map(fanny,scale_degree_freq) %>% unname()
sf_f <- map(felix,scale_degree_freq)
sf_freqs <- c(map(sf_h,3) ,map(sf_f,3))
fsf_1 <- map(sf_freqs,1) %>% unlist
fsf_2 <- map(sf_freqs,2) %>% unlist
fsf_3 <- map(sf_freqs,3)%>% unlist
fsf_4 <- map(sf_freqs,4)%>% unlist
fsf_5 <- map(sf_freqs,5)%>% unlist
fsf_6 <- map(sf_freqs,6)%>% unlist
fsf_7 <- map(sf_freqs,7)%>% unlist

sf_d <- map(disputed,scale_degree_freq) %>% unname()
sf_fd <- map(sf_d,3)
fd_1 <- map(sf_fd,1) %>% unlist
fd_2 <- map(sf_fd,2) %>% unlist
fd_3 <- map(sf_fd,3)%>% unlist
fd_4 <- map(sf_fd,4)%>% unlist
fd_5 <- map(sf_fd,5)%>% unlist
fd_6 <- map(sf_fd,6)%>% unlist
fd_7 <- map(sf_fd,7)%>% unlist

#==============================================================
#==============================================================
#r freqs
rfh <- map(fanny,rhythem_freq)
rfm <- map(felix,rhythem_freq)

rf <- c(rfh,rfm)

rf2 <- map(rf,"2") %>% unlist
rf2d <- map(rf,"2.") %>% unlist
rf4 <- map(rf,"4") %>% unlist
rf4d<- map(rf,"4.") %>% unlist
rf8<- map(rf,"8") %>% unlist
rf8d<- map(rf,"8.") %>% unlist
rf16<- map(rf,"16") %>% unlist
rf32<- map(rf,"32") %>% unlist


#top_dot freqs
trff <- map(felix,top_dot_freq) %>% unname()
trfh <- map(fanny,top_dot_freq) %>% unname()

trfm <- c(trfh,trff) %>% unlist %>% unname()

# dotted freqs 

rff <- map(felix,dot_freq) %>% unname()
rfh <- map(fanny,dot_freq) %>% unname()

rfm <- c(rff,rfh) %>% unlist %>% unname()

rfd <- map(disputed_features,dot_freq) %>% unname() %>% unlist
#==============================================================
# Meter
met <- c(map(fanny,meter), map(felix,meter)) %>% unlist

#==============================================================
# rhythmic entropy

r_ent <- c(map(fanny,rhy_entropy),map(felix,rhy_entropy)) %>% 
  unname %>% unlist

#==============================================================
#==============================================================
#================ FEATURE DATAFRAME ===========================
#==============================================================
#==============================================================
# FEATURES create data frame
ffeatures <- data.frame(fcomposer,
                        fcons_dis,fcons_imp,fcons_perf,
                        fdens_mean,fdens_sd,
                        fsf_1,fsf_2,fsf_3,fsf_4,fsf_5,fsf_6,
                        fsf_7,flen,rf2,rf2d,rf4,rf4d,rf8,rf8d,
                        rf16,rf32,r_ent)
#==============================================================
#==============================================================
#==============================================================
#==============================================================

disputed_features <- data.frame(cons_dis_d,cons_imp_d,
                                cons_perf_d,dens_mean_d,dens_sd_d,
                                fd_1,fd_2,fd_3,fd_4,fd_5,fd_6,
                                fd_7,lend)


#==============================================================
