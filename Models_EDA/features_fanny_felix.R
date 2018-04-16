source("~/Desktop/Thesis/Models_EDA/name-file.R")

#==============================================================
# get all data

fdata <- c(felix,fanny)
fcomposer<- c(rep("felix",length(felix)),
              rep("fanny",length(fanny)))
fdata <- rbind(fcomposer,fdata)                

# Compute feature: mel ints
mel_intsh <- map(fanny,mel_ints,"piece") # each 
mel_intsf <- map(felix,mel_ints,"piece")

#compute feature: length
lenh <- map(fanny,length_measures)
lenf <- map(felix,length_measures)
flen <- c(lenh,lenf) %>% unlist()
#compute feature: harm ints
#harm_intb <- map(bach, freq_harm_ints)
#harm_intm <- map(mendelssohn, freq_harm_ints)

# Compute feature: connsonance
cons_h <- map(fanny,consonances,"piece")
cons_f <- map(felix,consonances,"piece")

fcons_perf <- c(map(cons_h,1),map(cons_f,1)) %>% unlist()
fcons_imp <- c(map(cons_h,2),map(cons_f,2)) %>% unlist()
fcons_dis <- c(map(cons_h,3),map(cons_f,3)) %>% unlist()

# Compute features: density
dens_h <- map(fanny,beat_density)
dens_f <- map(felix,beat_density) # mean then sd

fdens_mean <- c(map(dens_h,1),map(dens_f,1)) %>% unlist()
fdens_sd <- c(map(dens_h,2),map(dens_f,2)) %>% unlist()

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

#==============================================================
#==============================================================
# FEATURES create data frame
ffeatures <- data.frame(fcomposer,
                        fcons_dis,fcons_imp,fcons_perf,
                        fdens_mean,fdens_sd,
                        fsf_1,fsf_2,fsf_3,fsf_4,fsf_5,fsf_6,fsf_7,flen)