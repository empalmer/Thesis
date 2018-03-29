library(museR)
library(tidyverse)

#==============================================================

# summary
summ <- summary(features)

# Correlation matrix
cor <- cor(features[,-1])


# Density plots!

# mean density
dens_mean <- ggplot(features,aes(x = dens_mean,color = composer))+ 
  geom_density()
dens_mean

#scale degree 1 usage
sf_1p <- ggplot(features,aes(x = sf_1,color = composer))+ 
  geom_density()
sf_1p

#scale degree 2 usage
sf_2p <- ggplot(features,aes(x = sf_2,color = composer))+ 
  geom_density()
sf_2p

#scale degree 3 usage
sf_3p <- ggplot(features,aes(x = sf_3,color = composer))+ 
  geom_density()
sf_3p


#scale degree 4 usage
sf_4p <- ggplot(features,aes(x = sf_4,color = composer))+ 
  geom_density()
sf_4p

#scale degree 7 usage
sf_7p <- ggplot(features,aes(x = sf_7,color = composer))+ 
  geom_density()
sf_7p

# Cons diss
cons_dis_plot <- ggplot(features,aes(x = cons_dis,color = composer))+ 
  geom_density()
cons_dis_plot


