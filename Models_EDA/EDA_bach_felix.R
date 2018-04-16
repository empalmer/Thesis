source("features_bach_mendelssohn.R")

#==============================================================
# Look at pairs plots and correlations
pairss <- ggpairs(features,aes(color = composer))
pairss
nocpair <- ggpairs(features)
nocpair
pairs2 <- pairs(features[,-1])
pairs2

#==============================================================
# Correlation matrix

cor(features)