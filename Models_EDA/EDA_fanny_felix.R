source("features_fanny_felix.R")
library(GGally)

#==============================================================
# Look at pairs plots and correlations
pairss <- ggpairs(ffeatures,aes(color = fcomposer))
pairss
nocpair <- ggpairs(ffeatures)
nocpair
pairs2 <- pairs(ffeatures[,-1])
pairs2

#==============================================================
# Correlation matrix
cor(ffeatures)

#==============================================================
# PCA and skree plots 

PCA <- prcomp(ffeatures[,-1], scale = T)

# biplot
biplot(PCA,scale = 0, xlabs = rep("",nrow(PCA$x)))


# biplot with elipses and color groups, first two principal components
g <- ggbiplot(PCA, obs.scale = 1, var.scale = 1, 
              groups = ffeatures[,1], ellipse = TRUE, 
              circle = F)
g
# biplot with elipses and color groups, first two principal components
h <- ggbiplot(PCA, choices = 2:3,obs.scale = 1, var.scale = 1, 
              groups = ffeatures[,1], ellipse = TRUE, 
              circle = F)
h
#skreee
d <- data.frame(PC = 1:13,
                PVE = PCA$sdev^2 / sum(PCA$sdev^2))
ggplot(d, aes(x = PC, y = PVE)) +
  geom_line() + 
  geom_point()

# circle
theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle,aes(x,y)) + geom_path()

loadings <- data.frame(PCA$rotation, 
                       .names = row.names(PCA$rotation))
p + geom_text(data=loadings, 
              mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")


#===================================================
# clustering

km2 <- kmeans(ffeatures[ ,-1], 2, nstart = 20)
pca1 <- prcomp(ffeatures[, -1])
d <- data.frame(PC1 = pca1$x[, 1],
                PC2 = pca1$x[, 2],
                cluster = as.factor(km2$cluster),
                state = ffeatures[,1])
ggplot(d, aes(x = PC1, y = PC2, col = cluster)) +
  geom_point() +
  geom_text(aes(label = state), vjust = 2)
