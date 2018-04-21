source("features_bach_mendelssohn.R")
library(GGally)
library(cowplot)
library(ggbiplot)
#==============================================================
# Look at pairs plots and correlations
pairss <- ggpairs(features,aes(color = composer))
pairss
nocpair <- ggpairs(features)
nocpair
pairs2 <- pairs(features[,-1])
pairs2

library(corrplot)
corrplot(cor(features[,-1]), order = "hclust")

#==============================================================
# Correlation matrix
cor(features)

#==============================================================
# PCA and skree plots 

PCA <- prcomp(features[,-1], scale = T)
plot(PCA, type = "1", main="Principal Components Analysis")

# biplot
biplot(PCA,scale = 0,xlabs=rep("", nrow(PCA$x)))

# biplot with elipses and color groups, first two principal components
g <- ggbiplot(PCA, obs.scale = 1, var.scale = 1, 
              groups = features[,1], ellipse = TRUE, 
              circle = F)
g
# biplot with elipses and color groups, first two principal components
h <- ggbiplot(PCA, choices = 2:3,obs.scale = 1, var.scale = 1, 
              groups = features[,1], ellipse = TRUE, 
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

#==============================================================
# distributions for all 


a <- ggplot(features, aes(x = features$dens_mean,color = features[,1])) + 
  geom_density()
b <- ggplot(features, aes(x = features[,3],color = features[,1])) + 
  geom_density()
c <- ggplot(features, aes(x = features[,4],color = features[,1])) + 
  geom_density()

plot_grid(a,b)

#==============================================================
# k-means clustering



km2 <- kmeans(features[ ,-1], 2, nstart = 20)
pca1 <- prcomp(features[, -1])
d <- data.frame(PC1 = pca1$x[, 1],
                PC2 = pca1$x[, 2],
                cluster = as.factor(km2$cluster),
                state = features[,1])
ggplot(d, aes(x = PC1, y = PC2, col = cluster)) +
  geom_point() +
  geom_text(aes(label = state), vjust = 2)

km3 <- kmeans(features[ ,-1], 3, nstart = 20)
pca1 <- prcomp(features[, -1])
d <- data.frame(PC1 = pca1$x[, 1],
                PC2 = pca1$x[, 2],
                cluster = as.factor(km3$cluster),
                state = features[,1])
ggplot(d, aes(x = PC1, y = PC2, col = cluster)) +
  geom_point() +
  geom_text(aes(label = state), vjust = 2)






