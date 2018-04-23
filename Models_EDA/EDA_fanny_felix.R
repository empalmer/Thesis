source("features_fanny_felix.R")
library(GGally)
library(devtools)
install_github("ggobi/ggally")
install_github("vqv/ggbiplot")
library(ggbiplot)

#==============================================================
# Look at pairs plots and correlations
pairss <- ggpairs(ffeatures,aes(color = fcomposer))
pairss
nocpair <- ggpairs(ffeatures)
nocpair
pairs2 <- pairs(ffeatures[,-1])
pairs2

#==============================================================
# Density distributions
library(reshape)
setwd("~/Desktop/Thesis/Thesis_doc/images")
pdf("distribution_f.pdf")
ggplot(melt(ffeatures), aes(x = value, color = fcomposer)) + geom_density()+
  facet_wrap(~variable, scales = "free")
dev.off()
#==============================================================
# Correlation matrix + visualization
cor(ffeatures)

library(corrplot)
setwd("~/Desktop/Thesis/Thesis_doc/images")
pdf("cor_circles_f.pdf")
corrplot(cor(ffeatures[,-1]), order = "hclust")
dev.off()

#==============================================================
# PCA and skree plots 

PCA <- prcomp(ffeatures[,-1], scale = T)

# biplot
pdf("biplot_f.pdf")
biplot(PCA,scale = 0, xlabs = rep("",nrow(PCA$x)))
dev.off()

# biplot with elipses and color groups, first two principal components
pdf("bi_elipse_12_f.pdf")
ggbiplot(PCA, obs.scale = 1, var.scale = 1, 
              groups = ffeatures[,1], ellipse = TRUE, 
              circle = F)
dev.off()
# biplot with elipses and color groups, first two principal components
h <- ggbiplot(PCA, choices = 2:3,obs.scale = 1, var.scale = 1, 
              groups = ffeatures[,1], ellipse = TRUE, 
              circle = F)
h
#skreee
d <- data.frame(PC = 1:21,
                PVE = PCA$sdev^2 / sum(PCA$sdev^2))
pdf("skree_f.pdf")
ggplot(d, aes(x = PC, y = PVE)) + geom_line() + geom_point()
dev.off()

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

df <- PCA$x[,1:11]

km2 <- kmeans(df, 2, nstart = 20)
d <- data.frame(PC1 = PCA$x[, 1],
                PC2 = PCA$x[, 2],
                cluster = as.factor(km2$cluster),
                state = ffeatures[,1])
pdf("kmeans_2_f.pdf")
ggplot(d, aes(x = PC1, y = PC2, col = cluster)) +
  geom_point() +
  geom_text(aes(label = state), vjust = 2)
dev.off()



#===================================================
#Clustering on only felix: for age

felix_features <- ffeatures[32:nrow(ffeatures),-1]
felix_features_name <- c(rep("new",14),rep("old",15))

pca_age <- prcomp(felix_features,scale = T)

km2 <- kmeans(pca_age$x, 2, nstart = 20)
d <- data.frame(PC1 = pca_age$x[, 1],
                PC2 = pca_age$x[, 2],
                cluster = as.factor(km2$cluster),
                state = felix_features_name)
ggplot(d, aes(x = PC1, y = PC2, col = cluster)) +
  geom_point() +
  geom_text(aes(label = state), vjust = 2)
