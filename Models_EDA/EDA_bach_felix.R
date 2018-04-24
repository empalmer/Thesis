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
setwd("~/Desktop/Thesis/Thesis_doc/images")
pdf("cor_circles_b.pdf")
corrplot(cor(features[,-1]), order = "hclust")
dev.off()
#==============================================================
# Correlation matrix
cor(features)

#==============================================================
# PCA and skree plots 

PCA <- prcomp(features[,-1], scale = T)

# biplot
setwd("~/Desktop/Thesis/Thesis_doc/images")
pdf("biplot_b.pdf")
biplot(PCA,scale = 0,xlabs=rep("", nrow(PCA$x)))
dev.off()

# biplot with elipses and color groups, first two principal components
pdf("bi_elipse12.pdf")
ggbiplot(PCA, obs.scale = 1, var.scale = 1, 
              groups = features[,1], ellipse = TRUE, 
              circle = F)
dev.off()
# biplot with elipses and color groups, first two principal components
pdf("bi_elipse23.pdf")
ggbiplot(PCA, choices = 2:3,obs.scale = 1, var.scale = 1, 
              groups = features[,1], ellipse = TRUE, 
              circle = F)
dev.off()

#skreee
d <- data.frame(PC = 1:21,
                PVE = PCA$sdev^2 / sum(PCA$sdev^2))
pdf("skree_b.pdf")
ggplot(d, aes(x = PC, y = PVE)) +
  geom_line() + 
  geom_point()
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

#==============================================================
# Distributions for each features
library(reshape)
setwd("~/Desktop/Thesis/Thesis_doc/images")
pdf("distribution_b.pdf")
ggplot(melt(features), aes(x = value, color = composer)) + geom_density()+
  facet_wrap(~variable, scales = "free") +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(face="bold", size=10),
        strip.background = element_rect(fill = "white", size = 1))
dev.off()

#==============================================================
# k-means clustering

eleven_pcs <- PCA$x[,1:11]

km2 <- kmeans(eleven_pcs, 2, nstart = 20)
d <- data.frame(PC1 = PCA$x[, 1],
                PC2 = PCA$x[, 2],
                cluster = as.factor(km2$cluster),
                state = features[,1])
pdf("kmeans_2_b.pdf")
ggplot(d, aes(x = PC1, y = PC2, col = cluster)) +
  geom_point() +
  geom_text(aes(label = state), vjust = 2)
dev.off()

#km3
both <- c(features[1:37,1],ffeatures[,1])
km3 <- kmeans(eleven_pcs, 3, nstart = 20)
d <- data.frame(PC1 = PCA$x[, 1],
                PC2 = PCA$x[, 2],
                cluster = as.factor(km3$cluster),
                state = features[,1])
pdf("kmeans_3_b.pdf")
ggplot(d, aes(x = PC1, y = PC2, col = cluster)) +
  geom_point() +
  geom_text(aes(label = state), vjust = 2)
dev.off()





