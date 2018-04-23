library(e1071)
library(caret)

#============================================================
# 5-fold Naive Bayes on 11 PCA's
PCA <- prcomp(features[,-1],scale = T)
xs <- PCA$x[,1:11]

mcr <- 0
for(j in 1:100){
  fold <- rep(1:5,each = ceiling(nrow(features)/5))
  fold <- sample(fold,nrow(features))
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- xs[which(fold !=i),]
    test <- xs[which(fold ==i),]
    y <- features[which(fold!=i),1]
    yt <- features[which(fold==i),1]
    nb_mod <- naiveBayes(x = train, y = y)
    x <- predict(nb_mod,test,type = "class")
    t <- table(x,yt)
    mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
  }
  mcr[j] <- mean(mcr_i)
}
MCR_NB_PCA_b <- mean(mcr)
MCR_NB_PCA_b
