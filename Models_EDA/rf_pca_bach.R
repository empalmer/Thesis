library(randomForest)
library(caret)
library(mnormt)

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
    yt <- features[which(fold ==i),1]
    mod <- randomForest(train,  y= y,
                        mtry = 5,importance = T)
    p <- predict(mod,newdata = test)

    mcr_i[i] <- mean(p != yt)
  }
  mcr[j] <- mean(mcr_i)
}
MCR_RF_b <- mean(mcr)
MCR_RF_b
