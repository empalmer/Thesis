library(class)

#===================================================
# KNN Classification 
fold <- rep(1:5,each = ceiling(nrow(ffeatures)/5))
fold <- sample(fold,nrow(ffeatures))
fmcr_i <- rep(NA,12)
for(j in 1:12){
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- ffeatures[which(fold !=i),]
    test <- ffeatures[which(fold ==i),]
    x <- knn(train[,-1],test[,-1],train[,1], k = j)
    t <- table(x,test[,1])
    mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
  }
  fmcr_i[j] <- mean(mcr_i)
}
which(fmcr_i == min(fmcr_i))


fold <- rep(1:5,each = ceiling(nrow(ffeatures)/5))
fold <- sample(fold,nrow(ffeatures))
mcr_i <- rep(NA,5)
for(i in 1:5){
  train <- ffeatures[which(fold !=i),]
  test <- ffeatures[which(fold ==i),]
  x <- knn(train[,-1],test[,-1],train[,1], k = 8)
  t <- table(x,test[,1])
  mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
}
fmcr <- mean(mcr_i)
fmcr


# Prediction: 

pred <- knn(ffeatures[,-1],disputed_features,ffeatures[,1],k=8)
pred
