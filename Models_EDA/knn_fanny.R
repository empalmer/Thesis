library(class)

#===================================================
# KNN Classification 
kn <- 0
for(k in 1:100){
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
      mcr_i[i] <- (sum(t) - sum(diag(t)))/sum(t)
    }
    fmcr_i[j] <- mean(mcr_i)
  }
  kn[k] <- which(fmcr_i == min(fmcr_i))
}
K <- 1 #(mode of kn)

ffeatures2 <- scale(ffeatures[,-1],scale = T, center = T)
fmcr <- 0
for(j in 1:100){
  fold <- rep(1:5,each = ceiling(nrow(ffeatures2)/5))
  fold <- sample(fold,nrow(ffeatures2))
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- ffeatures2[which(fold !=i),]
    test <- ffeatures2[which(fold ==i),]
    y <- ffeatures[which(fold !=i),1]
    yt <- ffeatures[which(fold ==i),1]
    x <- knn(train,test,y, k = 1)
    t <- table(x,yt)
    mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
  }
  fmcr[j] <- mean(mcr_i)
}
MCR_knn_f <- mean(fmcr)
MCR_knn_f

# Prediction: 
pred <- knn(ffeatures[,-1],disputed_features,ffeatures[,1],k=8)
pred
