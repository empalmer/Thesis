library(class)


PCA <- prcomp(features[,-1],scale = T)
xs <- PCA$x

#===================================================
# find best k:
fold <- rep(1:5,each = ceiling(nrow(features)/5))
fold <- sample(fold,nrow(features))
mcr_j <- rep(NA,12)
for(j in 1:12){
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- xs[which(fold !=i),]
    test <- xs[which(fold ==i),]
    y <- features[which(fold!=i),1]
    yt <- features[which(fold ==i),1]
    x <- knn(train,test,y, k = j)
    t <- table(x,yt)
    mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
  }
  mcr_j[j] <- mean(mcr_i)
}
which(mcr_j == min(mcr_j))

# KNN Classification, with k as chosen above
fmcr <- 0
for(j in 1:100){
  fold <- rep(1:5,each = ceiling(nrow(features)/5))
  fold <- sample(fold,nrow(features))
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- xs[which(fold !=i),]
    test <- xs[which(fold ==i),]
    y <- features[which(fold!=i),1]
    yt <- features[which(fold ==i),1]
    x <- knn(train,test,y, k = 9)
    t <- table(x,yt)
    mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
  }
  fmcr[j] <- mean(mcr_i)
}
MCR_KNN_b <- mean(fmcr)
MCR_KNN_b
