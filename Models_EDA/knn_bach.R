library(class)

#===================================================
# find best k: 
fold <- rep(1:5,each = ceiling(nrow(features)/5))
fold <- sample(fold,nrow(features))
fmcr_i <- rep(NA,12)
for(j in 1:12){
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- features[which(fold !=i),]
    test <- features[which(fold ==i),]
    x <- knn(train[,-1],test[,-1],train[,1], k = j)
    t <- table(x,test[,1])
    mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
  }
  fmcr_i[j] <- mean(mcr_i)
}
which(fmcr_i == min(fmcr_i))

# KNN Classification, with k as chosen above
fold <- rep(1:5,each = ceiling(nrow(features)/5))
fold <- sample(fold,nrow(features))
mcr_i <- rep(NA,5)
for(i in 1:5){
  train <- features[which(fold !=i),]
  test <- features[which(fold ==i),]
  x <- knn(train[,-1],test[,-1],train[,1], k = 1)
  t <- table(x,test[,1])
  mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
}
fmcr <- mean(mcr_i)
fmcr

