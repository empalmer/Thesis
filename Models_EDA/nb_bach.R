library(e1071)
library(caret)

#============================================================
# 5-fold Naive Bayes
fold <- rep(1:5,each = ceiling(nrow(features)/5))
fold <- sample(fold,nrow(features))
mcr_i <- rep(NA,5)
for(i in 1:5){
  train <- features[which(fold !=i),]
  test <- features[which(fold ==i),]
  nb_mod <- naiveBayes(x = train[,-1], y = train[,1])
  x <- predict(nb_mod,test[,-1],type = "class")
  t <- table(x,test[,1])
  mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
}
mcr <- mean(mcr_i)
mcr



