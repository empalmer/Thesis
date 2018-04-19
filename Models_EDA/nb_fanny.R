library(e1071)
library(caret)

#============================================================
# 5-fold Naive Bayes
fnb_full_mod <- naiveBayes(x = ffeatures[,-1],
                          y = ffeatures[,1])

fold <- rep(1:5,each = ceiling(nrow(ffeatures)/5))
fold <- sample(fold,nrow(ffeatures))
mcr_i <- rep(NA,5)
for(i in 1:5){
  train <- ffeatures[which(fold !=i),]
  test <- ffeatures[which(fold ==i),]
  nb_mod <- naiveBayes(x = train[,-1], y = train[,1])
  x <- predict(nb_mod,test[,-1],type = "class")
  t <- table(x,test[,1])
  mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
}
fmcr <- mean(mcr_i)
fmcr

# prediction on unknowns: 
x <- predict(fnb_full_mod,disputed_features,type = "class")
x

