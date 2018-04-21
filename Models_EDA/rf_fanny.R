library(randomForest)

forest_mod <- randomForest(fcomposer~.,data = ffeatures,
                           mtry = 5,importance = T)
forest_mod

plot(forest_mod)
legend("top", colnames(forest_mod$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(forest_mod,type=2)


mcr <- 0
for(j in 1:100){
  fold <- rep(1:5,each = ceiling(nrow(ffeatures)/5))
  fold <- sample(fold,nrow(ffeatures))
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- which(fold !=i)
    test <- which(fold ==i)
    mod <- randomForest(fcomposer ~., data = ffeatures, subset = train,
                      mtry = 5,importance = T)
    p <- predict(mod,newdata = ffeatures[test,])
    mcr_i[i] <- mean(p != ffeatures[test,1])
  }
  mcr[j] <- mean(mcr_i)
}
MCR_rf <- mean(mcr)
MCR_rf
