library(randomForest)
library(caret)
library(mnormt)

forest_mod <- randomForest(composer~.,data = features,
                           mtry = 13, importance = T)


plot(forest_mod)
legend("top", colnames(forest_mod$err.rate),col=1:4,cex=0.8,fill=1:4)

pdf("varImp_b.pdf")
varImpPlot(forest_mod,type=2)
dev.off()

library(rpart)
fit=rpart(factor(Y)~., df)
plot(fit)
text(fit)

mcr <- 0
for(j in 1:100){
  fold <- rep(1:5,each = ceiling(nrow(features)/5))
  fold <- sample(fold,nrow(features))
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- which(fold !=i)
    test <- which(fold ==i)
    mod <- randomForest(composer ~., data = features, subset = train,
                    mtry = 5,importance = T)
    p <- predict(mod,newdata = features[test,])
  
    mcr_i[i] <- mean(p != features[test,1])
  }
  mcr[j] <- mean(mcr_i)
}
MCR_RF_b <- mean(mcr)
MCR_RF_b

