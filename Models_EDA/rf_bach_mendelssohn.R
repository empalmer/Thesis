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

predicts <- predict(log_lasso_mod,newx = x,
                    type = 'coefficients',s = min_lambda)
scaled_coeffs <- predicts[-1,1]

rf_imp <- forest_mod$importance[,4]

imps <- data.frame(feature = names(scaled_coeffs),
                   log = abs(scaled_coeffs),
                   rf = abs(rf_imp))

imps$feature <- factor(names(scaled_coeffs),
                       levels = rev(c("dens_mean","dens_sd","rf16","sf_2",
                                  "rf32","rf4","sf_6","sf_1","cons_dis","sf_7",
                                  "sf_3","rf8",
                                    "rf4d", "sf_4", 
                                  "cons_perf","cons_imp","sf_5","rf2",
                                  "rf2d","rf8d","len")))
pdf("var_imp_rflog_b.pdf")
ggplot(melt(imps), aes(x = feature, y = value, fill = variable )) +
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip()
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

