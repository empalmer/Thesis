library(randomForest)

forest_mod <- randomForest(fcomposer~.,data = ffeatures,
                           mtry = 5,importance = T)
forest_mod

plot(forest_mod)
legend("top", colnames(forest_mod$err.rate),col=1:4,cex=0.8,fill=1:4)


## variable importance
pdf("varImp_f.pdf")
varImpPlot(forest_mod,type=2)
dev.off()


## with logistic: 
predicts <- predict(flog_lasso_mod,newx = xf,
                    type = 'coefficients',s = min_lambda)
scaled_coeffs <- predicts[-1,1]

rf_imp <- forest_mod$importance[,4]

imps <- data.frame(feature = names(scaled_coeffs),
                   log = abs(scaled_coeffs),
                   rf = abs(rf_imp))

imps$feature <- factor(names(scaled_coeffs),
                       levels = c("fsf_1","flen","fsf_2","rf8","fcons_dis",
                                  "fsf_5","fcons_perf","fsf_4","fcons_imp",
                                  "rf4d","rf2","fdens_sd","rf8d","fdens_mean",
                                  "fsf_7","fsf_6","rf4","rf2d","rf16","fsf_3","rf32"))

pdf("var_imp_rflog_f.pdf")
ggplot(melt(imps), aes(x = feature, y = value, fill = variable )) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90)) + ylim(0,4)
dev.off()


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

# predict 

forest_mod <- randomForest(ffeatures[,-1], y = ffeatures[,1],
                           mtry = 5,importance = T)
predict(forest_mod, newdata = disputed_features)
