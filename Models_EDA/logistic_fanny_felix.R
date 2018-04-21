source("~/Desktop/Thesis/Models_EDA/features_fanny_felix.R")
library(glmnet)
library(boot)
library(plotmo)
#==============================================================
# lasso self code 
grid <- 10^seq(10,-2,length=100)
fold <- rep(1:5,each = ceiling(nrow(ffeatures)/5))
fold <- sample(fold,nrow(ffeatures))
for(j in 1:length(grid)){
  train <- ffeatures[which(fold !=i),]
  test <- ffeatures[which(fold ==i),]
  mod <- cv.glmnet(train[,-1],train[1],alpha = 1)
  
  best_lambda <
}



# Logistic cv missclasification rate: 
fmcr <- 0
for(j in 1:100){
  fold <- rep(1:5,each = ceiling(nrow(ffeatures)/5))
  fold <- sample(fold,nrow(ffeatures))
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- ffeatures[which(fold !=i),]
    test <- ffeatures[which(fold ==i),]
    mod <- glm(train[,-1],as.factor(train[,1]), family = "binomial")
    x <- predict(mod,test[,-1],type = "response")
    x_class <- ifelse(x >.5,"felix","fanny")
    t <- table(x_class,test[,1])
    mcr_i[i] <- (sum(t) - sum(diag(t)))/sum(t)
  }
  fmcr[j] <- mean(mcr_i)
}
MCR_log_f <- mean(fmcr)
MCR_log_f

#==============================================================
# 
# pca data
PCAf <- prcomp(ffeatures[,-1], scale = T)
f_pcax <- PCAf$x[,-13]
fy <- as.factor(features[,1])
pca_df <- as.data.frame(cbind(compos = yf,f_pcax))
x_pca <- model.matrix(compos~.,pca_df)
# normal data - as model matrix for lasso 
xf <- model.matrix(fcomposer ~.,ffeatures)[,-1]
yf <- ffeatures[,1]

# 5 fold CV: w/o lasso

flog_mod <- glm(fcomposer ~.,data = ffeatures,family = "binomial")
fcv_log_mod <- cv.glm(data = ffeatures, flog_mod ,K = 5 )
fMSE_log <- fcv_log_mod$delta[1]
fMSE_log

dis <- predict(flog_mod,type= "response")

#==============================================================
# 5-fold CV LASSO logistic
grid <- 10^seq(10,-2,length=100)
flog_lasso_mod <- glmnet(xf,yf,family = "binomial", 
                         alpha = 1,lambda = grid)
fcv_log_lasso_mod <- cv.glmnet(xf,yf,family = "binomial",
                              nfolds = 5,type.measure = "class")
min_lambda <- fcv_log_lasso_mod$lambda.min
fcv_log_lasso_mod$cvm
MSE_lasso <- min(fcv_log_lasso_mod$cvm)
MSE_lasso

# lambda plot
plot(fcv_log_lasso_mod)
# coeficient plots
plot(flog_lasso_mod, xvar = "lambda", xlim = c(-5, 0), main = "Lasso")
plot_glmnet(flog_lasso_mod, xvar = "lambda",xlim = c(-5,0))

#==============================================================
# predicting pieces from lasso model...
df <- rbind(xf,as.matrix(disputed_features))
train <- 1:nrow(xf)
test <- (nrow(xf)+1):nrow(df)
best_lam_mod <- glmnet(df[train,],yf[train],family = "binomial",alpha = 1)
predicts <- predict(best_lam_mod,newx = df[test,],
                   type = 'class',s = min_lambda)
predicts


#ROCR Curve
ROCRpred <- prediction(predict, dresstrain$Recommended)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))




