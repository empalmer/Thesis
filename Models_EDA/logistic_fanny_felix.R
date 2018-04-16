source("~/Desktop/Thesis/Models_EDA/features_fanny_felix.R")
library(glmnet)
library(boot)
#==============================================================
# 5 fold CV: w/o lasso
xf <- model.matrix(fcomposer ~.,ffeatures)[,-1]
yf <- ffeatures[,1] %>% unname() %>% as.character()
# full data set
flog_mod <- glm(fcomposer ~.,data = ffeatures,family = "binomial")
# 5-fold logistic regresssion 
fcv_log_mod <- cv.glm(data = ffeatures, flog_mod ,K = 5 )
fMSE_log <- fcv_log_mod$delta[2]
fMSE_log

#==============================================================
# LASSO cross validated
grid <- c(10^seq(10,-2,length=100),0)
flog_lasso_mod <- glmnet(xf,yf,family = "binomial", alpha = 0,lambda = grid)
# 5-fold CV LASSO logistic. 


fcv_log_lasso_mod <- cv.glmnet(xf,yf,family = "binomial",
                              nfolds = 5,type.measure = "mse")
plot(fcv_log_lasso_mod)
min_lambda <- fcv_log_lasso_mod$lambda.min
MSE_lasso <- fcv_log_lasso_mod$cvm[which(fcv_log_lasso_mod$cvm == min(fcv_log_lasso_mod$cvm))]
MSE_lasso

fcv_log_mod <- cv.glm(data = ffeatures, flog_mod, K = 5 )
fMSE_log <- fcv_log_mod$delta[2]
fMSE_log






