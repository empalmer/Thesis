library(glmnet)
library(boot)
library(plotmo)

library(ROCR)
source("features_bach_mendelssohn.R")

#Amelia::missmap()
#==============================================================
# 5 fold CV: w/o lasso
x <- model.matrix(composer ~.,features)[,-1]
y <- features[,1] %>% unname() %>% as.character()
# full data set
log_mod <- glm(composer ~.,data = features,family = "binomial")
# 5-fold logistic regresssion
cv_log_mod <- cv.glm(data = features, log_mod ,K = 5 )
MSE_log <- cv_log_mod$delta[1]
MSE_log

#==============================================================
# LASSO cross validated
grid <- 10^seq(10,-2,length=100)
log_lasso_mod <- glmnet(x,y,family = "binomial", alpha = 1)
# 5-fold CV LASSO logistic. 
cv_log_lasso_mod <- cv.glmnet(x,y,family = "binomial",
                              nfolds = 5,type.measure = "class")
pdf("loglambda_b.pdf")
plot(cv_log_lasso_mod)
dev.off()


# extract coefficients: for variable importance
predicts <- predict(log_lasso_mod,newx = x,
                    type = 'coefficients',s = min_lambda)
scaled_coeffs <- scale(predicts[,1], center = T, scale = T)[,1]
#

min_lambda <- cv_log_lasso_mod$lambda.min
min_lambda

min_mse <- min(cv_log_lasso_mod$cvm)
min_mse


plot(log_lasso_mod, xvar = "lambda", xlim = c(-5, 0), main = "Lasso")

pdf("lasso_coef_b.pdf")
plot_glmnet(log_lasso_mod, xvar = "lambda", ylim =c(-35,35), label = T)
dev.off()





# #==============================================================
# # LASSO logistic
# lasso.cv.out <- cv.glmnet(x, y,family = "binomial", alpha = 1)
# lasso.bestlam <- lasso.cv.out$lambda.min
# 
# lasso.pred <- predict(lasso.mod, s = lasso.bestlam,
#                       newx = test, type = "response")
# lass <- rep("bach",length(y.test))
# lass[lasso.pred > .5] <- "mendelssohn"
# t <- table(lass,y.test)
# MSE_lasso <- (t[1,2]+t[2,1])/sum(t)
# MSE_lasso
# 
# plot(lasso.mod, xvar = "lambda", xlim = c(-5, 0), main = "Lasso")
# plot_glmnet(lasso.mod, xvar = "lambda",xlim = c(-5,0))

# #==============================================================
# # 5 fold CV for logistic
# set.seed(1)
# fold <- rep(1:5, each = ceiling(nrow(features)/5))
# fold <- sample(fold, nrow(features))
# features2 <- features
# features$fold <- fold
# 
# MSE_i <- rep(NA, 5)
# for(i in 1:5){
#   d_i <- which(features$fold != i)
#   d_im <- which(features$fold == i)
#   m_l <- glm.fit <- glm(composer ~ dens_mean,
#                         data = features, family = binomial,
#                         subset = d_i)
#   pred_i <- predict(m_l,features[d_im,], type = "response")
#   glm.pred <- rep("bach",length(d_im))
#   glm.pred[pred_i > .5] = "mendelssohn"
#   t <-table(glm.pred,features[d_im,1])
#   MSE_i[i] <- (t[1,2]+t[2,1])/sum(t)
# }
# logistic_MSE_k <- mean(MSE_i)
# logistic_MSE_k
# 
# #==============================================================