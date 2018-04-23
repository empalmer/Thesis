library(glmnet)
library(boot)
library(plotmo)

library(ROCR)
source("features_bach_mendelssohn.R")

#Amelia::missmap()
#==============================================================
# 5 fold CV: w/o lasso

PCA <- prcomp(features[,-1],scale = T)

x <- PCA$x[,1:11]
y <- features[,1] %>% unname() %>% as.character()

#==============================================================
# LASSO cross validated
grid <- 10^seq(10,-2,length=100)
log_lasso_mod <- glmnet(x,y,family = "binomial", alpha = 1)
# 5-fold CV LASSO logistic.
cv_log_lasso_mod <- cv.glmnet(x,y,family = "binomial",
                              nfolds = 5,type.measure = "class")
pdf("pca_loglambda_b.pdf")
plot(cv_log_lasso_mod)
dev.off()

min_lambda <- cv_log_lasso_mod$lambda.min
min_lambda
log(min_lambda)

min_mse <- min(cv_log_lasso_mod$cvm)
min_mse


plot(log_lasso_mod, xvar = "lambda", xlim = c(-5, 0), main = "Lasso")

pdf("lasso_coef_b")
plot_glmnet(log_lasso_mod, xvar = "lambda", ylim =c(-4,3), label = T)
dev.off()


