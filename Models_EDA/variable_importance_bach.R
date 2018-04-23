

forest_mod <- randomForest(composer~.,data = features,
                           mtry = 13, importance = T)
rf.var <- forest_mod$importance[,4]


log_lasso_mod <- glmnet(x,y,family = "binomial")
log.coef <- coef(log_lasso_mod,s = min_lambda)


