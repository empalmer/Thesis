#======================================================
### ROC curve?
library(ROCR)
library(pROC)
library(plotROC)

# Logistic lasso works yes yay.
x <- model.matrix(composer ~.,features)[,-1]
y <- features[,1] %>% unname() %>% as.character()
log_fit <- glmnet(x,y,family = "binomial", alpha = 1)
probs <- predict(log_fit, s=min_lambda,newx = x, type="response")
pred <- prediction(probs,y)
perf <- performance(pred,measure = "tpr", x.measure = "fpr") 
plot(perf)

# Lda 
lda_mod <- lda(x = features[,-1], grouping = features[,1])
x <- predict(lda_mod,newdata = features[,-1])
lda.pred <- prediction(x$posterior[,2],features[,1])
lda.perf <- performance(lda.pred,"tpr","fpr")
ROCR::plot(lda.perf)


# Random Forests: 

forest_mod <- randomForest(composer~.,data = features,
                           mtry = 13, importance = T)
forest_mod.pr <- predict(forest_mod, newdata = features[,-1], type = "prob")
rf.pred <- prediction(forest_mod.pr[,2],features[,1])
rf.perf <- performance(rf.pred,"tpr","fpr")
ROCR::plot(rf.perf)

# knn 

knn_mod <- knn()

