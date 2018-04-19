library(MASS)
library(caret)
source("~/Desktop/Thesis/Models_EDA/features_fanny_felix.R")
#==============================================================
#==============================================================
# LDA on feature space - currenlty running!
fold <- rep(1:5,each = ceiling(nrow(features)/5))
fold <- sample(fold,nrow(features))
mcr_i <- rep(NA,5)
for(i in 1:5){
  train <- features[which(fold !=i),]
  test <- features[which(fold ==i),]
  lda_mod <- lda(x = train[,-1], grouping = train[,1])
  x <- predict(lda_mod,newdata =test[,-1])
  t <- table(x$class,test[,1])
  mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
}
mcr_lda <- mean(mcr_i)
mcr_lda

#==============================================================
#==============================================================
### LDA run on 12 principal components for some reason
PCA <- prcomp(features[,-1], scale = T)
x <- PCA$x[,-13]
y <- as.factor(features[,1])
lda_mod <- lda(x = x, grouping = y)

# LDA on 12 PCs with 5-fold CV
x <- x[sample(nrow(x)),]
folds <- cut(seq(1,nrow(features)),breaks=5,labels=FALSE)
MSE_i <- rep(0,5)
for(i in 1:5){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- x[testIndexes, ]
  trainData <- x[-testIndexes, ]
  ytest <- y[testIndexes]
  ytrain <- y[-testIndexes]
  lda_mod <- lda(x = trainData, grouping = ytrain)
  lda_pred <- predict(lda_mod,testData)
  lda_class <- lda_pred$class
  t <-table(lda_class,ytest)
  MSE_i[i] <- (t[1,2]+t[2,1])/sum(t)
}
MSE_lda <- mean(MSE_i)
MSE_lda


## 5 fold CV
fold <- rep(1:5, each = ceiling(nrow(features)/5))
fold <- sample(fold, nrow(features))
features$fold <- fold

MSE_i2 <- rep(NA, 5)
for(i in 1:5){
  d_i <- which(features$fold != i)
  d_im <- which(features$fold == i)
  m_l <- lda(composer ~ .,
             data = features,
             subset = d_i)
  pred_i <- predict(m_l,features[d_im,], type = "response")
  lda.class <- pred_i$class
  t <-table(lda.class,features[d_im,1])
  MSE_i[i] <- (t[1,2]+t[2,1])/sum(t)
}
lda_MSE_k <- mean(MSE_i2)
lda_MSE_k


#==============================================================