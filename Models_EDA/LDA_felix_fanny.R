source("~/Desktop/Thesis/Models_EDA/features_fanny_felix.R")
library(MASS)
library(caret)
library(e1071)

#==============================================================
#==============================================================
# LDA on feature space - currenlty running!
mcr_lda <- 0
for(j in 1:100){
  fold <- rep(1:5,each = ceiling(nrow(ffeatures)/5))
  fold <- sample(fold,nrow(ffeatures))
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- ffeatures[which(fold !=i),]
    test <- ffeatures[which(fold ==i),]
    lda_mod <- lda(x = train[,-1], grouping = train[,1])
    x <- predict(lda_mod,newdata =test[,-1])
    t <- table(x$class,test[,1])
    mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
  }
  mcr_lda[j] <- mean(mcr_i)
}
MCR_lda_f <- mean(mcr_lda)
MCR_lda_f


# predictions!

full_lda_mod <- lda(x = ffeatures[,-1],grouping = ffeatures[,1])
x <- predict(full_lda_mod, disputed_features)
x$cl
#==============================================================
#==============================================================
#==============================================================
#LDA on pcas: 
PCA <- prcomp(ffeatures[,-1], scale = T)
x <- PCA$x[,-12:13]
y <- as.factor(ffeatures[,1])
df <- cbind(y,x)
fold <- rep(1:5,each = ceiling(nrow(df)/5))
fold <- sample(fold,nrow(df))
mcr_i <- rep(NA,5)
for(i in 1:5){
  train <- df[which(fold !=i),]
  test <- df[which(fold ==i),]
  lda_mod <- lda(x = train[,-1], grouping = train[,1])
  x <- predict(lda_mod,newdata =test[,-1])
  t <- table(x$class,test[,1])
  mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
}
mcr_lda <- mean(mcr_i)
mcr_lda

# predictions... errors
full_pca <- lda(df[,-1],grouping = df[,1])

#==============================================================
#==============================================================
#==============================================================




### LDA run on 12 principal components for some reason
PCA <- prcomp(ffeatures[,-1], scale = T)
x <- PCA$x[,-13]
y <- as.factor(ffeatures[,1])
lda_mod <- lda(x = x, grouping = y)
# Leave one out CV: 
lda_mod_cv <- lda(x = x, grouping = y, CV = T)
#==============================================================
# LDA on 12 PCs with 5-fold CV
x <- x[sample(nrow(x)),]
folds <- cut(seq(1,nrow(ffeatures)),breaks=5,labels=FALSE)
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

#==============================================================
#==============================================================
#==============================================================
#==============================================================
# QDA
qda.fit <- qda(x = x, grouping = y)
## 5 fold CV
fold <- rep(1:5, each = ceiling(nrow(ffeatures)/5))
fold <- sample(fold, nrow(ffeatures))
ffeatures$fold <- fold
MSE_i <- rep(NA, 5)
for(i in 1:5){
  d_i <- which(ffeatures$fold != i)
  d_im <- which(ffeatures$fold == i)
  m_l <- qda(fcomposer ~ .,
             data = ffeatures,
             subset = d_i)
  pred_i <- predict(m_l,ffeatures[d_im,], type = "response")
  lda.class <- pred_i$class
  t <-table(lda.class,ffeatures[d_im,1])
  MSE_i[i] <- (t[1,2]+t[2,1])/sum(t)
}
lda_MSE_k <- mean(MSE_i)
lda_MSE_k

#==============================================================
#==============================================================
#==============================================================
#==============================================================
# Naieve Bayes

nb <- naiveBayes(x,y)
