library(museR,MASS,class,tree,randomForest,e1071,ggplot2,
        GGally,ISLR,boot,glmnet,caret,plotmo)

source("features_bach_mendelssohn.R")

#==============================================================
### LDA

lda.fit <- lda(composer ~., data = train)
plot(lda.fit)
lda.pred <- predict(lda.fit,test)
lda.class <- lda.pred$class
table(lda.class,test$composer)

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
### QDA 

qda.fit <- qda(composer ~ dens_mean, data = features, subset = values)
#Error in qda.default(x, grouping, ...) : rank deficiency in group bach
qda.fit
qda.class <- predict(qda.fit, test)
table(qda.class$class,test[,1])

### QDA 5-fold CV
for(i in 1:5){
  d_i <- which(features$fold != i)
  d_im <- which(features$fold == i)
  m_l <- qda(composer ~ dens_mean,
                        data = features,
                        subset = d_i)
  pred_i <- predict(m_l,features[d_im,], type = "response")
  t <-table(pred_i$class,features[d_im,1])
  MSE_i[i] <- (t[1,2]+t[2,1])/sum(t)
}
qda_MSE_k <- mean(MSE_i)
qda_MSE_k

#==============================================================
## KNN with k = 2

a <- train[,-1]
b <- test[,-1]
c <- train[,1]

knn.pred <- knn(a,b,c,k=2)
table(knn.pred,test[,1])

## KNN 5-fold CV 
for(i in 1:5){
  d_i <- which(features$fold != i)
  d_im <- which(features$fold == i)
  a_i <- features[d_i,-1]
  b_i <- features[d_im,-1]
  c_i <- features[d_i,1]
  m_l <- knn(a_i,b_i,c_i,k = 2)
  t <-table(m_l,features[d_im,1])
  MSE_i[i] <- (t[1,2]+t[2,1])/sum(t)
}
knn_MSE_k <- mean(MSE_i)
knn_MSE_k

#==============================================================
## Decision trees 

treeeees <- tree(composer~.,features)
summary(treeeees)
plot(treeeees)

# bag
tres2 <- randomForest(composer~.,train,mtry = 12, importance = TRUE)
tres2

yhat.tres <- predict(tres2,test)
table(yhat.tres,test[,1])

#==============================================================
# SVM

dat <- cbind(y=as.factor(train[,1]),train[,-1])
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 10, scale = F)

summary(svmfit)

tune.out <- tune(svm, y~., data = dat, kernel = "linear",
                 ranges = list(cost = c(.001,.01,.1,1,5,10,100)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)

preds <- predict(bestmod,test[,-1])

table(predict = preds, truth = test[,1])
mis_class_svm <- 1/14
  
#==============================================================
# PCA 

PCA <- prcomp(features[,-1], scale = T)
biplot(PCA,scale = 0)

d <- data.frame(PC = 1:12,
                PVE = PCA$sdev^2 / sum(PCA$sdev^2))
ggplot(d, aes(x = PC, y = PVE)) +
  geom_line() + 
  geom_point()



#==============================================================
# K- means clustering

km2 <- kmeans(features[,-1],2, nstart = 20)

d <- data.frame(PC1 = PCA$x[, 1],
                PC2 = PCA$x[, 2],
                cluster = as.factor(km2$cluster),
                composer = features[,1])

ggplot(d, aes(x = PC1, y = PC2, col = cluster)) +
  geom_point() +
  geom_text(aes(label = composer), vjust = 2)

x <- PCA$x
km_pca2 <- kmeans(x,2,nstart = 20)
d <- data.frame(PC1 = PCA$x[, 1],
                PC2 = PCA$x[, 2],
                cluster = as.factor(km_pca2$cluster),
                composer = features[,1])

ggplot(d, aes(x = PC1, y = PC2, col = cluster)) +
  geom_point() +
  geom_text(aes(label = composer), vjust = 2)



