source("features_fanny_felix")



#==============================================================
# PCA 
fPCA <- prcomp(ffeatures[,-1], scale = T)
biplot(fPCA,scale = 0)

# Skree plot
d <- data.frame(PC = 1:13,
                PVE = PCA$sdev^2 / sum(PCA$sdev^2))
ggplot(d, aes(x = PC, y = PVE)) +
  geom_line() + 
  geom_point()


#==============================================================
#==============================================================
# Logistic regression
#Warning messages:
#  1: glm.fit: algorithm did not converge 
#2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
glm.fit <- glm(fcomposer ~ ., data = ffeatures, family = binomial,
               subset = values)
summary(glm.fit)
#==============================================================
# LASSO logistic

x <- model.matrix(fcomposer~.,ffeatures)[,-1]
y <- ffeatures[,1] %>% unname() %>% as.character

grid <- 10^(seq(10,-2,length = 100))
lasso.mod <- glmnet(x,y,family = "binomial",alpha = 1, lambda = grid)

train <- sample(1:nrow(x),nrow(x)/2)
test <- as.matrix(x[-train,])
y.test <- y[-train]
y.train <- y[train] %>% unname () %>% as.character()

lasso.mod <- glmnet(x[train,],y.train, family = "binomial" ,alpha= 1,
                    lambda = grid, thresh = 1e-12)
lasso.cv.out <- cv.glmnet(x, y,family = "binomial", alpha = 1)
lasso.bestlam <- lasso.cv.out$lambda.min

lasso.pred <- predict(lasso.mod, s = lasso.bestlam,
                      newx = test, type = "response")
lass <- rep("bach",length(y.test))
lass[lasso.pred > .5] <- "mendelssohn"
t <- table(lass,y.test)
MSE_lasso <- (t[1,2]+t[2,1])/sum(t)
MSE_lasso

plot(lasso.mod, xvar = "lambda", xlim = c(-5, 0), main = "Lasso")
plot_glmnet(lasso.mod, xvar = "lambda",xlim = c(-5,0))

#==============================================================
# 5 fold CV for logistic
set.seed(1)
fold <- rep(1:5, each = ceiling(nrow(ffeatures)/5))
fold <- sample(fold, nrow(ffeatures))
ffeatures$fold <- fold

MSE_i <- rep(NA, 5)
for(i in 1:5){
  d_i <- which(ffeatures$fold != i)
  d_im <- which(ffeatures$fold == i)
  m_l <- glm.fit <- glm(fcomposer ~ .,
                        data = ffeatures, family = binomial,
                        subset = d_i)
  pred_i <- predict(m_l,features[d_im,], type = "response")
  glm.pred <- rep("felix",length(d_im))
  glm.pred[pred_i > .5] = "fanny"
  t <-table(glm.pred,features[d_im,1])
  MSE_i[i] <- (t[1,2]+t[2,1])/sum(t)
}
logistic_MSE_k <- mean(MSE_i)
logistic_MSE_k

#==============================================================
### LDA
xPC <- fPCA$x %>% as.data.frame
PCAfit <- cbind(composer =ffeatures$fcomposer,xPC)

test <- sample(1:40,10)
train <- PCAfit[-test,]
lda.fit <- lda(composer ~., data = train)
plot(lda.fit)
lda.pred <- predict(lda.fit,test)
lda.class <- lda.pred$class
table(lda.class,test$fcomposer)

## 5 fold CV
fold <- rep(1:5, each = ceiling(nrow(ffeatures)/5))
fold <- sample(fold, nrow(ffeatures))
ffeatures$fold <- fold

MSE_i <- rep(NA, 5)
for(i in 1:5){
  d_i <- which(ffeatures$fold != i)
  d_im <- which(ffeatures$fold == i)
  m_l <- lda(fcomposer ~ .,
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


