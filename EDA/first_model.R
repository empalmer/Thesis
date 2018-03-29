library(MASS)
library(class)
library(tree)
library(randomForest)
library(e1071)
library(ggplot2)

# get all data
mendelssohn <- c(felix,fanny)
data <- c(bach,felix,fanny)
name<- c(rep("bach",length(bach)),rep("felix",length(felix)),
         rep("fanny",length(fanny)))
data <- rbind(name,data)                

composer <- c(rep("bach",length(bach)),rep("mendelssohn",length(mendelssohn)))

# Compute feature: mel ints
mel_intsb <- map(bach,mel_ints,"piano") # each 
mel_intsf <- map(mendelssohn,mel_ints,"V")


# Compute feature: connsonance
cons_b <- map(bach,consonances,"piano")
cons_m <- map(mendelssohn,consonances,"V")

cons_perf <- c(map(cons_b,1),map(cons_m,1)) %>% unlist()
cons_imp <- c(map(cons_b,2),map(cons_m,2)) %>% unlist()
cons_dis <- c(map(cons_b,3),map(cons_m,3)) %>% unlist()

# Compute features: density
dens_b <- map(bach,beat_density)
dens_m <- map(mendelssohn,beat_density) # mean then sd

dens_mean <- c(map(dens_b,1),map(dens_m,1)) %>% unlist()
dens_sd <- c(map(dens_b,2),map(dens_m,2)) %>% unlist()

# Compute features: scale degree freq

sf_b <- map(bach,scale_degree_freq) %>% unname()
sf_m <- map(mendelssohn,scale_degree_freq)
sf_freqs <- c(map(sf_b,3) ,map(sf_m,3))
sf_1 <- map(sf_freqs,1) %>% unlist
sf_2 <- map(sf_freqs,2) %>% unlist
sf_3 <- map(sf_freqs,3)%>% unlist
sf_4 <- map(sf_freqs,4)%>% unlist
sf_5 <- map(sf_freqs,5)%>% unlist
sf_6 <- map(sf_freqs,6)%>% unlist
sf_7 <- map(sf_freqs,7)%>% unlist

# FEATURES!!! 

features <- data.frame(composer,
                       cons_dis,cons_imp,cons_perf,
                       dens_mean,dens_sd,
                       sf_1,sf_2,sf_3,sf_4,sf_5,sf_6,sf_7)
features2 <- features[,-c(4)]
values <- sample(1:54,40)
test <- features[-values,]
train <- features[values,]
#==============================================================
# Logistic regression
#Warning messages:
#  1: glm.fit: algorithm did not converge 
#2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
glm.fit <- glm(composer ~ dens_mean, data = features, family = binomial,
               subset = values)
summary(glm.fit)
glm.probs <- predict(glm.fit, type = "response")
glm.probs

glm.probs <- predict(glm.fit,test,type = "response")
glm.probs
glm.pred <- rep("bach",14)
glm.pred[glm.probs > .5] = "mendelssohn"
table(glm.pred,test[,1])
msclas_logistic <- 2/14

#==============================================================
### LDA
lda.fit <- lda(composer ~., data = train)
plot(lda.fit)
lda.pred <- predict(lda.fit,test)
lda.class <- lda.pred$class
table(lda.class,test$composer)
msclas_lda <- 0


#==============================================================
### QDA 

qda.fit <- qda(composer ~ dens_mean, data = features, subset = values)
#Error in qda.default(x, grouping, ...) : rank deficiency in group bach
qda.fit
qda.class <- predict(qda.fit, test)
table(qda.class$class,test[,1])

msclas_qda <- 1/14
#==============================================================
## KNN

a <- train[,-1]
b <- test[,-1]
c <- train[,1]

knn.pred <- knn(a,b,c,k=2)
table(knn.pred,test[,1])
msclas_knn <- 1/14

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
biplot(pr.out,scale = 0)



#==============================================================
# K- means clustering

km2 <- kmeans(features[,-1],2, nstart = 20)

d <- data.frame(PC1 = PCA$x[, 1],
                PC2 = PCA$x[, 2],
                cluster = as.factor(km2$cluster),
                compower = features[,1])

ggplot(d, aes(x = PC1, y = PC2, col = cluster)) +
  geom_point() +
  geom_text(aes(label = composer), vjust = 2)






