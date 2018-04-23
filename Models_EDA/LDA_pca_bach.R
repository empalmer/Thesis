
library(MASS)
library(caret)

PCA <- prcomp(features[,-1],scale = T)

xs <- as.data.frame(PCA$x[,1:11])
y <- features[,1] %>% unname() %>% as.character()

mcr_lda <- 0
for(j in 1:100){
  fold <- rep(1:5,each = ceiling(nrow(features)/5))
  fold <- sample(fold,nrow(features))
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- xs[which(fold !=i),]
    test <- xs[which(fold ==i),]
    lda_mod <- lda(x = train, grouping = y[which(fold!=i)])
    x <- predict(lda_mod,newdata =test)
    t <- table(x$class,test[,1])
    mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
  }
  mcr_lda[j] <- mean(mcr_i)
}
MCR_LDA_PCA_b <- mean(mcr_lda)
MCR_LDA_PCA_b
