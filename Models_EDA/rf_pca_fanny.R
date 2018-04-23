

PCA <- prcomp(ffeatures[,-1],scale = T)
xs <- PCA$x[,1:11]
y <- ffeatures[,1] %>% unname() %>% as.character()


mcr <- 0
for(j in 1:100){
  fold <- rep(1:5,each = ceiling(nrow(ffeatures)/5))
  fold <- sample(fold,nrow(ffeatures))
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- xs[which(fold !=i),]
    test <- xs[which(fold ==i),]
    y <- ffeatures[which(fold !=i),1]
    yt <- ffeatures[which(fold ==i),1]
    mod <- randomForest(train,y,
                        mtry = 5,importance = T)
    p <- predict(mod,newdata = test)
    mcr_i[i] <- mean(p != yt)
  }
  mcr[j] <- mean(mcr_i)
}
MCR_rf <- mean(mcr)
MCR_rf
