
PCA <- prcomp(ffeatures[,-1],scale = T)
xs <- PCA$x[,1:11]
y <- ffeatures[,1] %>% unname() %>% as.character()

mcr_lda <- 0
for(j in 1:100){
  fold <- rep(1:5,each = ceiling(nrow(ffeatures)/5))
  fold <- sample(fold,nrow(ffeatures))
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- xs[which(fold !=i),]
    test <- xs[which(fold ==i),]
    y <- ffeatures[which(fold !=i),1]
    yt <- ffeatures[which(fold ==i),1]
    lda_mod <- lda(x = train, grouping = y)
    x <- predict(lda_mod,newdata =test)
    t <- table(x$class,yt)
    mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
  }
  mcr_lda[j] <- mean(mcr_i)
}
MCR_lda_f <- mean(mcr_lda)
MCR_lda_f
