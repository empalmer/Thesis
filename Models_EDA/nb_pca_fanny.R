
PCA <- prcomp(ffeatures[,-1],scale = T)
xs <- PCA$x[,1:11]
y <- ffeatures[,1] %>% unname() %>% as.character()


fmcr <- 0
for(j in 1:100){
  fold <- rep(1:5,each = ceiling(nrow(ffeatures)/5))
  fold <- sample(fold,nrow(ffeatures))
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- xs[which(fold !=i),]
    test <- xs[which(fold ==i),]
    y <- ffeatures[which(fold !=i),1]
    yt <- ffeatures[which(fold ==i),1]
    nb_mod <- naiveBayes(x = train, y = y)
    x <- predict(nb_mod,test,type = "class")
    t <- table(x,yt)
    mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
  }
  fmcr[j] <- mean(mcr_i)
}
MCR_nb_f <- mean(fmcr)
MCR_nb_f
