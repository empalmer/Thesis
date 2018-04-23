
PCA <- prcomp(ffeatures[,-1],scale = T)
xs <- PCA$x[,1:11]
y <- ffeatures[,1] %>% unname() %>% as.character()

kn <- 0
for(k in 1:100){
  fold <- rep(1:5,each = ceiling(nrow(ffeatures)/5))
  fold <- sample(fold,nrow(ffeatures))
  fmcr_i <- rep(NA,12)
  for(j in 1:12){
    mcr_i <- rep(NA,5)
    for(i in 1:5){
      train <- xs[which(fold !=i),]
      test <- xs[which(fold ==i),]
      y <- ffeatures[which(fold !=i),1]
      yt <- ffeatures[which(fold == i),1]
      x <- knn(train,test,y, k = j)
      t <- table(x,yt)
      mcr_i[i] <- (sum(t) - sum(diag(t)))/sum(t)
    }
    fmcr_i[j] <- mean(mcr_i)
  }
  kn[k] <- which(fmcr_i == min(fmcr_i))
}
K <- 1 #(mode of kn)

fmcr <- 0
for(j in 1:100){
  fold <- rep(1:5,each = ceiling(nrow(ffeatures2)/5))
  fold <- sample(fold,nrow(ffeatures2))
  mcr_i <- rep(NA,5)
  for(i in 1:5){
    train <- xs[which(fold !=i),]
    test <- xs[which(fold ==i),]
    y <- ffeatures[which(fold !=i),1]
    yt <- ffeatures[which(fold == i),1]
    x <- knn(train,test,y, k = 6)
    t <- table(x,yt)
    mcr_i[i] <- (t[1,2]+t[2,1])/sum(t)
  }
  fmcr[j] <- mean(mcr_i)
}
MCR_knn_f <- mean(fmcr)
MCR_knn_f
