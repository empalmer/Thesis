#======================================================
### ROC curve?
library(ROCR)
library(pROC)
library(plotROC)

fold <- rep(1:5,each = ceiling(nrow(features)/5))
fold <- sample(1:2,size = nrow(features), prob = c(.8,.2), replace = T)
train <- features[which(fold ==1),]
test <- features[which(fold ==  2),]

lab <- test[,1]


lda_mod <- lda(x = train[,-1], grouping = train[,1])
x <- predict(lda_mod,newdata =test[,-1])
lda.pred <- prediction(x$posterior[,2],lab)
lda.perf <- performance(lda.pred,"tpr","fpr")
plot(lda.perf)

forest_mod <- randomForest(composer~.,data = train,
                           mtry = 13, importance = T)
forest_mod.pr <- predict(forest_mod, newdata = test[,1])
rf.pred <- prediction(forest_mod.pr,lab)
rf.perf <- performance(rf.pred,"tpr","fpr")
plot(rf.perf)

adult.rf <-randomForest(income~.,data=data$train, mtry=2, ntree=1000,
                        keep.forest=TRUE, importance=TRUE,test=data$val)
adult.rf.pr = predict(adult.rf,type="prob",newdata=data$val)[,2]
#prediction is ROCR function
adult.rf.pred = prediction(adult.rf.pr, data$val$income)
#performance in terms of true and false positive rates
adult.rf.perf = performance(adult.rf.pred,"tpr","fpr")

#plot the curve
plot(adult.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")


predicts <- prediction(predicted, trues)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col=rainbow(10))


predicted <- forest_mod$predicted %>% unname() %>% as.numeric()
predicted <- predicted -1
trues <- features [,1] %>% as.numeric %>% as.factor
trues <- trues -1
df_rf <- data.frame(tru =  features[,1], pred = predicted)
ggplot(df_rf, aes(d = trues, m = predicted)) + geom_roc()


adult.rf.pr = predict(adult.rf,type="prob",newdata=data$val)[,2]
predicted <- forest_mod$predicted %>% unname() %>% as.numeric
true <- as.numeric(features[,1])
pred <- prediction(predicted, true)
perf = performance(pred,"tpr","fpr")
plot(perf,main="ROC Curve")
abline(a=0,b=1,lwd=2,lty=2,col="gray")

ggroc <- function(roc, showAUC = TRUE, interval = 0.2, breaks = seq(0, 1, interval)){
  require(pROC)
  if(class(roc) != "roc")
    simpleError("Please provide roc object from pROC package")
  plotx <- rev(roc$specificities)
  ploty <- rev(roc$sensitivities)

  ggplot(NULL, aes(x = plotx, y = ploty)) +
    geom_segment(aes(x = 0, y = 1, xend = 1,yend = 0), alpha = 0.5) +
    geom_step() +
    scale_x_reverse(name = "Specificity",limits = c(1,0), breaks = breaks, expand = c(0.001,0.001)) +
    scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = breaks, expand = c(0.001, 0.001)) +
    theme_bw() +
    theme(axis.ticks = element_line(color = "grey80")) +
    coord_equal() +
    annotate("text", x = interval/2, y = interval/2, vjust = 0, label = paste("AUC =",sprintf("%.3f",roc$auc)))
}


library(verification)

#' Functions plots multiple 'roc' objects into one plot
#' @param rocs
#'   A list of 'roc' objects. Every list item has a name.
#' @param breaks
#'   A vector of integers representing ticks on the x- and y-axis
#' @param legentTitel
#'   A string which is used as legend titel
ggrocs <- function(rocs, breaks = seq(0,1,0.1), legendTitel = "Legend") {
  if (length(rocs) == 0) {
    stop("No ROC objects available in param rocs.")
  } else {
    require(plyr)
    # Store all sensitivities and specifivities in a data frame
    # which an be used in ggplot
    RocVals <- plyr::ldply(names(rocs), function(rocName) {
      if(class(rocs[[rocName]]) != "roc") {
        stop("Please provide roc object from pROC package")
      }
      data.frame(
        fpr = rev(rocs[[rocName]]$specificities),
        tpr = rev(rocs[[rocName]]$sensitivities),
        names = rep(rocName, length(rocs[[rocName]]$sensitivities)),
        stringAsFactors = T
      )
    })

    aucAvg <- mean(sapply(rocs, "[[", "auc"))

    rocPlot <- ggplot(RocVals, aes(x = fpr, y = tpr, colour = names)) +
      geom_segment(aes(x = 0, y = 1, xend = 1,yend = 0), alpha = 0.5, colour = "gray") +
      geom_step() +
      scale_x_reverse(name = "False Positive Rate (1 - Specificity)",limits = c(1,0), breaks = breaks) +
      scale_y_continuous(name = "True Positive Rate (Sensitivity)", limits = c(0,1), breaks = breaks) +
      theme_bw() +
      coord_equal() +
      annotate("text", x = 0.1, y = 0.1, vjust = 0, label = paste("AUC =",sprintf("%.3f",aucAvg))) +
      guides(colour = guide_legend(legendTitel)) +
      theme(axis.ticks = element_line(color = "grey80"))

    rocPlot
  }
}

