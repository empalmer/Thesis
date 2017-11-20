######Attribution ICMPC Code
####Graphing
library(devtools)
#install_github("ggbiplot", "vqv")
library(gmodels)

library(ggbiplot)
# load the "class" library
library(class)
setwd("~/Dropbox/Research/CurrentProjects/josquin_authorship/ICMPC_Talk/")
complete_data <- read.csv("attribution_data_new.csv", na.strings=c("","NA"), header=T)
complete_data <- complete_data[,-62]

###subset the data by analysis level

# Josquin attribution level 1 and palestrina

josquin <- complete_data[complete_data$Composer == 'Josquin des Prez',-12]

josquin_secure <- josquin[josquin$Attribution.Level <= 2 ,]
josquin_secure$Composer <- as.character(josquin_secure$Composer)
josquin_less_secure <- josquin[ josquin$Attribution.Level >= 3,]


####Other composers
bach <- complete_data[complete_data$Composer == "Bach_Johann Sebastian",-12]
larue <- complete_data[complete_data$Composer == "la Rue_Pierre de",-12]
palestrina <- complete_data[complete_data$Composer == "Palestrina_Giovanni Perluigi da",-12]
ockeghem <- complete_data[complete_data$Composer == "Johannes Ockeghem",-12]
orto <- complete_data[complete_data$Composer == "de Orto_Marbrianus",-12]
dufay <- complete_data[complete_data$Composer == "Du Fay_Guillaume",-12]

josquin_bach <- rbind(josquin_secure, bach)
josquin_palestrina <- rbind(josquin_secure, palestrina)
josquin_larue <- rbind(josquin_secure, larue)

######PCA on both entire josquin level 1 and palestrina.
#columns_wanted <- c(7,9)
####5:58 is everything, 5:11 is everything but note-to-note transitions.




comparison <- rbind(josquin_secure, bach)

####looking at treatment of the tritone
  
columns_wanted <- c(5:11)  
Matrix <- comparison[,columns_wanted]
Matrix <- as.matrix(Matrix)
Matrix[is.na(Matrix)] <- 0
# log.pieces <- log(Matrix)
log.pieces <- log(Matrix)
composer <- comparison[,1]
  
####principle component analysis.

pieces.pca <- prcomp(Matrix,
                 center = TRUE,
                 scale. = TRUE) 
print(pieces.pca)
plot(pieces.pca, type = "l", main="Principal Components Analysis")
summary(pieces.pca)
  

g <- ggbiplot(pieces.pca, obs.scale = 1, var.scale = 1, 
              groups = composer, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
# 
# seven_component_model <- data.frame(pieces.pca$x[,1:8])

#####Other Graphing
require(ggplot2)

theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle,aes(x,y)) + geom_path()

loadings <- data.frame(pieces.pca$rotation, 
                       .names = row.names(pieces.pca$rotation))
p + geom_text(data=loadings, 
              mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")


#####Cleaning

josq_loadings <- pieces.pca$x[,1:27]
josq_kmeans <- cbind(comparison$Composer,josq_loadings)
josq_kmeans <- as.data.frame(josq_kmeans)
data <- josq_kmeans
table(data$V1)
####K-Means on half of each.


str(josq_kmeans)
# drop the id feature

#data <- data[,-1]

table(data$PC1)

###Created Training and Testing Data 
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7,0.3))
trainData <- data[ind==1,]
testData <- data[ind==2,]
trainDataX <- trainData[,-1]
testDataX <- testData[,-1]


data_train_labels <- trainData[,1]
data_test_labels <- testData[,1]

## Step 3: Training a model on the data ----


data_test_pred <- knn(train = trainDataX, test= testDataX, 
                      cl = data_train_labels, k=2)

## Step 4: Evaluating model performance ----
# Create the cross tabulation of predicted vs. actual

CrossTable(x = data_test_labels, y = data_test_pred,
           prop.chisq=FALSE)



###########################################################################
###### SVM #############
###########################################################################
# comparison <- rbind(josquin_secure, bach)
# my_columns <- c(1,5:11)
# Matrix <- comparison[,my_columns]
# Matrix <- as.matrix(Matrix)
# Matrix[is.na(Matrix)] <- 0
# log.pieces <- log(as.matrix(Matrix))
# composer <- comparison[,1]

###principle component analysis.
# 
# pieces.pca <- prcomp(Matrix,
#                      center = TRUE,
#                      scale. = TRUE) 
# print(pieces.pca)
# plot(pieces.pca, type = "l", main="Principal Components Analysis")
# summary(pieces.pca)

############################SVM ######################
## SVM on PCA Loadings from Josquin vs Dufay 



composers <- comparison[,1]
composers <- as.matrix(composers)
composersLoads <- pieces.pca$x[,1:27]
composersLoads <- as.data.frame(composersLoads)
toUse <- cbind(composers,composersLoads)


ind <- sample(2, nrow(pieces.pca$x), replace=TRUE, prob=c(0.8,0.2))
trainData <- toUse[ind==1,]
testData <- toUse[ind==2,]
tempTest <- testData[,-1]
#data_train_labels <- trainData[,1]
#data_test_labels <- testData[,1]

library(kernlab)

my_classifier <- ksvm(composers ~ ., data=trainData, kernel="vanilladot")
my_prediction <- predict(my_classifier, tempTest)
my_prediction
head(my_prediction)

###What's up with this?
table(my_prediction, testData$composer)

agreement <- my_prediction == testData$composer

table(agreement)
prop.table(table(agreement))

my_classifier_rbf <- ksvm(composers ~ ., data = trainData, kernel = "rbfdot")
composer_predictions_rbf <- predict(my_classifier_rbf, testData)
agreement_rbf <- composer_predictions_rbf == testData$composers
table(agreement_rbf)
prop.table(table(agreement_rbf))


CrossTable(x =testData$composers, y = composer_predictions_rbf,
           prop.chisq=FALSE)



##### Party Package Machine Learning 
#install.packages("party")
library(party)
partyTree <- composers ~ 
composer_ctree <- ctree(partyTree, data=trainDataX)
table(predict(composer_ctree), trainData$composers)
plot(composer_ctree)
plot(composer_ctree, type = "simple")

decision_tree <- ctree(as.factor(Composer) ~ nPVI_Entire + pitch_correlation + pitch_entropy + parallel_motion + similar_motion + oblique_motion + t1_1 + t1_2 + t1_3 + t1_4 + t1_5 + t1_6 + t1_7 + t2_1 + t2_2 + t2_3 + t2_4 + t2_5 + t2_6 + t2_7 + t3_1 + t3_2 + t3_3 + t3_4 + t3_5 + t3_6 + t3_7 + t4_1 + t4_2 + t4_3 + t4_4 + t4_5 + t4_6 + t4_7 + t5_1 + t5_2 + t5_3 + t5_4 + t5_5 + t5_6 + t5_7 + t6_1 + t6_2 + t6_3 + t6_4 + t6_5 + t6_6 + t6_7 + t7_1 + t7_2 + t7_3 + t7_4 + t7_5 + t7_6 + t7_7, data = comparison)
plot(decision_tree, type="simple")

)
> Fmla = clm ~ veh_value + veh_body + veh_age + gender + area + agecat
> TreeModel = ctree(Fmla, data = carFrame)
> plot(TreeModel, type="simple‚"Äù)


########### Ensemble Model/Andrew WIP
setwd("C://Users/Andrew/Desktop/R Things/Josquin")
complete_data <- read.csv("attribution_data_new.csv")
complete_data <- complete_data[,-62]

# Load Libraries
library(mlbench)
library(ggplot2)
library(caret)
library(caretEnsemble)

# Josquin attribution level 1 and palestrina

josquin <- complete_data[complete_data$Composer == 'Josquin des Prez',-12]

josquin_secure <- josquin[josquin$Attribution.Level <= 2 ,]
josquin_secure$Composer <- as.character(josquin_secure$Composer)
josquin_less_secure <- josquin[ josquin$Attribution.Level >= 3,]


####Other composers
bach <- complete_data[complete_data$Composer == "Bach_Johann Sebastian",-12]
larue <- complete_data[complete_data$Composer == "la Rue_Pierre de",-12]
palestrina <- complete_data[complete_data$Composer == "Palestrina_Giovanni Perluigi da",-12]
ockeghem <- complete_data[complete_data$Composer == "Johannes Ockeghem",-12]
orto <- complete_data[complete_data$Composer == "de Orto_Marbrianus",-12]
dufay <- complete_data[complete_data$Composer == "Du Fay_Guillaume",-12]

josquin_bach <- rbind(josquin_secure, bach)
josquin_palestrina <- rbind(josquin_secure, palestrina)
josquin_larue <- rbind(josquin_secure, larue)

# C5.0 & GBM
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# C5.0
set.seed(seed)
fit.c50 <- train(Class~., data=josquin_bach, method="C5.0", metric=metric, trControl=control)
# Stochastic Gradient Boosting
set.seed(seed)
fit.gbm <- train(Class~., data=josquin_bach, method="gbm", metric=metric, trControl=control, verbose=FALSE)
# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)

