#problem1
stocks <- read.csv("StocksCluster.csv")
str(stocks)
nrow(stocks)
prop.table(table(stocks$PositiveDec))
sum(stocks$PositiveDec) / nrow(stocks)
mean(stocks$PositiveDec)
cm <- cor(stocks[1:11])
diag(cm) <- -1 # making the diagonals as -1
max(cm)
summary(stocks)
cm <- colMeans(stocks[1:11])
which.max(cm)
which.min(cm)

#problem2
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family=binomial)
PredictTrain = predict(StocksModel, type="response")
cmat_LR <-table(stocksTrain$PositiveDec, PredictTrain > 0.5)
cmat_LR 
accu_LR <-(cmat_LR[1,1] + cmat_LR[2,2])/sum(cmat_LR)
accu_LR
PredictTest = predict(StocksModel, newdata=stocksTest, type="response")

#Then we compute the confusion matrix for the testing set using a threshold of 0.5
cmat_LR<-table(stocksTest$PositiveDec, PredictTest > 0.5)
cmat_LR
accu_LR <-(cmat_LR[1,1] + cmat_LR[2,2])/sum(cmat_LR)
accu_LR
sum(diag(cmat_LR))/nrow(stocksTest)
baseline<-table(stocksTest$PositiveDec)
baseline
accu_baseline <- max(baseline)/sum(baseline)
accu_baseline
baseline[2] / sum(baseline)

#problem3
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)
set.seed(144)
km <- kmeans(normTrain, centers=3, iter.max=1000)
km$size
table(km$cluster)
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)
sum(clusterTest == 2)

#problem4
stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)
stocksTrain11 <- split(stocksTrain, clusterTrain)
stocksTest11 <- split(stocksTest, clusterTest)
sapply(stocksTrain11, function(s){ mean(s$PositiveDec) })
StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family=binomial)
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family=binomial)
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family=binomial)
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)
stocksModels <- lapply(stocksTrain11, function(s){
glm(s$PositiveDec ~ ., family=binomial, data=s)
})
sapply(stocksModels, function(m){ m$coefficients })
PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type="response")
PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type="response")
PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type="response")
cmat1<-table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
cmat1
cmat2<-table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
cmat2
cmat3<-table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
cmat3
predictions <- sapply(1:3, function (i) {
p <- predict(stocksModels[[i]], newdata=stocksTest11[[i]], type="response")
(conf.mat <- table(stocksTest11[[i]]$PositiveDec, p > 0.5))
accuracy <- sum(diag(conf.mat)) / sum(conf.mat)
list(predict=p, accuracy=accuracy)
})
predictions
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
cmatoverall<-table(AllOutcomes, AllPredictions > 0.5)
cmatoverall
accu_overall<- (cmatoverall[1,1] + cmatoverall[2,2])/sum(cmatoverall)
accu_overall 





