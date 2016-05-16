census = read.csv("census.csv")

#problem1
library(caTools)
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl == TRUE)
test = subset(census, spl == FALSE)
incomeLog = glm(over50k ~ ., data=train, family="binomial")
summary(incomeLog)
predictions = predict(incomeLog, newdata=test, type="response")
table(test$over50k, predictions > 0.5)
tab1 = table(test$over50k, predictions > 0.5)
(tab1[1,1]+tab1[2,2])/nrow(test)
table(test$over50k)
9713/(9713+3078)

#problem2
library(rpart)
library(rpart.plot)
CARTo50k = rpart(over50k ~ ., data=train, method="class")
prp(CARTo50k)
predictions = predict(CARTo50k, newdata=test, type="class")
table(test$over50k, predictions)
(9243+1596)/nrow(test)
library(ROCR)
PredictROC = predict(CARTo50k, newdata = test)
head(PredictROC)
pred = prediction(PredictROC[,2], test$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)
prp(CARTo50k)

#problem3
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
library(randomForest)
set.seed(1)
forestover50k = randomForest(over50k ~ ., data=trainSmall)
predictForest = predict(forestover50k, newdata=test)
table(test$over50k, predictForest)
(9586+1093)/nrow(test)
vu = varUsed(forestover50k, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forestover50k$forest$xlevels[vusorted$ix]))
varImpPlot(forestover50k)

#problem4 cross validation
library(caret)
library(e1071)
set.seed(2)
numFolds = trainControl(method="cv", number=10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k ~ ., data=train, method="rpart", trControl=numFolds, tuneGrid=cartGrid)
CART50kcp = rpart(over50k ~ ., data=train, method="class", cp=0.002)
predictions = predict(CART50kcp, newdata=test, type="class")
table(test$over50k, predictions)
(9178+1838)/nrow(test)
prp(CART50kcp)



