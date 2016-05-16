data(state)
statedata = data.frame(state.x77)
str(statedata)

#Problem1
linmod = lm(Life.Exp ~ .,data=statedata)
summary(linmod)
LEpred = predict(linmod)
SSE = sum((statedata$Life.Exp - LEpred)^2)
SSE
linmod2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad,data=statedata)
summary(linmod2)
LEpred2 = predict(linmod2)
SSE2 = sum((statedata$Life.Exp - LEpred2)^2)
SSE2

#problem 2 build a CART model
library(rpart)
library(rpart.plot)
CARTmodel = rpart(Life.Exp ~ ., data=statedata)
prp(CARTmodel)
PredictionsCART= predict(CARTmodel)
tree.sse = sum((PredictionsCART - statedata$Life.Exp)^2)
tree.sse
CARTmodel2 = rpart(Life.Exp ~ ., data=statedata, minbucket=5)
prp(CARTmodel2)
PredictionsCART2 = predict(CARTmodel2)
tree.sse2 = sum((PredictionsCART2 - statedata$Life.Exp)^2)
tree.sse2
CARTmodel3 = rpart(Life.Exp ~ Area, data=statedata, minbucket=1)
prp(CARTmodel3)
PredictionsCART3 = predict(CARTmodel3)
tree.sse3 = sum((PredictionsCART3 - statedata$Life.Exp)^2)
tree.sse3

#problem3
library(caret)
set.seed(111)
numFolds = trainControl(method="cv", number=10)
cartGrid = expand.grid( .cp = seq(0.01,0.5,0.01))
train(Life.Exp ~ ., data=statedata, method="rpart", trControl=numFolds, tuneGrid=cartGrid)
CARTlifecp = rpart(Life.Exp ~ ., data=statedata, cp=0.11)
prp(CARTlifecp)
PredictionsCART4 = predict(CARTlifecp)
tree.sse = sum((statedata$Life.Exp - PredictionsCART4)^2)
tree.sse
numFolds = trainControl(method="cv", number=10)
cartGrid = expand.grid( .cp = seq(0.01,0.5,0.01))
set.seed(111)
train(Life.Exp ~ Area, data=statedata, method="rpart", trControl = numFolds, tuneGrid = cartGrid )
set.seed(111)
CARTmodel5 = rpart(Life.Exp ~ Area, data=statedata, cp=0.06)
prp(CARTmodel5)
PredictionsCART5 = predict(CARTmodel5)
tree.sse = sum((statedata$Life.Exp - PredictionsCART5)^2)
tree.sse


