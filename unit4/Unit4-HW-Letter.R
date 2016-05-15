letters = read.csv("letters_ABPR.csv")

#problem1
letters$isB = as.factor(letters$letter == "B")
table(letters$isB)
2350/(2350+766)
library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)
library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=train, method="class")
predictions = predict(CARTb, newdata=test, type="class")
table(test$isB, predictions)
(1118+340)/nrow(test)
library(randomForest)
set.seed(1000)
bForest = randomForest(isB ~ . - letter, data=train)
predictForest = predict(bForest, newdata=test)
table(test$isB, predictForest)
(1164+373)/nrow(test)

#problem2
letters$letter = as.factor(letters$letter)
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)
table(test$letter)
401/(395+383+401+379)
CARTletter = rpart(letter ~ . - isB, data=train, method="class")
predictions = predict(CARTletter, newdata=test, type="class")
table(test$letter, predictions)
tab1 = table(test$letter, predictions)
(tab1[1,1]+tab1[2,2]+tab1[3,3]+tab1[4,4])/nrow(test)
library(randomForest)
set.seed(1000)
letterForest = randomForest(letter ~ . - isB, data=train)
predictForest = predict(letterForest, newdata=test)
table(test$letter, predictForest)
tab2 = table(test$letter, predictForest)
(tab2[1,1]+tab2[2,2]+tab2[3,3]+tab2[4,4])/nrow(test)







