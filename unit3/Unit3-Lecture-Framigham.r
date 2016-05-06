framingham = read.csv("framingham.csv")
str(framingham)
library(caTools)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)
framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)
predictTest = predict(framinghamLog, type="response", newdata=test)
table(test$TenYearCHD, predictTest > 0.5)
(1069+11)/(1069+6+187+11)
(1069+6)/(1069+6+187+11)
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred),"auc")@y.values)
#TN FP
#FN TP
#Sensitivity = TP / (TP + FN) AKA True Positive rate
#Specificity = TN / (TN + FP) AKA True Negative rate
11/(187+11)
1069/(1069+6)





