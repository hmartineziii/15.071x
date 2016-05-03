#P(y=1) = 1 / (1 + e^(-(beta0+beta1*x1+...+betak*xk)))
#Odds = e^(beta0+beta1*x1+...+betak*xk)
#log(Odds) or "Logit" = beta0+beta1*x1+...+betak*xk

#QQ after Video3
-1.5+3*1+(-0.5)*5 #Logit
exp(-1) #Odds
P = 1/(1+exp(-1*(-1)))
P

#QQ
quality = read.csv("quality.csv")
str(quality)
table(quality$PoorCare)
98/131
install.packages("caTools")
library("caTools")

set.seed(88) #to match the example in class
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split

qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family = binomial)
summary(QualityLog)
predictTrain = predict(QualityLog, type="response")
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)

#QQ after video4
QualityLog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family = binomial)
summary(QualityLog2)

#Video5 
#TN FP
#FN TP
#Sensitivity = TP / (TP + FN) AKA True Positive rate
#Specificity = TN / (TN + FP) AKA True Negative rate
table(qualityTrain$PoorCare, predictTrain > 0.5)
10/25 #Sensitivity
70/74 #Specificity
table(qualityTrain$PoorCare, predictTrain > 0.7)
8/25 #Lower Sensitivity or true positives
73/74 #Higher Specificity or true negatives
table(qualityTrain$PoorCare, predictTrain > 0.2)
16/25 #Higher Sensitivity
54/74 #Lower Specificity

#QQ after video5
#TN FP
#FN TP
#Sensitivity = TP / (TP + FN) AKA True Positive rate
#Specificity = TN / (TN + FP) AKA True Negative rate
#15 10
#5  20
20/(20+5) #Sensitivity
15/(15+10) #Specificity







