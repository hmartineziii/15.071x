gerber = read.csv("gerber.csv")
str(gerber)

#problem1
table(gerber$voting)
tab1 = table(gerber$voting)
tab1[2]/nrow(gerber)
summary(gerber)
tapply(gerber$voting,gerber$civicduty, mean)
tapply(gerber$voting,gerber$hawthorne, mean)
tapply(gerber$voting,gerber$self, mean)
tapply(gerber$voting,gerber$neighbors, mean)
votingLog = glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)
summary(votingLog)
predictLog = predict(votingLog, type="response")
table(gerber$voting, predictLog >= 0.3)
(134513+51966)/(134513+100875+56730+51966)
table(gerber$voting, predictLog >= 0.5)
235388/(108696+235388)
library(ROCR)
ROCRpred = prediction(predictLog, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

#problem2
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)
CARTmodel3 = rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel3)
 
#problem3
CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
CARTmodel5 = rpart(voting ~ sex + control, data=gerber, cp=0.0)
prp(CARTmodel4, digits=6)
0.34-0.296638
prp(CARTmodel5, digits=6)
abs(0.302795-0.345818)
abs(0.290456-0.334176)
LogModelSex = glm(voting ~ sex + control , data = gerber, family = binomial)
summary(LogModelSex)
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")
prp(CARTmodel5, digits=6)
0.290456-.2908065
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
predict(LogModel2, newdata=Possibilities, type="response")
prp(CARTmodel5, digits=6)






