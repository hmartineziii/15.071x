parole = read.csv("parole.csv")

#problem1
str(parole)
summary(parole)
table(parole$violator)

#problem2
str(parole)
sapply(parole,class) #look for integer
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole)

#problem3
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

#problem4
violatorLog = glm(violator ~ ., data=train, family=binomial)
summary(violatorLog)
exp(1.6119919)
#male = 1 * 0.3869904
#white = 1 * 0.8867192
#age = 50 * -0.0001756
#state = 1 * 1
#time.served = 3 * -.1238867
#max.sentence = 12 * 0.0802954
#mult.off = 0 * 1.619919
#crime = 2 * 0.6837143
#intercept = -4.2411574
Logodds=-4.2411574+1*0.6837143+12*0.0802954+3*-.1238867+0+50*-.0001756+1*.8867192+1*.3869904
exp(Logodds)
0.1825687/(1+0.1825687) #or
1/(1+exp(-(-1.700629)))

#problem5
predictions = predict(violatorLog, type="response", newdata=test)
summary(predictions)
table(test$violator, as.numeric(predictions >= 0.5))
12/(11+12)
167/(167+12)
(167+12)/(167+12+11+12)
table(test$violator)
179/(179+23)
table(test$violator, as.numeric(predictions >= 0.5))
library(ROCR)
ROCRpred = prediction(predictions, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)