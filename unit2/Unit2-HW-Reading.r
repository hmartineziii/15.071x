pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

#Problem 1
str(pisaTrain)
tapply(pisaTrain$readingScore, pisaTrain$male, mean)
summary(pisaTrain)
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain)
str(pisaTest)

#Problem 2
table(pisaTrain$grade)
table(pisaTrain$male)
table(pisaTrain$raceeth)

#Problem 3
str(pisaTrain$raceeth)
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
lmScore = lm(readingScore ~ ., data=pisaTrain)
summary(lmScore)
SSE = sum((lmScore$residuals)^2)
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE
summary(lmScore)
# Score Difference = 29.542 * ( 11 - 9 ) = 59ish

#Problem 4
predTest = predict(lmScore, newdata = pisaTest)
summary(predTest)
637.7-353.2
SSE = sum((predTest - pisaTest$readingScore)^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE
baseline = mean(pisaTrain$readingScore) 
baseline
SST = sum((baseline - pisaTest$readingScore)^2)
SST
R2 = 1 - SSE/SST
R2


