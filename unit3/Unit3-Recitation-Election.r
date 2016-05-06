polling = read.csv("pollingData.csv")

#how to perform multiple imputation to fill in 
#missing values from surveys based on other
#surveys available
str(polling)
table(polling$Year)
summary(polling)
install.packages("mice")
library("mice")
simple = polling(c("Rasmussen", "SurveyUSA", "PropR", "DiffCount"))
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling)

# building models
train = subset(polling, Year == 2004 | Year == 2008)
test = subset(polling, Year == 2012)
table(train$Republican)
sign(20) #returns a 1 if positive a 0 if 0 and a -1 if negative
# its important because if a poll shows a republican winning sign(positive) will return a 1
#else a 0 or -1
table(sign(train$Rasmussen))
table(train$Republican, sign(train$Rasmussen))

#resolve multicollinearity
cor(train)
str(train)
cor(train(c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")))
mod1 = glm(Republican ~ PropR, data=train, family="binomial")
summary(mod1)
pred1 = predict(mod1, type="response")
table(train$Republican, pred1 >= 0.5)
mod2 = glm(Republican ~ DiffCount + SurveyUSA, data=train, family = "binomial")
pred2 = predict(mod2, type="response")
table(train$Republican, pred1 >= 0.5)
summary(mod2)

#evaluate model on the testing set
#smart baseline
table(test$Republican, sign(test$Rasmussen))
#mod2 prediction
testPrediction = predict(mod2, newdata=test, type="response")
table(test$Republican, testPrediction >= 0.5)
#0.5 is when you don't care which side you err on
#you want to get both right. Whether you err by a republican
#or you err by a democrat
subset(test, testPrediction >=0.5 & Republican ==0)







