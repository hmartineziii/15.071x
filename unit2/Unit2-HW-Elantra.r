elantra = read.csv("elantra.csv")
str(elantra)

#Problem1
train = subset(elantra, elantra$Year < 2013)
test = subset(elantra, elantra$Year > 2012)
str(train)

#Problem2
model1 = lm(ElantraSales~Unemployment+CPI_all+CPI_energy+Queries,data=train)
summary(model1)

#Problem3
model2 = lm(ElantraSales~Month+Unemployment+CPI_all+CPI_energy+Queries,data=train)
summary(model2)
110.69*(3-1)
110.69*(5-1)
#3.4
# The second choice is the correct answer. The previous subproblem essentially showed that for every month that we move into the future (e.g, from January to February, from February to March, etc.), our predicted sales go up by 110.69. This isn't right, because the effect of the month should not be affected by the numerical coding, and by modeling Month as a numeric variable, we cannot capture more complex effects. For example, suppose that when the other variables are fixed, an additional 500 units are sold from June to December, relative to the other months. This type of relationship between the boost to the sales and the Month variable would look like a step function at Month = 6, which cannot be modeled as a linear function of Month.
# The first choice is not right. As we have discussed before, increasing the number of coefficients will never cause the model's R-Squared to decrease, but if the increase is small, then we have not really improved the predictive power of our model, and converting Month to a factor variable is not justified
# The third choice is also not correct. Month is stored as an ordinary number, so there cannot be any issues due to the Date format.


#Problem4
model3 = lm(ElantraSales~as.factor(Month)+Unemployment+CPI_all+CPI_energy+Queries,data=train)
summary(model3)

#Problem5
cor(train$CPI_energy, train$Month)
cor(train$CPI_energy, train$Unemployment)
cor(train$CPI_energy, train$Queries)
cor(train$CPI_energy, train$CPI_all)

cor(train$Queries, train$Month)
cor(train$Queries, train$Unemployment)
cor(train$Queries, train$CPI_energy)
cor(train$Queries, train$CPI_all)

#Problem6
model4 = lm(ElantraSales~as.factor(Month)+Unemployment+CPI_all+CPI_energy,data=train)
summary(model4)
predict4 = predict(model4, newdata=test)
SSE = sum((predict4 - test$ElantraSales)^2)
SSE
mean(train$ElantraSales)
SST = sum((mean(train$ElantraSales) - test$ElantraSales)^2)
SST
R2 = 1 -SSE/SST
R2
max(predict4 - test$ElantraSales)
490.4278
min(predict4 - test$ElantraSales)
-7491.488
max(abs(predict4 - test$ElantraSales))
str(test)
test$Month[which.max(abs(predict4 - test$ElantraSales))]
test$Year[which.max(abs(predict4 - test$ElantraSales))]



