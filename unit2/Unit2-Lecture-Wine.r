#Video4
wine = read.csv("wine.csv")
str(wine)
summary(wine)

model1 = lm(Price ~ AGST, data=wine)
summary(model1)
model1$residuals
SSE = sum(model1$residuals^2)

model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)
SSE = sum(model2$residuals^2)

model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePopulation, data=wine)
summary(model3)
SSE = sum(model3$residuals^2)

#Quick Question after video4
wine = read.csv("wine.csv")
modelQQ1 = lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(modelQQ1)

#Video5
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)
#the significance of variables that used to be
#insignificant increases due to
#multicollinearity

#QQafter video5
wine = read.csv("wine.csv")
modelQQ2 = lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(modelQQ2)

#Video6
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)
#correlations of 0.7 and -0.7 lead to collinearity issues

#QQ after video6
cor(wine$HarvestRain, wine$WinterRain)

#Video7
#data we used to develop model is training data
#new data is called test data (to test the model)
wineTest = read.csv("wine_test.csv")
str(wineTest)
predictTest = predict(model4, newdata=wineTest)
predictTest
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1-SSE/SST

