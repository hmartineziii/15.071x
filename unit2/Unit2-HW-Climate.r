climate_change = read.csv("climate_change.csv")
str(climate_change)

#Problem 1
train = subset(climate_change, Year < 2007)
test = subset(climate_change, Year > 2006)
trainModel = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(trainModel)

#Problem 2
cor(train$N2O, train$MEI)
cor(train$N2O, train$CO2)
cor(train$N2O, train$CH4)
cor(train$N2O, train$CFC.11)
cor(train$N2O, train$CFC.12)
cor(train$N2O, train$Aerosols)
cor(train$N2O, train$TSI)

cor(train$CFC.11, train$MEI)
cor(train$CFC.11, train$CO2)
cor(train$CFC.11, train$CH4)
cor(train$CFC.11, train$N2O)
cor(train$CFC.11, train$CFC.12)
cor(train$CFC.11, train$Aerosols)
cor(train$CFC.11, train$TSI)

#Problem3
trainModel2 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data=train)
summary(trainModel2)

#Problem4
climateStep = step(trainModel)
summary(climateStep)

#Problem5
tempPredict = predict(climateStep, newdata = test)
SSE = sum((tempPredict - test$Temp)^2)
SST = sum((mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST
R2

