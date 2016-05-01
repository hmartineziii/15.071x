data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center, state.division, state.name, state.region)
str(statedata)

#problem1
jpeg("Unit2-HW-State01.jpg")
plot(statedata$x, statedata$y, xlab="", main="")
dev.off()

tapply(statedata$HS.Grad, statedata$state.region, mean)

jpeg("Unit2-HW-State02.jpg")
boxplot(statedata$Murder~statedata$state.region, xlab="", main="")
dev.off()

jpeg("Unit2-HW-State03.jpg")
northeast = subset(statedata, statedata$state.region == "Northeast")
boxplot(northeast$Murder~northeast$state.name, xlab="", main="")
dev.off()

table(northeast$state.name,northeast$Murder)

#Problem2
lifemodel = lm(Life.Exp~Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area,data=statedata)
summary(lifemodel)

jpeg("Unit2-HW-State04.jpg")
plot(statedata$Income, statedata$Life.Exp)
dev.off()

#Problem3
lifemodel1 = lm(Life.Exp~Income+Murder+HS.Grad+Frost,data=statedata)
summary(lifemodel1)
lifemodel2 = lm(Life.Exp~Population+Income+HS.Grad+Frost,data=statedata)
summary(lifemodel2)
lifemodel3 = lm(Life.Exp~Illiteracy+Frost+Murder+HS.Grad,data=statedata)
summary(lifemodel3)
lifemodel4 = lm(Life.Exp~Population+Frost+Murder+HS.Grad,data=statedata)
summary(lifemodel4)

prediction = predict(lifemodel4)
statedata$state.name[which.min(prediction)]
statedata$state.name[which.min(statedata$Life.Exp)]
statedata$state.name[which.max(prediction)]
statedata$state.name[which.max(statedata$Life.Exp)]
statedata$state.name[which.min(lifemodel4$residuals)]
statedata$state.name[which.max(lifemodel4$residuals)]




