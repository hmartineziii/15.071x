data(state)
statedata = data.frame(state.x77)
str(statedata)

#Problem1
linmod = lm(Life.Exp ~ .,data=statedata)
summary(linmod)
LEpred = predict(linmod)
SSE = sum((statedata$Life.Exp - LEpred)^2)
SSE
linmod2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad,data=statedata)
summary(linmod2)
LEpred2 = predict(linmod2)
SSE2 = sum((statedata$Life.Exp - LEpred2)^2)
SSE2




