loans = read.csv("loans.csv")

#problem1
str(loans)
table(loans$not.fully.paid)
table(loans$not.fully.paid)
1533/(1533+8045)
summary(loans)
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revo.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing)
table(missing$not.fully.paid)
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
imputed = read.csv("loans_imputed.csv")
loans[vars.for.imputation] = imputed

#problem2
set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)
mod = glm(not.fully.paid~., data=train, family="binomial")
summary(mod)
-9.406e-03*(710-700)
exp(-9.406e-03*700)
exp(-9.406e-03*710)
predicted.risk<-predict(mod,type="response", newdata=test)
test$predicted.risk<-predicted.risk
table(test$not.fully.paid, as.numeric(predicted.risk >= 0.5))
(2400+3)/(2400+13+457+3)
table(test$not.fully.paid)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

#problem3
bivariate<-glm(not.fully.paid~ int.rate,data=train,family = binomial)
summary(bivariate)
pred.bivariate<-predict(bivariate,type="response", newdata=test)
max(pred.bivariate)
table(test$not.fully.paid, as.numeric(pred.bivariate >= 0.5))
ROCRpred = prediction(pred.bivariate, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

#problem4
10*exp(0.06*3)

#problem5
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
summary(test)

#problem6
highInterest = subset(test, test$int.rate >= 0.15)
summary(highInterest)
table(highInterest$not.fully.paid)
110/(110+327)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, highInterest$predicted.risk <= cutoff)
str(selectedLoans)
summary(selectedLoans)
table(selectedLoans$not.fully.paid)



