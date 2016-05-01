FluTrain = read.csv("FluTrain.csv")

#Problem1
summary(FluTrain)
FluTrain$Week[which.max(FluTrain$ILI)]
FluTrain$Week[which.max(FluTrain$Queries)]
# ORRR
levels(factor(FluTrain$Week[which.max(FluTrain$ILI)]))
levels(factor(FluTrain$Week[which.max(FluTrain$Queries)]))

jpeg("Unit2-HW-Flu01.jpg")
hist(FluTrain$ILI, xlab="ILI", main="Distribution of Influenza Like Illness related Physician Visits")
dev.off()

jpeg("Unit2-HW-Flu02.jpg")
plot(log(FluTrain$ILI), FluTrain$Queries, xlab="Log(ILI)", main="ILI vs Queries")
dev.off()

#Problem2
plot(FluTrain$Queries,log(FluTrain$ILI), xlab="Queries", main="Queries vs. Log(ILI)")
fluTrend1 = lm(log(ILI) ~ Queries,data=FluTrain)
summary(fluTrend1)
COR = cor(log(FluTrain$ILI), FluTrain$Queries)
COR^2

#Problem3
FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(fluTrend1, newdata=FluTest))
which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]
(FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]
SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE

#Problem4
install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain$ILILag2)

jpeg("Unit2-HW-Flu03.jpg")
plot(log(FluTrain$ILILag2), log(FluTrain$ILI), xlab="Log(ILILag2)", main="Log(ILILag2) vs Log(ILI)")
dev.off()

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data=FluTrain)
summary(FluTrend2)

#Problem5
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)
FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain) - 1]
summary(FluTest$ILILag2)
FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)]
summary(FluTest$ILILag2)
FluTest$ILILag2[1]
FluTest$ILILag2[2]
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PredTest2 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE

