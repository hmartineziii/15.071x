IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

#1
str(IBM)
min(IBM$Date)
max(IBM$Date)
summary(IBM)
summary(GE)
summary(CocaCola)
summary(Boeing)
sd(ProcterGamble$StockPrice)

#2
jpeg("Unit1-HW-Stock01.jpg")
plot(CocaCola$Date, CocaCola$StockPrice, type="l")
dev.off()

jpeg("Unit1-HW-Stock02.jpg")
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, type="l", col="blue",lty=2)
dev.off()

jpeg("Unit1-HW-Stock03.jpg")
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, type="l", col="blue",lty=2)
abline(v=as.Date(c("2000-03-01")),lwd=2)
dev.off()

jpeg("Unit1-HW-Stock04.jpg")
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, type="l", col="blue",lty=2)
abline(v=as.Date(c("1983-03-01")),lwd=2)
dev.off()

jpeg("Unit1-HW-Stock05.jpg")
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
dev.off()

jpeg("Unit1-HW-Stock06.jpg")
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], col="blue")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="black")
abline(v=as.Date(c("2000-03-01")),lwd=2)
dev.off()

jpeg("Unit1-HW-Stock07.jpg")
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], col="blue")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="black")
abline(v=as.Date(c("1997-09-01")),lwd=2)
abline(v=as.Date(c("1997-11-01")),lwd=2)
dev.off()

jpeg("Unit1-HW-Stock08.jpg")
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], col="blue")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="black")
abline(v=as.Date(c("2004-01-01")),lwd=2)
abline(v=as.Date(c("2005-01-01")),lwd=2)
dev.off()


#4
tapply(IBM$StockPrice, months(IBM$Date), mean)
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)



