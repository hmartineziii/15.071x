baseball = read.csv("baseball.csv")
#Video1
#Video2
str(baseball)
moneyball = subset(baseball, Year < 2002)
str(moneyball)
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)
plot(moneyball$RD, moneyball$W)
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)
# W = 80.8814 + 0.1058 (RD)
# W >= 95
# RD = (95-80.8814)/RD
# RD = 133.4

#QQ after Video2
# RS = 713 RA = 641 RD = 99
# W = 80.8814 + 0.1058 (99)
# W = 91.3

#video3
str(moneyball)
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)
RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

#QQ after video3
summary(RunsReg)
-804.63+2737.77*0.311+1584.91*.405
RunsAllowedReg = lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RunsAllowedReg)
-837.38+2913.6*.297+1514.29*.370

#video4
#video5
#QQ after video 5
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2012)
cor(teamRank, wins2013)
