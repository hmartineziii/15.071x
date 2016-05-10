baseball = read.csv("baseball.csv")

#problem1
str(baseball)
nrow(baseball)
table(baseball$Year)
factor(baseball$Year)
47
2012-1962
str(baseball)
baseball = subset(baseball, Playoffs == TRUE)
summary(baseball)
table(baseball$Year)

#problem2
PlayoffTable = table(baseball$Year)
names(PlayoffTable)
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
table(baseball$NumCompetitors)

#problem3
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)
m1 = glm(WorldSeries ~ Year, data=baseball, family="binomial")
summary(m1)
m2 = glm(WorldSeries ~ RS, data=baseball, family="binomial")
summary(m2)
m3 = glm(WorldSeries ~ RA, data=baseball, family="binomial")
summary(m3)
m4 = glm(WorldSeries ~ W, data=baseball, family="binomial")
summary(m4)
m5 = glm(WorldSeries ~ OBP, data=baseball, family="binomial")
summary(m5)
m6 = glm(WorldSeries ~ SLG, data=baseball, family="binomial")
summary(m6)
m7 = glm(WorldSeries ~ BA, data=baseball, family="binomial")
summary(m7)
m8 = glm(WorldSeries ~ RankSeason, data=baseball, family="binomial")
summary(m8)
m9 = glm(WorldSeries ~ OOBP, data=baseball, family="binomial")
summary(m9)
m10 = glm(WorldSeries ~ OSLG, data=baseball, family="binomial")
summary(m10)
m11 = glm(WorldSeries ~ NumCompetitors, data=baseball, family="binomial")
summary(m11)
m12 = glm(WorldSeries ~ League, data=baseball, family="binomial")
summary(m12)

#problem4
mod = glm(WorldSeries ~ NumCompetitors + RankSeason + RA + Year, data=baseball, family="binomial")
cor(baseball$Year,baseball$RA)
cor(baseball$Year,baseball$RankSeason)
cor(baseball$Year,baseball$NumCompetitors)
cor(baseball$RA,baseball$RankSeason)
cor(baseball$RA,baseball$NumCompetitors)
cor(baseball$RankSeason,baseball$NumCompetitors)

mod1 = glm(WorldSeries ~ Year + RA, data=baseball, family="binomial")
mod2 = glm(WorldSeries ~ Year + RankSeason, data=baseball, family="binomial")
mod3 = glm(WorldSeries ~ Year + NumCompetitors, data=baseball, family="binomial")
mod4 = glm(WorldSeries ~ RA + RankSeason, data=baseball, family="binomial")
mod5 = glm(WorldSeries ~ RA + NumCompetitors, data=baseball, family="binomial")
mod6 = glm(WorldSeries ~ RankSeason + NumCompetitors, data=baseball, family="binomial")
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
summary(mod6)
summary(m1)
summary(m3)
summary(m8)
summary(m11)






