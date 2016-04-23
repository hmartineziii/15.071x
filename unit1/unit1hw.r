mvtWeek1 = read.csv("mvtWeek1.csv")
#1.1 1.2
str(mvtWeek1)
# 191641 observations, 11 variables

#1.3
max(mvtWeek1$ID)
# 9181151

#1.4
min(mvtWeek1$Beat)
#111

#1.5
nrow(subset(mvtWeek1, Arrest==TRUE))
## 15536 OR
summary(mvtWeek1)

#1.6
summary(mvtWeek1)
#OR
table(mvtWeek1$LocationDescription)




mvt=mvtWeek1
#2.1
mvt$Date[1]
#12/31/15 23:15

#2.2
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
# May 2006

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
#2.3
table(mvt$Month)
#Feb

#2.4
table(mvt$Weekday)
#Friday

#2.5
table(subset(mvt$Weekday, Arrest==TRUE))
#1435 ORRRR
table(mvt$Arrest, mvt$Month)





#3.1
