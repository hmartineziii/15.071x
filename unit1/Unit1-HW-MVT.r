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
jpeg("unit1_3-1.jpg")
hist(mvt$Date, breaks=100)
dev.off()
#decreases

#3.2
boxplot(mvt$Date)
boxplot(subset(mvt$Date, mvt$Arrest ==TRUE))
#ORRR
boxplot(mvt$Date ~ mvt$Arrest)

#3.3
table(mvt$Year == "2001", mvt$Arrest ==TRUE)
table(mvt$Year == "2001")
# ORRR
table(mvt$Arrest, mvt$Year)
2152/(2152+18517)

#3.4
1212/(1212+13068)

#3.5
550/(550+13542)


#4.1
sort(table(mvt$LocationDescription))

#4.2
sort(table(mvt$LocationDescription))[73:78]
TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION","DRIVEWAY - RESIDENTIAL") 
Top5 = subset(mvt, LocationDescription %in% TopLocations)
str(Top5)
#ORRR 
Top5 = subset(mvt, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")
#ORRR
TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")
Top5 = subset(mvt, LocationDescription %in% TopLocations)

#4.3
Top5$LocationDescription = factor(Top5$LocationDescription)


#4.3
table(Top5$LocationDescription, Top5$Arrest==TRUE)
                                
                                  FALSE   TRUE
  ALLEY                            2059    249
  DRIVEWAY - RESIDENTIAL           1543    132
  GAS STATION                      1672    439
  PARKING LOT/GARAGE(NON.RESID.)  13249   1603
  STREET                         144969  11595
> 249/2059
[1] 0.1209325
> 132/1543
[1] 0.08554763
> 439/1672
[1] 0.2625598
> 1603/13249
[1] 0.1209903
> 11595/144969
[1] 0.07998262
#ORRRR
table(Top5$LocationDescription, Top5$Arrest)
#ORRRR
Top5$LocationDescription = factor(Top5$LocationDescription)
t = table(Top5$LocationDescription)
df1 <- as.data.frame(table(Top5$LocationDescription, Top5$Arrest))
df2 <- rep(as.data.frame(t)[, 2], 2)
df <- data.frame(df1, df2)
df$percentage <- round(df$Freq/df$df2, 3) * 100
colnames(df)[1] = "Location"
colnames(df)[2] = "Arrest"
colnames(df)[3] = "Arrests"
colnames(df)[4] = "Thefts"
# Subset of a data in which an arrest was made
T = df[df$Arrest == TRUE, ]
# Sort the subset data in descending order of percentage to find the
# location at which the highest percentage of arrests were made
T[order(-T$percentage), ][1, ]
##      Location Arrest Arrests Thefts percentage
## 8 GAS STATION   TRUE     439   2111       20.8

#4.4
table(Top5$LocationDescription, Top5$Weekday)

#4.5
table(Top5$LocationDescription, Top5$Weekday)
