#Comments

WHO = read.csv("WHO.csv")
table(WHO$Region)
tapply(WHO$ChildMortality, WHO$Region, min, na.rm=TRUE)



getwd()
#gives u the working directory

# read in file
USDA=read.csv("USDA.csv")

#structure
str(USDA)

#Max level of sodium was high, lets find out more
USDA$sodium

#which has highest level of sodium
which.max(USDA$Sodium)

#find out which names of variables
names(USDA)

#what we want is the name Description
USDA$Description[265]

#new data frame for high sodium data
HighSodium=subset(USDA, Sodium>10000)
nrow(HighSodium)
HighSodium$Description

# now we wwant to know sodium of CAviar
match("CAVIAR", USDA$Description)
USDA$Sodium[4154]

#how to collapse command
USDA$Sodium[match("CAVIAR", USDA$Description)]

#compare level of caviar to all other foods
summary(USDA$Sodium)
sd(USDA$Sodium)
sd(USDA$Sodium, na.rm=TRUE)

jpeg("recitation01.jpg")
plot(USDA$Protein, USDA$TotalFat)
dev.off()

jpeg("recitation02.jpg")
plot(USDA$Protein, USDA$TotalFat, xlab = "Protein", ylab="Fat", main="Protein vs Fat", col="red")
dev.off()

jpeg("recitation03.jpg")
hist(USDA$VitaminC, xlab="Vitamin C", main="Histogram of Vitamin C Levels")
dev.off()

jpeg("recitation04.jpg")
hist(USDA$VitaminC, xlab="Vitamin C", main="Histogram of Vitamin C Levels", xlim=c(0,100))
dev.off()

jpeg("recitation05.jpg")
hist(USDA$VitaminC, xlab="Vitamin C", main="Histogram of Vitamin C Levels", xlim=c(0,100), breaks=100)
dev.off()

jpeg("recitation06.jpg")
hist(USDA$VitaminC, xlab="Vitamin C", main="Histogram of Vitamin C Levels", xlim=c(0,100), breaks=2000)
dev.off()

jpeg("recitation07.jpg")
boxplot(USDA$Sugar, main = "Boxplot of Sugar Levels")
dev.off()

jpeg("recitation08.jpg")
boxplot(USDA$Sugar, main = "Boxplot of Sugar Levels", ylab="Sugar (g)")
dev.off()


USDA$Sodium[1] > mean(USDA$Sodium, na.rm=TRUE)

USDA$Sodium[50] > mean(USDA$Sodium, na.rm=TRUE)

HighSodium = USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE)
str(HighSodium)
HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))

table(USDA$HighSodium)

table(USDA$HighSodium, USDA$HighFat)

tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=TRUE)
tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=TRUE)
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm=TRUE)