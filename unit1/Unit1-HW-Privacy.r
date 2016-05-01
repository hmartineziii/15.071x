poll = read.csv("AnonymityPoll.csv")

#1
str(poll)
summary(poll$Smartphone)
table(poll$Smartphone)
table(poll$State, poll$Region)

#2
table(poll$Internet.Use, poll$Smartphone)
summary(poll)
limited = subset(poll, poll$Internet.Use==1 | poll$Smartphone==1)
str(limited)

#3
summary(limited)
table(limited$Info.On.Internet==0)
table(limited$Info.On.Internet==11)
table(limited$Worry.About.Info)
table(limited$Anonymity.Possible)
table(limited$Tried.Masking.Identity)
table(limited$Privacy.Laws.Effective)

#4
jpeg("Unit1-HW-Privacy01.jpg")
hist(limited$Age)
dev.off()

jpeg("Unit1-HW-Privacy02.jpg")
plot(limited$Age, limited$Info.On.Internet)
dev.off()

table(limited$Age, limited$Info.On.Internet)
max(table(limited$Age, limited$Info.On.Internet))

jpeg("Unit1-HW-Privacy03.jpg")
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))
dev.off()

tapply(poll$Info.On.Internet,poll$Smartphone==1, mean, na.rm=TRUE)
tapply(poll$Tried.Masking.Identity,poll$Smartphone, table)
