#p1
parole<-read.csv("parole.csv")
str(parole)
parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
str(parole)
prop.table(table(parole$male, parole$violator),2)
table(parole$crime[parole$state==2])

#p2
library(ggplot2)
ggplot(data = parole, aes(x = age)) + geom_histogram()
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5,color="blue")

#p3
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(.~male)
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)
colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5) + scale_fill_manual(values=colorPalette)
ggplot(parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, position = "identity") + scale_fill_manual(values=colorPalette)
ggplot(parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, position = "identity", alpha = 0.5) + scale_fill_manual(values=colorPalette)

#p4
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1)
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 0.1)
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1) + facet_grid(crime ~ .)
ggplot(data=parole, aes(x=time.served, fill=crime)) + geom_histogram(binwidth=1, position="identity", alpha=0.5)
















