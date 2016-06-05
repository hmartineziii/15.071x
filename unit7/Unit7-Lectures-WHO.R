# VIDEO 4 - A BASIC SCATTERPLOT
WHO = read.csv("WHO.csv")
str(WHO)


# Plot from Week 1
plot(WHO$GNI, WHO$FertilityRate)
install.packages("ggplot2")
library(ggplot2)
scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate))
scatterplot + geom_point()
scatterplot + geom_line()
scatterplot + geom_point()
scatterplot + geom_point(color = "blue", size = 3, shape = 17) 
scatterplot + geom_point(color = "darkred", size = 3, shape = 8) 
scatterplot + geom_point(colour = "blue", size = 3, shape = 17) + ggtitle("Fertility Rate vs. Gross National Income")
fertilityGNIplot = scatterplot + geom_point(colour = "blue", size = 3, shape = 17) + ggtitle("Fertility Rate vs. Gross National Income")
pdf("MyPlot.pdf")
print(fertilityGNIplot)
dev.off()

# VIDEO 5 - MORE ADVANCED SCATTERPLOTS 
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()
Sys.sleep(2)
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()
Sys.sleep(2)
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()
Sys.sleep(2)
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point()
Sys.sleep(2)
mod = lm(Under15 ~ log(FertilityRate), data = WHO)
summary(mod)
Sys.sleep(2)
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm")
Sys.sleep(2)
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", level = 0.99)
Sys.sleep(2)
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se = FALSE)
Sys.sleep(2)
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", colour = "orange")

#QQAfter video5
ggplot(WHO, aes(x = FertilityRate, y = Under15, color=Region)) + geom_point()








