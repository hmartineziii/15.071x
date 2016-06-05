#problem1
airlines<- read.csv("AirlinesCluster.csv")
summary(airlines)
colMeans(airlines)
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines) #Normalising the data
summary(airlinesNorm)
round(colMeans(airlinesNorm),6)
apply(airlinesNorm, 2, max)
apply(airlinesNorm, 2, min)

#problem2
distances <- dist(airlinesNorm,method="euclidean")
airlineClust <- hclust(distances,method="ward.D")
plot(airlineClust)
clusterGroups<- cutree(airlineClust, k=5)
airlineClusters <-split(airlinesNorm, clusterGroups)
nrow(airlineClusters[[1]])
table(clusterGroups)
tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusTrans, clusterGroups, mean)
tapply(airlines$FlightMiles, clusterGroups, mean)
tapply(airlines$FlightTrans, clusterGroups, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)
airlinesUnnormClusters<-split(airlines,clusterGroups)
round(sapply(airlinesUnnormClusters,colMeans),4)

#problem3
set.seed(88)
kmeansClust<- kmeans(airlinesNorm, centers=5, iter.max=1000)
table(kmeansClust$cluster)
sum(kmeansClust$size > 1000)
airlinesUnnormClusters<-split(airlinesNorm,clusterGroups)
round(sapply(airlinesUnnormClusters,colMeans),4)
kmeansClust$centers






