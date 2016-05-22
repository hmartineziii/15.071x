#problem1
emails = read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)
table(emails$spam)
max(nchar(emails$text))
which.min(nchar(emails$text))

#problem2
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, PlainTextDocument)
dtm = DocumentTermMatrix(corpus)
dtm
ncol(dtm)
spdtm= removeSparseTerms(dtm, 1-0.05)
spdtm
ncol(spdtm)
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))
emailsSparse$spam = emails$spam
sum(colSums(subset(emailsSparse, emailsSparse$spam==0)) >= 5000)
head(sort(colSums(subset(emailsSparse, spam == 0)),decreasing = T),10)
sum(colSums(subset(emailsSparse, emailsSparse$spam==1)) >= 1000)-1

#problem3
emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)
spamLog = glm(spam ~ ., data = train, family="binomial")
library(rpart)
library(rpart.plot)
spamCART = rpart(spam ~ ., data = train, method="class")
library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~ ., data = train)
predTrainLog = predict(spamLog, type="response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type="prob")[,2]
predTrainLog
a = sum(predTrainLog< 0.00001)
a
b = sum(predTrainLog > 0.99999)
b
nrow(train) - a - b
summary(spamLog)
table(train$spam, predTrainLog >= 0.5)
(3052+954)/(9052+954+4)
library(ROCR)
ROCRpredTrain = prediction(predTrainLog, train$spam)
perf = performance(ROCRpredTrain, "tpr", "fpr")
auc = as.numeric(performance(ROCRpredTrain, "auc")@y.values)
auc
cmat_CART<-table(train$spam, predTrainCART > 0.5)
cmat_CART
accu_CART <- (cmat_CART[1,1] + cmat_CART[2,2])/sum(cmat_CART)
accu_CART
library(ROCR)
predictionTrainCART = prediction(predTrainCART, train$spam)
perf <- performance(predictionTrainCART, "tpr", "fpr")
as.numeric(performance(predictionTrainCART, "auc")@y.values)
cmat_RF<-table(train$spam, predTrainRF > 0.5)
cmat_RF
accu_RF <- (cmat_RF[1,1] + cmat_RF[2,2])/sum(cmat_RF)
accu_RF
library(ROCR)
predictionTrainRF = prediction(predTrainRF, train$spam)
perf<-performance(predictionTrainRF, "tpr", "fpr")
as.numeric(performance(predictionTrainRF, "auc")@y.values)

#problem4
predTestLog<- predict(spamLog, newdata=test, type="response")
predTestCART <- predict(spamCART, newdata=test)[,2]
predTestRF <- predict(spamRF, newdata=test, type="prob")[,2]
cmat_log<-table(test$spam, predTestLog> 0.5)
cmat_log
accu_log <- (cmat_log[1,1] + cmat_log[2,2])/sum(cmat_log)
accu_log
library(ROCR)
predictionTestLog = prediction(predTestLog,test$spam)
perf <- performance(predictionTestLog, "tpr", "fpr")
as.numeric(performance(predictionTestLog, "auc")@y.values)
cmat_CART<-table(test$spam, predTestCART > 0.5)
cmat_CART
accu_CART <- (cmat_CART[1,1] + cmat_CART[2,2])/sum(cmat_CART)
accu_CART
library(ROCR)
predictionTestCART = prediction(predTestCART,test$spam)
perf <- performance(predictionTestCART, "tpr", "fpr")
as.numeric(performance(predictionTestCART, "auc")@y.values)
cmat_RF<-table(test$spam, predTestRF > 0.5)
cmat_RF
accu_RF <- (cmat_RF[1,1] + cmat_RF[2,2])/sum(cmat_RF)
accu_RF
library(ROCR)
predictionTestRF = prediction(predTestRF,test$spam)
perf <- performance(predictionTestRF, "tpr", "fpr")
as.numeric(performance(predictionTestRF, "auc")@y.values)

#problem5
#problem6
wordCount = rowSums(as.matrix(dtm))
library(slam)
wordCount = rollup(dtm, 2, FUN=sum)$v
hist(wordCount)
hist(log(wordCount))
emailsSparse$logWordCount<-log(wordCount)
boxplot(emailsSparse$logWordCount~emailsSparse$spam)
train2 = subset(emailsSparse, spl == TRUE)
test2 = subset(emailsSparse, spl == FALSE)
library(rpart)
library(rpart.plot)
spam2CART = rpart(spam~., data=train2, method="class")
prp(spam2CART)
set.seed(123)
spam2RF = randomForest(spam~., data=train2)
predTest2CART = predict(spam2CART, newdata=test2)[,2]
predTest2RF = predict(spam2RF, newdata=test2, type="prob")[,2]
cmat_CART<-table(test2$spam, predTest2CART > 0.5)  #first arg is the true outcomes and the second is the predicted outcomes
cmat_CART
accu_CART <- (cmat_CART[1,1] + cmat_CART[2,2])/sum(cmat_CART)
accu_CART
library(ROCR)
predictionTest2CART= prediction(predTest2CART, test2$spam)
perf <- performance(predictionTest2CART, "tpr", "fpr")
as.numeric(performance(predictionTest2CART, "auc")@y.values)
cmat_RF<-table(test2$spam, predTest2RF > 0.5)
cmat_RF
accu_RF <- (cmat_RF[1,1] + cmat_RF[2,2])/sum(cmat_RF)
accu_RF
library(ROCR)
predictionTest2RF = prediction(predTest2RF, test2$spam)
perf<-performance(predictionTest2RF, "tpr", "fpr")
as.numeric(performance(predictionTrainRF, "auc")@y.values)










