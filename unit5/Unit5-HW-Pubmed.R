trials = read.csv("clinical_trial.csv", stringAsFactors=FALSE)

#problem1
summary(trials)
str(trials)
max(nchar(trials$abstract))
table(nchar(trials$abstract))[1:5]
table(nchar(trials$abstract) == 0)
sum(nchar(trials$abstract) == 0)
trials$title[which.min(nchar(trials$title))]

#problem2
library(tm)
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower))
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
length(stopwords("english"))
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
dim(dtmTitle)
dim(dtmAbstract)
max(colSums(dtmAbstract))

#problem3
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial=trials$trial
ncol(dtm)
library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, 0.7)
train= subset(dtm, spl == TRUE)
test=subset(dtm, spl == FALSE)
cmat_baseline <-table(train$trial) 
cmat_baseline 
730/(730+572)
library(rpart)
library(rpart.plot)
trialCART = rpart(trial~., data=train, method="class")
prp(trialCART)
predTrain= predict(trialCART)
max(predTrain[,2])
predTrain = predict(trialCART)[,2]
cmat_CART1<-table(train$trial,predTrain >= 0.5)
cmat_CART1
accu_CART <- (cmat_CART1[1,1] + cmat_CART1[2,2])/sum(cmat_CART1)
accu_CART
441/(131+441) # sensitivity
631/(631+99) #specificity

#problem4
predTest = predict(trialCART, newdata=test)[,2] 
summary(predTest)
cmat_CART<-table(test$trial, predTest>= 0.5)  #first arg is the true outcomes and the second is the predicted outcomes
cmat_CART
accu_CART <- (cmat_CART[1,1] + cmat_CART[2,2])/sum(cmat_CART)
accu_CART
library(ROCR)
ROCRpredTest = prediction(predTest, test$trial)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc




