wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)

#problem1
str(wiki)
wiki$vandal = as.factor(wiki$Vandal)
table(wiki$vandal)
library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, content_transformer(tolower))
corpusAdded = tm_map(corpusAdded, PlainTextDocument)
corpusAdded = tm_map(corpusAdded, removePunctuation)
length(stopwords("english"))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded
wordsAdded= as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
dim(wordsAdded)
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, content_transformer(tolower))
corpusRemoved = tm_map(corpusRemoved, PlainTextDocument)
corpusRemoved = tm_map(corpusRemoved, removePunctuation)
length(stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved
wordsRemoved= as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
dim(wordsRemoved)
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$vandal= wiki$vandal
library(caTools)
set.seed(123)
split = sample.split(wikiWords$vandal, 0.7)
trainVandal = subset(wikiWords, split == TRUE)
testVandal = subset(wikiWords, split == FALSE)
table(testVandal)
618/(618+545)
library(rpart)
library(rpart.plot)
CARTvandal = rpart(vandal ~ ., data=trainVandal, method="class")
predictions = predict(CARTvandal, newdata=testVandal, type="class")
table(testVandal$vandal, predictions)
(618+12)/(618+12+533)
prp(CARTvandal)

#problem2
grepl("cat","dogs and cats",fixed=TRUE) # TRUE
grepl("cat","dogs and rats",fixed=TRUE) # FALSE
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
library(rpart)
library(rpart.plot)
wikiCART2 = rpart(vandal ~ ., data=wikiTrain2, method="class")
testPredictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$vandal, testPredictCART2)
(609+57)/(609+9+488+57)
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
wikiTrain3 = subset(wikiWords2, split==TRUE)
wikiTest3 = subset(wikiWords2, split==FALSE)
wikiCART3 = rpart(vandal ~ ., data=wikiTrain3, method="class")
testPredictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$vandal, testPredictCART3)

#problem3
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain4 = subset(wikiWords3, split==TRUE)
wikiTest4 = subset(wikiWords3, split==FALSE)
wikiCART4 = rpart(vandal ~ ., data=wikiTrain4, method="class")
testPredictCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$vandal, testPredictCART4)











