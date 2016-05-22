#Video5
tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)
#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]$content
corpus = tm_map(corpus, content_transformer(tolower))
corpus[[1]]$content
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
stopwords("english")[1:10]
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]$content
corpus = tm_map(corpus, stemDocument)

#video6
frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005, 505:515])
findFreqTerms(frequencies, lowfreq=20)
sparse = removeSparseTerms(frequencies, 0.995)
sparse
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Negative = tweets$Negative
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio=0.7)
trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)

#QQ after video6
findFreqTerms(frequencies, lowfreq=100)

#Video7
library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)
predictCART = predict(tweetCART, newdata = testSparse, type="class")
table(testSparse$Negative, predictCART)
(294+18)/(294+6+37+18)
table(testSparse$Negative)
300/355
library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data=trainSparse)
table(testSparse$Negative, predictRF)

#QQ after video7
tweetLog = glm(Negative ~ ., data=trainSparse, family="binomial")
summary(tweetLog)
predictions = predict(tweetLog, newdata=testSparse, type="response")
cmat_log = table(testSparse$Negative, predictions > 0.5)
cmat_log
accu_log = (cmat_log[1,1] + cmat_log[2,2])/sum(cmat_log)
accu_log






