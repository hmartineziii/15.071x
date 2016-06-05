#p1
tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
library(tm)
library(SnowballC)
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus<- tm_map(corpus, removeWords, stopwords("english"))
frequencies <- DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(frequencies))
frequencies 

#p2
library(wordcloud)
colnames(allTweets)
colSums(allTweets)
wordcloud(colnames(allTweets), colSums(allTweets))
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
frequencies <- DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(frequencies))
wordcloud(colnames(allTweets), colSums(allTweets))

#p3
#p4
library(RColorBrewer)
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25),min.freq=10, colors=brewer.pal(9,"Blues"))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25),min.freq=10, colors=brewer.pal(9,"Blues")[c(5,6,7,8,9)])
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25),min.freq=10, colors=brewer.pal(9,"Blues")[5:9])
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25),min.freq=10,colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)])
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25),min.freq=10, colors=brewer.pal(9, "Blues")[-1:-4])










