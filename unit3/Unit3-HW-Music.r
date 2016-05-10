songs = read.csv("songs.csv")

#problem1
str(songs)
table(songs$year)
table(songs$artistname == "Michael Jackson")
MichaelJackson = subset(songs, artistname == "Michael Jackson")
str(MichaelJackson)
table(factor(MichaelJackson$songtitle), MichaelJackson$Top10)
## ORRR
MichaelJackson[c(“songtitle”, “Top10”)]
summary(songs$timesignature)
table(songs$timesignature)
songs$songtitle[which.max(songs$tempo)]

#problem2
SongsTrain = subset(songs, year < 2010)
SongsTest = subset(songs, year == 2010)
str(SongsTrain) ### ORRR nrow(SongsTrain)
#remove unwanted indep variables
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

#problem3
cor(SongsTrain$loudness, SongsTrain$energy)
nonvars2 = c("year", "loudness", "songtitle", "artistname", "songID", "artistID")
nonvars3 = c("year", "energy", "songtitle", "artistname", "songID", "artistID")
SongsTrain2 = SongsTrain[ , !(names(SongsTrain) %in% nonvars2) ]
SongsTrain3 = SongsTrain[ , !(names(SongsTrain) %in% nonvars3) ]
SongsLog2 = glm(Top10 ~ ., data=SongsTrain2, family=binomial)
SongsLog3 = glm(Top10 ~ ., data=SongsTrain3, family=binomial)
##ORRR
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog2)

#problem4
testPrediction = predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10, testPrediction >= 0.45)
(309+19)/nrow(SongsTest)
table(SongsTest$Top10)
table(SongsTest$Top10, testPrediction >= 0.45)
19/(40+19) # sensitivity
309/(309+5) #specificity













