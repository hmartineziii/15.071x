#video6
Claims = read.csv("ClaimsData.csv")
str(Claims)
table(Claims$bucket2009)/nrow(Claims)
library(caTools)

set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain = subset(Claims, spl == TRUE)
ClaimsTest = subset(Claims, spl == FALSE)

#QQ after video6
summary(ClaimsTrain)
cmat1 = table(Claims$diabetes) #confusion matrix 1
cmat1
cmat1[2]/(cmat1[2]+cmat1[1])

#Video7
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
mat2=table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
(mat2[1,1]+mat2[2,2]+mat2[3,3]+mat2[4,4]+mat2[5,5])/nrow(ClaimsTest)
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)
PenaltyMatrix
as.matrix(mat2)*PenaltyMatrix
sum(as.matrix(mat2)*PenaltyMatrix)/nrow(ClaimsTest)

#QQ after video7
table(ClaimsTest$bucket2009)
tab1 = table(ClaimsTest$bucket2009)
tab1[1]/nrow(ClaimsTest)
PenaltyMatrix2 = c(0,2,4,6,8)
tab1 * PenaltyMatrix2
sum(tab1 * PenaltyMatrix2)/nrow(ClaimsTest)

#Video8
library(rpart)
library(rpart.plot)
ClaimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005)
prp(ClaimsTree)
PredictTest = predict(ClaimsTree, newdata=ClaimsTest, type="class")
mat3=table(ClaimsTest$bucket2009, PredictTest)
(mat3[1,1]+mat3[2,2]+mat3[3,3]+mat3[4,4]+mat3[5,5])/nrow(ClaimsTest)
as.matrix(mat3)*PenaltyMatrix
sum(as.matrix(mat3)*PenaltyMatrix)/nrow(ClaimsTest)
ClaimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005, parms=list(loss=PenaltyMatrix))
PredictTest = predict(ClaimsTree, newdata=ClaimsTest, type="class")
mat3=table(ClaimsTest$bucket2009, PredictTest)
(mat3[1,1]+mat3[2,2]+mat3[3,3]+mat3[4,4]+mat3[5,5])/nrow(ClaimsTest)
as.matrix(mat3)*PenaltyMatrix
sum(as.matrix(mat3)*PenaltyMatrix)/nrow(ClaimsTest)






