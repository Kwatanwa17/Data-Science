##Q1
options(scipen=100) 
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

library(dplyr)

trainig <- filter(segmentationOriginal, Case == "Train")
testing <- filter(segmentationOriginal, Case == "Test")

set.seed(125)
modelFit <- train(Class ~ ., method = "rpart", data = trainig)

library(rattle)

fancyRpartPlot(modelFit$finalModel)

##Q2
#The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.

##Q3
library(pgmm)
data(olive)
olive = olive[,-1]

modelFit <- train(Area~., method = "rpart", data = olive)

predict(modelFit,newdata = as.data.frame(t(colMeans(olive))))

##Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
modelFit <- train(factor(chd) ~ age + alcohol + obesity + tobacco + typea + ldl,
                  method = "glm",
                  data = trainSA,
                  family = "binomial")


missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

predictionTrain <- as.numeric(as.character(predict(modelFit, newdata = trainSA, type = "raw")))
predictionTest <- as.numeric(as.character(predict(modelFit, newdata = testSA, type = "raw")))

missClass(trainSA$chd, predictionTrain)
missClass(testSA$chd, predictionTest)

## Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
library(randomForest)

set.seed(33833)
vowel.train$y <- as.factor(vowel.train$y)
modelFit <- randomForest::randomForest(y~., data = vowel.train)
order(varImp(modelFit), decreasing = TRUE)

##The order of the variables is:
x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10.

