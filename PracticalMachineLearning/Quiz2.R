library(caret)

## Q1
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50, list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

## Q2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

library(Hmisc)

value <- cut2(testing, g=9)
qplot(testing$CompressiveStrength, , color = value)
hist(testing$Superplasticizer)
a <- preProcess(training,thresh = 0.80)

##Q3

## Q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
library(dplyr)
a <- select(training, starts_with("IL"))

preProcess(a, thresh = 0.80)
##12

## Q5

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training <- adData[ inTrain,]
testing <- adData[-inTrain,]

library(dplyr)

training <- select(training, c("diagnosis", starts_with("IL")))
testing <- select(testing, c("diagnosis", starts_with("IL")))

NoPCA <- train(training$diagnosis~., method = "glm", data = training)
PCA <- train(form=training$diagnosis~., method = "glm", preProcess = "pca", data = training)
