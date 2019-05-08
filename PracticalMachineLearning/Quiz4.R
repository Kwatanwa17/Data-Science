## Q1
library(ElemStatLearn)
library(caret)

data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
fit_RF <- train(y~., method = "rf", data = vowel.train)
fit_Boosted <- train(y~., method = "gbm", data = vowel.train)

pred_RF <- predict(fit_RF, vowel.test)
acc_RF <- confusionMatrix(pred_RF, vowel.test$y)
acc_RF

pred_Boosted <- predict(fit_Boosted, vowel.test)
acc_Boosted <- confusionMatrix(pred_Boosted, vowel.test$y)
acc_Boosted

agree <- confusionMatrix(pred_RF, pred_Boosted)
agree

##Q2
library(caret)

library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

fitRF <- train(diagnosis~., method = "rf", data = training)
fitGBM <- train(diagnosis~., method = "gbm", data = training)
fitLDA <- train(diagnosis~., method = "lda", data = training)

predRF <- predict(fitRF, newdata = testing)
predGBM <- predict(fitGBM, newdata = testing)
predLDA <- predict(fitLDA, newdata = testing)

predDF <- data.frame(predRF, predGBM, predLDA, diagnosis = testing$diagnosis, stringsAsFactors = FALSE)

stack <- train(diagnosis~., data = predDF, method = "rf")

##Q3 

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)
predLasso <- train(CompressiveStrength~., method = "lasso", data = training)
plot(predLasso$finalModel)

## Q4

library(lubridate) # For year() function below

dat = read.csv("gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

library(forecast)
# 
# training$date <- as.Date(training$date, format = "%y-%m-%d")

fit <- bats(tstrain)
tstest <- ts(testing$visitsTumblr)


##Q5
set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)

fitSVM <- train(CompressiveStrength ~., method = "svmRadial", data = training)
pred <- predict(fitSVM, testing)

RMSE(pred, testing$CompressiveStrength)
