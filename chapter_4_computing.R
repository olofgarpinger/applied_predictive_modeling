library(AppliedPredictiveModeling)
library(tidyverse)
library(caret)

# Data splitting
data(twoClassData)
str(predictors)
str(classes)

set.seed(1)
trainingRows <- createDataPartition(classes, p = 0.8, list = FALSE)
head(trainingRows)

trainPredictors <- predictors %>% 
  slice(trainingRows)
trainPredictors

trainClasses <- classes[trainingRows]
summary(trainClasses)

testPredictors <- predictors %>% 
  slice(-trainingRows)
testPredictors

testClasses <- classes[-trainingRows]
summary(testClasses)


# Resampling
set.seed(1)
repeatedSplits <- createDataPartition(trainClasses, p = .80, 
                                      times = 3)
str(repeatedSplits)

set.seed(1)
cvSplits <- createFolds(trainClasses, k = 10,
                        returnTrain = T)
str(cvSplits)

fold1 <- cvSplits[[1]]
fold1

cvPredictors1 <- trainPredictors %>% slice(fold1)
cvClasses1 <- trainClasses[fold1]
nrow(trainPredictors)
nrow(cvPredictors1)


# Basic Model Building in R
trainPredictors <- as.matrix(trainPredictors)
knnFit <- knn3(x = trainPredictors, y = trainClasses, k = 5)
knnFit

testPredictions <- predict(knnFit, newdata = testPredictors,
                           type = "class")
head(testPredictions)
str(testPredictions)
table(testPredictions, testClasses)


# Determination of tuning parameters
library(caret)
data(GermanCredit)
dim(GermanCredit)

## First, remove near-zero variance predictors then get rid of a few predictors 
## that duplicate values. For example, there are two possible values for the 
## housing variable: "Rent", "Own" and "ForFree". So that we don't have linear
## dependencies, we get rid of one of the levels (e.g. "ForFree")

GermanCredit <- GermanCredit[, -nearZeroVar(GermanCredit)]
GermanCredit$CheckingAccountStatus.lt.0 <- NULL
GermanCredit$SavingsAccountBonds.lt.100 <- NULL
GermanCredit$EmploymentDuration.lt.1 <- NULL
GermanCredit$EmploymentDuration.Unemployed <- NULL
GermanCredit$Personal.Male.Married.Widowed <- NULL
GermanCredit$Property.Unknown <- NULL
GermanCredit$Housing.ForFree <- NULL

## Split the data into training (80%) and test sets (20%)
set.seed(100)
inTrain <- createDataPartition(GermanCredit$Class, p = .8)[[1]]
GermanCreditTrain <- GermanCredit[ inTrain, ]
GermanCreditTest  <- GermanCredit[-inTrain, ]

set.seed(1056)
svmFit <- train(Class ~ ., data = GermanCreditTrain,
                method = "svmRadial",
                preProc = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "repeatedcv",
                                         repeats = 5,
                                         classProbs = TRUE))
svmFit

plot(svmFit, scales = list(x = list(log = 2)))

predictedClasses <- predict(svmFit, GermanCreditTest)
str(predictedClasses)
predictedProbs <- predict(svmFit, newdata = GermanCreditTest, type = "prob")
head(predictedProbs)

# Between-model comparisons
set.seed(1056)
logisticReg <- train(Class ~ .,
                     data = GermanCreditTrain,
                     method = "glm",
                     trControl = trainControl(method = "repeatedcv",
                                              repeats = 5))
logisticReg

resamp <- resamples(list(SVM = svmFit, Logistic = logisticReg))
?resamples
summary(resamp)

modelDifferences <- diff(resamp)
summary(modelDifferences)



