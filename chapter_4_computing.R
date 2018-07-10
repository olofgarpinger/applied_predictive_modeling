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










