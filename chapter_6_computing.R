library(AppliedPredictiveModeling)
library(tidyverse)
library(caret)
library(MASS)
data(solubility)

trainingData <- as.tibble(solTrainXtrans) %>% 
  mutate(Solubility = solTrainY)

lmFitAllPredictors <- lm(Solubility ~ ., data = trainingData)
summary(lmFitAllPredictors)

lmPred1 <- predict(lmFitAllPredictors, solTestXtrans)

lmValues1 <- data.frame(obs = solTestY, pred = lmPred1)
defaultSummary(lmValues1)

rlmFitAllPredictors <- rlm(Solubility ~ ., data = trainingData)
summary(rlmFitAllPredictors)

ctrl <- trainControl(method = "cv", number = 10)

set.seed(100)
lmFit1 <- train(x = solTrainXtrans, y = solTrainY, method = "lm", trControl = ctrl)
lmFit1

xyplot(solTrainY ~ predict(lmFit1),
       type = c("p", "g"),
       xlab = "Predicted", ylab = "Observed")
xyplot(resid(lmFit1) ~ predict(lmFit1),
       type = c("p", "g"),
       xlab = "Predicted", ylab = "Residuals")

corThresh <- .9
tooHigh <- findCorrelation(cor(solTrainXtrans), corThresh)
corrPred <- names(solTrainXtrans)[tooHigh]
trainXfiltered <- solTrainXtrans[, -tooHigh]
testXfiltered <- solTestXtrans[, -tooHigh]
set.seed(100)
lmFiltered <- train(x = trainXfiltered, y = solTrainY, method = "lm", 
                    trControl = ctrl)
lmFiltered

set.seed(100)
rlmPCA <- train(solTrainXtrans, solTrainY, method = "rlm", preProcess = "pca", trControl = ctrl)
rlmPCA
