library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
library(e1071)
library(lattice)
library(tidyverse)

data("segmentationOriginal")
segmentationOriginal <- as.tibble(segmentationOriginal)

segData <- segmentationOriginal %>% 
  filter(Case == "Train")

cellID <- segData$Cell
class <- segData$Class
case <- segData$Case
segData <- segData %>% select(-(Cell:Class))

statusColNum <- grep("Status", names(segData))
statusColNum
segData <- segData %>% select(-statusColNum)

skewness(segData$AngleCh1)
skewValues <- map_dbl(segData, skewness)
head(skewValues)

Ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1)
Ch1AreaTrans$lambda

head(segData$AreaCh1)
predict(Ch1AreaTrans, head(segData$AreaCh1))

pcaObject <- prcomp(segData,
                    center = TRUE, 
                    scale. = TRUE)
percentVariance <- pcaObject$sd^2/sum(pcaObject$sd^2)*100
percentVariance[1:3]

head(pcaObject$x[, 1:5])

head(pcaObject$rotation[, 1:3])

trans <- preProcess(as.data.frame(segData),
                    method = c("BoxCox", "center", "scale", "pca"))
trans

transformed <- predict(trans, as.data.frame(segData))
head(transformed)

nearZeroVar(segData)

correlations <- cor(segData)
dim(correlations)
correlations[1:4, 1:4]

corrplot(correlations, order = "hclust")

highCorr <- findCorrelation(correlations, cutoff = .75)
sort(highCorr)

filteredSegData <- segData %>% 
  select(-highCorr)
filteredSegData
corrplot(cor(filteredSegData), order = "hclust")

mpgSubset <- mpg %>% select(c(1,3)) %>% mutate(manufacturer = as.factor(manufacturer))
head(mpgSubset)
levels(mpgSubset$manufacturer)

simpleMod <- dummyVars( ~ displ + manufacturer,
                       data = mpgSubset,
                       levelsOnly = TRUE)
simpleMod

predict(simpleMod, (mpgSubset))

withInteraction <- dummyVars(~ displ + manufacturer + displ:manufacturer,
                             data = mpgSubset,
                             levelsOnly = TRUE)
withInteraction
predict(withInteraction, head(mpgSubset))


