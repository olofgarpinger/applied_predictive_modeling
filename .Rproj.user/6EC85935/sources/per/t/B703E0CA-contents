library(tidyverse)
library(corrplot)
library(caret)

# 3.1 
library(mlbench)

data(Glass)
str(Glass)

Glass <- as.tibble(Glass)

Type <- Glass$Type
p <- Glass %>% 
  select(-Type) %>% 
  scale() %>% 
  as.tibble() %>% 
  mutate(Type = Type) %>% 
  gather(RI:Fe, key = "variable", value = "amount") %>% 
  ggplot(aes(amount))

p +
  geom_histogram(binwidth = 0.2) +
  facet_wrap(~ variable) 

p + 
  geom_freqpoly(aes(y = ..density.., col = Type), binwidth = 0.2) +
  facet_wrap(~ variable)

Glass %>% 
  ggplot(aes(Type)) +
  geom_bar()

plot(Glass)

corrplot(cor(Glass %>% select(-Type)), order = "hclust")

GGally::ggpairs(Glass)

p + 
  geom_boxplot(aes(x = variable, y = amount))

Glass %>% 
  select(-Type) %>%
  map_dbl(skewness)

yjTrans <- preProcess(Glass %>% select(-Type), method = c("center", "scale","YeoJohnson"))
yjData <- predict(yjTrans, newdata = Glass %>% select(-Type))
yjData %>% map_dbl(skewness)

pp <- yjData %>% 
  as.tibble() %>% 
  mutate(Type = Type) %>% 
  gather(RI:Fe, key = "variable", value = "amount") %>% 
  ggplot(aes(amount)) 

pp +
  geom_histogram(binwidth = 0.2) +
  facet_wrap(~ variable) 

pp + 
  geom_freqpoly(aes(y = ..density.., col = Type), binwidth = 0.2) +
  facet_wrap(~ variable)


# Outliers

zjTrans <- preProcess(Glass %>% select(-Type), method = c("center", "scale", "spatialSign"))
zjData <- predict(zjTrans, newdata = Glass %>% select(-Type)) %>% mutate(Type = Type)

GGally::ggpairs(zjData)

corrplot(cor(Glass %>% select(-Type)))
corrplot(cor(zjData %>% select(-Type)))

zjData %>% 
  gather(RI:Fe, key = "variable", value = "amount") %>% 
  ggplot(aes(amount)) +
  geom_freqpoly(aes(col = Type), binwidth = 0.1, lwd = 1) +
  facet_wrap( ~ variable)



# 3.2
library(mlbench)

data("Soybean")
str(Soybean)
Soybean <- as.tibble(Soybean)
Soybean

# a)
Soybean %>% 
  gather(date:roots, key = "variable", value = "category") %>% 
  ggplot(aes(category)) +
  geom_bar() +
  facet_wrap( ~ variable)

names(Soybean[nearZeroVar(Soybean)])

Soybean %>% 
  gather(date:roots, key = "variable", value = "category") %>% 
  filter(is.na(category)) %>% 
  group_by(variable) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

Soybean %>% 
  count(Class) %>% 
  arrange(desc(n)) %>% 
  left_join(Soybean %>% 
  na.omit() %>% 
  count(Class) %>% 
  arrange(desc(n)), by = "Class") %>% 
  mutate(n_diff = n.x - n.y)

Amelia::missmap(Soybean)

Soybean %>% filter(Class == "phytophthora-rot") %>% Amelia::missmap()
Soybean %>% filter(Class == "diaporthe-pod-&-stem-blight") %>% Amelia::missmap()
Soybean %>% filter(Class == "herbicide-injury") %>% Amelia::missmap()
Soybean %>% filter(Class == "cyst-nematode") %>% Amelia::missmap()
Soybean %>% filter(Class == "2-4-d-injury") %>% Amelia::missmap()

SoybeanSub <- Soybean %>% filter(Class != "diaporthe-pod-&-stem-blight",
                   Class != "herbicide-injury",
                   Class != "cyst-nematode",
                   Class != "2-4-d-injury") 

SoybeanSub %>% 
  Amelia::missmap()

SoybeanSub %>% 

# nzv => removes "leaf.mild" "mycelium"  "sclerotia" predictors

preProcess(Soybean, method = "nzv") 
nearZeroVar(Soybean)


# 3.3
library(caret)
data("BloodBrain")

bbbDescr <- as.tibble(bbbDescr)
logBBB %>% as.tibble() %>% 
  ggplot(aes(value)) + geom_histogram()

names(bbbDescr[nearZeroVar(bbbDescr)])

bbbDescr %>% 
  select(names(bbbDescr[nearZeroVar(bbbDescr)])) %>% 
  gather(negative:alert, key = "variable", value = "value") %>% 
  ggplot(aes(value)) + 
  geom_histogram() + 
  facet_wrap( ~ variable)

corrplot::corrplot(cor(bbbDescr), order = "hclust")


bbbDescrSub <- predict(preProcess(bbbDescr, method = c("nzv", "corr")), bbbDescr)

corrplot::corrplot(cor(bbbDescrSub), order = "hclust")

sort(map_dbl(bbbDescrSub, skewness))


